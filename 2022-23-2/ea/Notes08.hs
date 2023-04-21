{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Data.Foldable
import Data.Traversable
import Control.Applicative
import Control.Monad.State
import Control.Monad

-- Traversable, Foldable  (+generikus példák)
-- Parser
-- (GHC runtime)

-- class Foldable t where
--    toList :: t a -> [a]
--    foldr :: (a -> b -> b) -> b -> t a -> b

-- Sok metódus azért van Foldable-ben,
-- hogy lehessen hatékony specializált definíciót megadni
-- DeriveFoldable

data Tree a = Leaf a | Node (Tree a) (Tree a)
   deriving (Eq, Show, Functor, Foldable)

-- toList = foldr (:) []
-- class Traversable

-- mapM és mapM_ függvény túlterhelve

-- class (Functor t, Foldable t) => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- traverse_' :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
-- traverse_' f ta = foldr (\a funit -> f a *> funit) (pure ()) ta

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf a)   = Leaf <$> f a
  traverse f (Node l r) = Node <$> traverse f l <*> traverse f r

  -- f :: (a -> b -> c -> ... z)
  -- arg1 :: f a
  -- arg2 :: f b
  -- ....
  -- f <$> arg1 <*> arg2 <*> ... <*> argN  :: f z

  -- fmap f arg1   ==     f <$> arg     ((<$>) fmap operátor verziója)

label :: (Traversable t) => t a -> t (a, Int)
label t = evalState (traverse f t) 0 where
  f a = do
    n <- get
    put (n + 1)
    pure (a, n)

newtype Reverse t a = Reverse (t a) deriving Functor

-- Opcionális házi: írd meg az alábbi instance-ot úgy, hogy
-- a foldr és a traverse is fordított sorrendben dolgozza fel
-- az elemeket.
instance (Foldable t) => Foldable (Reverse t) where
  foldr = undefined

instance (Traversable t) => Traversable (Reverse t) where
  traverse = undefined

-- opcionális (hasonló) feladat:

-- fordítsuk meg az elemek sorrendjét bármilyen Traversable
-- struktúrában. Hasonlóképpen a "labelTraversable"-hez
reverseTraversable :: Traversable t => t a -> t a
reverseTraversable = undefined

-- tipp: State-et használva
-- egyszer bejárjuk, kigyűjtjük az értékeket listába
-- másodszor bejárjuk: visszatöltjük a listából fordítva

-- \x -> [x, x+ 1] :: Int -> [Int]
-- traverse (\x -> [x, x+1]) (Node (Leaf 0) (Leaf 1))
--   Tree [a] -> [Tree a]

--   [Tree a] -> Tree [a]

instance Applicative Tree where
  pure = Leaf
  (<*>) = ap

instance Monad Tree where
  return :: a -> Tree a
  return = pure            -- Leaf :: a -> Tree a

  (>>=) :: Tree a -> (a -> Tree b) -> Tree b
  (>>=) (Leaf a)   f = f a
  (>>=) (Node l r) f = Node (l >>= f) (r >>= f)

  -- (megfelel a monád törvényeknek)

-- >>=: helyettesítsünk be minden levél helyére új fákat
t1 :: Tree Int
t1 = Node (Leaf 0) (Leaf 1) >>= \n -> Node (Leaf n) (Leaf (n + 1))

foo :: [Tree a] -> Tree [a]
foo = traverse id

-- foo [Leaf 0, Leaf 1]

-- Parser
--------------------------------------------------------------------------------

-- parsing: token sorozotaból csináljuk fát (bármilyen strukturált adatot)

-- input               (strukturált adat , maradék input)
-- String   ->   Maybe (a                , String       )

-- Parser a ~ (Maybe + State String)

-- utána lehet nézni: Monad transformer
--                    több monádot össze lehet kombinálni
--                    az összes rész-mellékhatás elérhető
--   import Control.Monad.Trans
--   type Parser a = StateT String Maybe a

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

   -- fmap f (Parser g) = Parser $ \s -> case g s of
   --    Nothing     -> Nothing
   --    Just (a, s) -> Just (f a, s)

instance Applicative Parser where
  pure  a = Parser $ \s -> Just (a, s)
  (<*>) = ap

instance Monad Parser where

  -- return: nincs mellékhatás
  return :: a -> Parser a     -- return a :: String -> Maybe (a, String)
  return = pure

  -- egymást után két parser-t végrehajtunk
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (Parser f) g = Parser $ \s -> case f s of
    Nothing     -> Nothing -- első parser hibázik: hiba az eredmény
    Just (a, s) -> runParser (g a) s -- első parser sikeres:
                                     -- meghívjuk a másodikat

-- magasabb szintű API

-- parser, ami üres inputot fogadja csak el
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- sikeres: az input első karakterére igaz egy feltétel,
-- kiolvassuk és visszaadjuk
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- konkrét karaktert olvasunk
char :: Char -> Parser ()
char c = () <$ satisfy (==c)   -- (<$) a fa = fmap (\_ -> a) fa

instance Alternative Parser where
  -- rögtön hibázó parser
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- pa <|> pb     : ha pa hibázik, futtassul pb-t
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- konkrét String-et olvassunk
string :: String -> Parser ()
string s = mapM_ char s

--    Parser             regex
-- pa <|> pb             p1|p2
-- pa >> pb              p1 p2
--   eof                 epsilon
--  many p               p*

--   futtassuk "p"-t annyiszor, ahányszor lehet,
--   az összes eredmény adjuk vissza listában
-- many :: Parser a -> Parser [a]

-- p1 = ......
-- p2 =    p1 <|> ... <|> many p1 <|> ...
