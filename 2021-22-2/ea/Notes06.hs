{-# language InstanceSigs, DeriveFunctor, DeriveFoldable,
             DeriveTraversable #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Monad -- ap :: Monad m => m (a -> b) -> m a -> m b
import Control.Applicative  -- class Alternative

-- Traversable, Parser
------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))
  (>>=) (State f) g = State $ \s -> case f s of
    (a, s) -> runState (g a) s

put :: s -> State s ()
put s = State $ \_ -> ((), s)

get :: State s s
get = State $ \s -> (s, s)

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState sta s = fst (runState sta s)

execState :: State s a -> s -> s
execState sta s = snd (runState sta s)


-- Traversable
------------------------------------------------------------

-- mapA   :: Applicative f => (a -> f b) -> [a] -> f [b]
-- mapA_  :: Applicative f => (a -> f b) -> [a] -> f ()

-- mapA-t túlterheljük más struktúrákra

-- class (Functor t, Foldable t) => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- instance Traversable [] where
--   traverse = mapA

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show, Foldable)

-- deriving (Functor, Foldable, Traversable)
--   {-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

instance Functor Tree where
  fmap f (Leaf a)   = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf a)   = Leaf <$> f a
  traverse f (Node l r) = Node <$> traverse f l <*> traverse f r

label :: Tree a -> Tree (Int, a)
label t = evalState (traverse go t) 0 where
  go :: a -> State Int (Int, a)
  go a = do
    n <- get
    put $ n + 1
    pure (n, a)

-- opcionális házi:
-- foldr-t definiáljuk traverse-el
--      (valamilyen Applicative-ot kell definiálni, amiből a foldr visszakapjuk)
--      egyszerű verzió, de nem a leghatékonyabb: (State [a])-ban gyűjtjük az elemeket,
--          a listára a végén meghívjuk a foldr-t
-- fmap-t  definiáljuk traverse-el
--      newtype Id a = Id a        instance Applicative Id where ...


-- Parser monád
----------------------------------------------------------------------

-- parser generátor vs. manuális parser
--   manuális (rekurzív) parser: (gcc, g++, clang, rust)

-- parser monád : deklaratív szintaxis, teljes kontroll a control flow fölött
--                ("PEG" nyelvtan (parsing expression grammar))
--                  többnyire PEG nyelvtan-okat fogunk parsolni


-- String -> Maybe a

-- State String + Maybe mellékhatások kombinációja.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving (Functor)

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return :: a -> Parser a
  return a = Parser $ \s -> Just (a, s)   -- nincs hatás

  -- egymás után futtatás
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing     -> Nothing
    Just (a, s) -> runParser (g a) s
      -- g a               :: Parser b
      -- runParser (g a)   :: String -> Maybe (b, String)
      -- runParser (g a) s :: Maybe (b, String)

-- primitív parserek, "kombinátor"
--   "parser combinator library"

-- pontosan az üres inputot olvassuk
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- olvassunk egy karaktert az input elejéről, amire igaz egy feltétel
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- ignoreResult :: Functor f => f a -> f ()      -- standard: void
-- ignoreResult fa = fmap (\_ -> ()) fa

-- ignoreResult fa = () <$ fa

-- kicseréljük a visszatérési értéket egy adott értékre
-- (<$) :: Functor f => a -> f b -> f a

-- olvassunk egy konkrét karaktert
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- olvassunk egy konkrét String-et
string :: String -> Parser ()   -- String ~ [Char]
string s = mapM_ char s         -- egymás után olvasom az összes Char-t a String-ben


-- "választás" parserek között

-- class Alternative f where
--    empty :: f a                      -- "hiba" érték
--    (<|>) :: f a -> f a -> f a        -- asszociatív bináris operátor, egységeleme "empty"

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- ha a bal parser hibázik, jobb parser-t futtatja
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing -> g s
    res     -> res

  -- empty <|> p = p
  -- p <|> empty = p

-- runParser (string "kutya" <|> string "majom") "majomx" == Just ("majom", "x")

-- iteráció:
--    many  :: Parser a -> Parser [a]       -- 0-szor vagy többször futtatja
--    some  :: Parser a -> Parser [a]       -- 0-szer vagy többször futtatja
--    many_ :: Parser a -> Parser ()        -- 0-szor vagy többször futtatja
--    some_ :: Parser a -> Parser ()        -- 0-szer vagy többször futtatja

-- many, some: Control.Applicative-ból importálhatók

-- standard definíció:
many' :: Parser a -> Parser [a]
many' pa = some' pa <|> pure []

some' :: Parser a -> Parser [a]
some' pa = (:) <$> pa <*> many' pa

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

-- Control.Applicative-ból:
-- optional :: Parser a -> Parser (Maybe a)   -- hibát értékként visszaadja (soha nem hibázik)
-- optional pa = (Just <$> pa) <|> pure Nothing

optional_ :: Parser a -> Parser ()
optional_ pa = () <$ optional pa

-- reguláris kifejezések:
--   ("validáló" parser, Parser ())

--   $         eof
--   e₁e₂      e₁ >> e₂
--   e₁|e₁     e₁ <|> e₂
--   c         char c
--   e*        many_ e
--   e+        some_ e
--   e?        optional_ e

-- tetszőleges rekurzív Parser a definíciót
--    (tetszőleges (Turing-teljes) parser-t írhatunk)

-- példa parser-re, ami *nem* olvasható regex-el:

-- olvasunk n darab 'a'-t, utána n darab 'b'-t
p1 :: Parser ()
p1 = do
  as <- many (char 'a')              -- végtelen sok esetet kéne regex-el kezelni!
  replicateM_ (length as) (char 'b')

pBool :: Parser Bool
pBool = (True  <$ string "True")
    <|> (False <$ string "False")

pPair :: Parser a -> Parser b -> Parser (a, b)
pPair pa pb = do
  char '('
  a <- pa
  char ','
  b <- pb
  char ')'
  pure (a, b)

pPair' :: Parser a -> Parser b -> Parser (a, b)
pPair' pa pb =
    (,) <$> (char '(' *> pa)
        <*> (char ',' *> pb <* char ')')

-- köv:
--    (<$>), (<$), (<*>), (<*), (*>)
