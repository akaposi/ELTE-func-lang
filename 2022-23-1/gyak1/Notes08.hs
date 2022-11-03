

{-# language InstanceSigs, DeriveFunctor #-}

import Control.Monad
import Control.Applicative
import Data.Char  -- isDigit, isAlpha, digitToInt


-- State monád
------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

instance Applicative (State s) where
  pure  a = State (\s -> (a, s))
  (<*>) = ap

instance Monad (State s) where
  return = pure
  State f >>= g = State (\s -> case f s of (a, s') -> runState (g a) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma


--------------------------------------------------------------------------------

-- bónusz feladat: fordítsd meg a tárolt értékek sorrendjét egy tetszőleges
-- Traversable struktúrában!
-- (Traversable: lásd előadás. Röviden: a "traverse" függvényt használhatod
--  a megoldáshoz, ami a mapM általánosítása különböző struktrákra).
reverseElems' :: Traversable t => t a -> t a
reverseElems' = undefined


-- Foldable, Traversable
--------------------------------------------------------------------------------

-- Definiáld a következő instance-okat:

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Functor)

data Tree2 a = Leaf1 a | Leaf2 a a | Node2 (Tree2 a) (Tree2 a)
             | Node3 (Tree2 a) (Tree2 a) (Tree2 a) deriving (Show, Functor)

data Foo3 a = Foo3 a a a a a deriving (Show, Functor)

data Pair a b = Pair a b deriving (Show, Functor)


instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr = undefined

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse = undefined

instance Foldable Tree2 where
  foldr :: (a -> b -> b) -> b -> Tree2 a -> b
  foldr = undefined

instance Traversable Tree2 where
  traverse :: Applicative f => (a -> f b) -> Tree2 a -> f (Tree2 b)
  traverse = undefined

instance Foldable (Pair c) where
  foldr = undefined

instance Traversable (Pair c) where
  traverse = undefined


-- Definiáld a következő függvényeket úgy, hogy csak foldr-t használj!

isEmpty :: Foldable t => t a -> Bool
isEmpty = undefined

length' :: Foldable t => t a -> Int
length' = undefined

sum' :: (Foldable t, Num a) => t a -> a
sum' = undefined

toList' :: Foldable t => t a -> a
toList' = undefined

-- első elemet add vissza, ha van
safeHead :: Foldable t => t a -> Maybe a
safeHead = undefined

-- utolsó elemet add vissza, ha van
safeLast :: Foldable t => t a -> Maybe a
safeLast = undefined

-- bónusz feladat: definiáld a foldl-t foldr *egyszeri* felhasználásával,
-- rekurzió nélkül.
foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl' = undefined


-- Parser
--------------------------------------------------------------------------------


newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) = ap

instance Monad Parser where
  return = pure
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing     -> Nothing
    Just (a, s) -> runParser (g a) s

eof :: Parser ()
eof = Parser $ \s -> case s of
  "" -> Just ((), "")
  _  -> Nothing

-- egy karaktert olvassunk az input elejéről, amire
-- igaz egy feltétel
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- olvassunk egy konkrét karaktert
char :: Char -> Parser ()
char c = () <$ satisfy (==c)
  -- satisfy (==c)   hiba: Parser Char helyett Parser () kéne

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- konkrét String olvasása:
string :: String -> Parser ()
string = mapM_ char -- minden karakterre alkalmazzuk a char-t

-- standard függvények (Control.Applicative-ból)
-- many :: Parser a -> Parser [a]
--    (0-szor vagy többször futtatunk egy parser-t)
-- some :: Parser a -> Parser [a]
--    (1-szor vagy többször futtatunk egy parser-t)

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

pDigit :: Parser Int
pDigit = digitToInt <$> satisfy isDigit

-- pozitív Int olvasása
pPos :: Parser Int
pPos = do
  ds <- some pDigit
  pure $ sum $ zipWith (*) (reverse ds) (iterate (*10) 1)


--------------------------------------------------------------------------------

-- Implementáld a következő regex-eket Parser-ként! Szükség szerint definiálj
-- segédfüggvényeket.

-- (foo|bar)*kutya
-- példák:   kutya fookutya barfookutya
p1 :: Parser ()
p1 = undefined

-- \[foo(, foo)*\]     (nemüres ,-vel választott "foo" lista)
-- példák:   [foo] [foo, foo] [foo, foo, foo]
p2 :: Parser ()
p2 = undefined

-- (ac|bd)*
-- példák:   acac  acbdbd
p3 :: Parser ()
p3 = undefined

-- [a..z]+@foobar\.(com|org|hu)
-- példák:   kutya@foobar.org  macska@foobar.org
p4 :: Parser ()
p4 = undefined

-- -?[0..9]+
-- (?e azt jelenti, hogy e opcionális)
p5 :: Parser ()
p5 = undefined

-- ([a..z]|[A..Z])([a..z]|[A..Z]|[0..9])*
p6 :: Parser ()
p6 = undefined

-- ([a..z]+=[0..9]+)(,[a..z]+=[0..9]+)*
-- példák:  foo=10,bar=30,baz=40
p7 :: Parser ()
p7 = undefined
