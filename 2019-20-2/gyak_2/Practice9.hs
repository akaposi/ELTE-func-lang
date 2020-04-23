{-# LANGUAGE InstanceSigs #-}
module Practice9 where

import Control.Monad (ap, forM)
import Data.Char

-- Parser = Maybe + State
-- String -> (Maybe <data type>, String)
-- String -> Maybe (<data type>, String)

newtype Parser a = P { runParser :: String -> Maybe (a, String) }

char :: Char -> Parser Char
char x = satisfy (==x)

{- tests
runParser (char 'c') "c"    == Just ('c', "")
runParser (char 'c') "casd" == Just ('c', "asd")
runParser (char 'c') "Xasd" == Nothing
-}

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = P $ \str ->
  case str of
    c:cs | pred c -> Just (c, cs)
    _             -> Nothing

{- tests
runParser (satisfy isLower) "c"    == Just ('c', "")
runParser (satisfy isLower) "casd" == Just ('c', "asd")
runParser (satisfy isLower) "Xasd" == Nothing
-}

-- NOTE: use the P data ctor
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P $ \str ->
    case runParser p str of
      Just (x, str') -> Just (f x, str')
      _ -> Nothing

-- instance Monad m => Applicative m where
--   pure :: a -> m a
--   pure = return

--   (<*>) :: m (a -> b) -> m a -> m b
--   (<*>) mF mX = do
--     f <- mF
--     fmap f mX
--   -- (<*>) mF mX = mF >>= (\f -> fmap f mX)

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return :: a -> Parser a
  return x = P $ \str -> Just (x, str)

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) p k = P $ \str ->
    case runParser p str of
      Just (x, str') -> runParser (k x) str'
      Nothing        -> Nothing

-- char :: Char -> Parser Char
-- map :: (a -> b) -> [a] -> [b]
-- map char :: [Char] -> [Parser Char]
-- "abc" -> [char 'a', char 'b', char 'c']
-- sequenceParser :: [Parser a] -> Parser [a]  // sequenceA

sequenceParser :: [Parser a] -> Parser [a]
sequenceParser (p:ps) = do
  x  <- p
  xs <- sequenceParser ps
  return (x:xs)
sequenceParser [] = return []

-- string :: [Char] -> Parser [Char]
string :: String -> Parser String
string str = sequenceParser $ map char str

{- tests
runParser (string "abc") "abc"    == Just ("abc", "")
runParser (string "abc") "abcXYZ" == Just ("abc", "XYZ")
runParser (string "abc") "XbcXYZ" == Nothing
runParser (string "abc") "aXcXYZ" == Nothing
-}

-- NOTE: use the P data ctor
combine :: Parser a -> Parser a -> Parser a
combine p q = P $ \str ->
  case runParser p str of
    Nothing -> runParser q str
    x -> x

-- NOTE: DO NOT use the P data ctor, just satisfy + monad instance (do notation)
-- use: digitToInt + fmap
digit :: Parser Int
digit = fmap digitToInt $ satisfy (`elem` ['0'..'9'])

-- NOTE: DO NOT use the P data ctor, just satisfy + monad instance (do notation)
digitCoordinate :: Parser (Int, Int)
digitCoordinate = (,) <$> digit <*> digit

digitVec3 :: Parser (Int, Int, Int)
-- (digit, digit, digit)
-- (,,) digit digit digit
-- fmap (,,) digit                     :: Parser (b -> c -> (Int, b, c))
-- fmap (,,) digit <*> digit           :: Parser (c -> (Int, Int, c))
-- fmap (,,) digit <*> digit <*> digit :: Parser (Int, Int, Int)
-- (,,) <$> digit <*> digit <*> digit  :: Parser (Int, Int, Int)
digitVec3 = (,,) <$> digit <*> digit <*> digit

-- NOTE: DO NOT use the P data ctor, just satisfy + monad instance (do notation)
-- TODO: recognize n digits
digitN :: Int -> Parser [Int]
digitN n = forM [1..n] $ \_ -> digit

digits :: Parser [Int]
digits = do
  n <- digit
  digitN n

class Applicative f => Alternative f where
  (<|>) :: f a -> f a -> f a
  empty :: f a

-- forall p. p <|> empty === p
-- forall p. empty <|> p === p

instance Alternative Parser where
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) = combine

  empty :: Parser a
  empty = P $ \str -> Nothing

true :: Parser Bool
true = undefined

false :: Parser Bool
false = undefined

bool :: Parser Bool
bool = undefined

{- tests
runParser true "true"    == Just (True, "")
runParser true "trueXXX" == Just (True, "XXX")
runParser true "tRue"    == Nothing

runParser false "false"    == Just (False, "")
runParser false "falseXXX" == Just (False, "XXX")
runParser false "faLse"    == Nothing

runParser bool "true"  == Just (True, "")
runParser bool "false" == Just (False, "")
-}
