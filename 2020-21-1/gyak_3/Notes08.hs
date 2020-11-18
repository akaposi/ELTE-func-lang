{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MonadComprehensions #-}
module Notes08 where

import Data.Char
import Data.List
import Control.Applicative
import Control.Monad 

-------------------------------------------------------------------------------

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case p s of
    Just (x, s') -> Just (f x, s')
    Nothing      -> Nothing

instance Applicative Parser where pure = return; (<*>) = ap

instance Monad Parser where
  return x = Parser $ \s -> Just (x, s)
  Parser p >>= f = Parser $ \s -> case p s of
    Just (x, s') -> runParser (f x) s'
    Nothing      -> Nothing

putP :: String -> Parser ()
putP s = Parser $ \_ -> Just ((), s)

getP :: Parser String
getP = Parser $ \s -> Just (s, s)

empty' :: Parser a
empty' = Parser $ \_ -> Nothing

guard' :: Bool -> Parser ()
guard' True  = pure ()
guard' False = empty'

-------------------------------------------------------------------------------

-- The `eof` (end of file) parser.
--  succeeds if the input string is empty, fails otherwise.
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

eof' :: Parser ()
eof' = do
  input <- getP
  guard' (null input)

-- The parser `satisfy p` succeeds if the input string starts with a character 
--  that satisfies the predicate p (and consumes that character), and fails otherwise.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  c:cs | p c       -> Just (c, cs)
       | otherwise -> Nothing
  [] -> Nothing

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' p = do
  input <- getP
  case input of
    c:cs | p c -> do
      putP cs
      pure c
    _ -> empty

-------------------------------------------------------------------------------

-- The parser `char c` should succeed if the input string start with the character c.
-- Examples:
--   runParser (char 'a') "" == Nothing
--   runParser (char 'a') "abc" == Just ((), "bc")
--   runParser (char 'a') "bcd" == Nothing
char :: Char -> Parser ()
char c = satisfy (== c) *> pure ()

-- The parser anyChar should succeed if the input string is not empty, and return its first character.
-- Examples:
--   runParser anyChar "" == Nothing
--   runParser anyChar "()" == Just ('(', ")")
--   runParser anyChar "abc" == Just ('a', "bc")
anyChar :: Parser Char
anyChar = satisfy (const True)

-- The parser `string s` should succeed if the input string starts with the string s.
--   runParser (string "abc") "abdef" == Nothing
--   runParser (string "") "abcdef" == Just ((), "abcdef")
--   runParser (string "abc") "abcdef" == Just ((), "def")
string :: String -> Parser ()
string s = forM_ s char

-------------------------------------------------------------------------------

-- class Applicative f => Alternative f where
--   empty :: f a 
--   (<|>) :: f a -> f a -> f a

instance Alternative Parser where
  -- The parser `empty` always fails.
  empty = Parser $ \_ -> Nothing

  -- The parser `p1 <|> p2` tries the parser p1. 
  --  If p1 succeeds, it returns the result of p1.
  --  If p1 fails, then it tries the parser p2 instead.
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-------------------------------------------------------------------------------

-- The parser `some p` and `many p` both try to use the parser p as many times as possible.
--  `many p` always succeeds.
--  `some p` succeeds if the first run of p succeeded.

some' :: Parser a -> Parser [a]
many' :: Parser a -> Parser [a]
-- `some' p` uses p at least 1 times
--   => `some' p` uses p once, and then uses p again any number of times.
some' p = do
  x  <- p
  xs <- many' p
  pure $ (x : xs)
--  (:) <$> p <*> many p

-- `many' p` uses p any number of times
--   => `many' p` uses p either (at least 1 times) or doesn't use p.
many' p = some p
      <|> pure []

--    many p     /=     pure [] <|> some p      

-- Examples:
--   runParser (some (char 'a')) "aaabbb" = Just ("aaa", "bbb")
--   runParser (some (char 'a')) "bbb" = Nothing
--   runParser (many (char 'a')) "aaabbb" = Just ("aaa", "bbb")
--   runParser (many (char 'a')) "bbb" = Just ("", "bbb")

-------------------------------------------------------------------------------

-- The parser digit should parse a digit between 0 and 9.
digit :: Parser Integer
-- digit = fromIntegral . digitToInt <$> satisfy isDigit
digit = do
  c <- satisfy isDigit
  pure (fromIntegral (digitToInt c))
-- <$> : same as fmap

-- The parser digit should parse a positive integer
posInt :: Parser Integer
posInt = foldl' (\x y -> 10*x+y) 0 <$> some digit

-- The parser int should parse a positive or negative integer
int :: Parser Integer
int = negInt <|> posInt
  where negInt = char '-' *> (negate <$> posInt)

-- The parser `space` should parse a single whitespace character.
--  Hint: use `isSpace :: Char -> Bool`
space :: Parser ()
space = satisfy isSpace *> pure ()

-- The parser `ws` should parse as many whitespace characters as possible.
ws :: Parser ()
ws = many space *> pure ()

--------------------------------------------------------------------------------
-- Parsing a simple configuration file.
-- A configuration file is a list of lines "key = value" where key is an 
--  identifier and value is an integer.

pLine :: Parser (String, Integer)
pLine = do
  ws

  -- k <- some isAlpha 

  k <- some (satisfy isAlphaNum)
  guard (isAlpha (head k)) 
  -- k is any non-empty string of alphanumeric characters, starting from an alphabetic character.
  ws
  char '='
  ws
  v <- int
  pure (k, v)

pFile :: Parser [(String, Integer)]
pFile = do
  kvs <- many pLine
  ws
  eof
  pure kvs

test1, test2, test3, test4 :: String
test1 = ""
test2 = "key = 10"
test3 = "key1 = 10 \nkey2 = 100"
test4 = "  key   =   10   \n   key2  =   0   \n   key3  =  -10 "

--------------------------------------------------------------------------------
