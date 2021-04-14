{-# options_ghc -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Notes09 where

import Data.Char
import Data.List
import Data.Functor
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

-------------------------------------------------------------------------------

-- The `eof` (end of file) parser.
--  succeeds if the input string is empty, fails otherwise.
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- The parser `satisfy p` succeeds if the input string starts with a character 
--  that satisfies the predicate p (and consumes that character), and fails otherwise.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c       -> Just (c, cs)
       | otherwise -> Nothing
  [] -> Nothing

-------------------------------------------------------------------------------

-- The parser `char c` should succeed if the input string start with the character c.
-- Examples:
--   runParser (char 'a') "" == Nothing
--   runParser (char 'a') "abc" == Just ((), "bc")
--   runParser (char 'a') "bcd" == Nothing
char :: Char -> Parser ()
-- char c = do
--   satisfy (== c)
--   pure ()
char c = satisfy (== c) $> ()

-- The parser anyChar should succeed if the input string is not empty, and return its first character.
-- Examples:
--   runParser anyChar "" == Nothing
--   runParser anyChar "()" == Just ('(', ")")
--   runParser anyChar "abc" == Just ('a', "bc")
anyChar :: Parser Char
-- anyChar = satisfy (\_ -> True)
anyChar = satisfy (const True)

-- The parser `string s` should succeed if the input string starts with the string s.
--   runParser (string "abc") "abdef" == Nothing
--   runParser (string "") "abcdef" == Just ((), "abcdef")
--   runParser (string "abc") "abcdef" == Just ((), "def")
string :: String -> Parser ()
-- string []     = pure ()
-- string (c:cs) = do
--   char c
--   string cs

string cs = forM_ cs char

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
--  `many p` always succeeds.                           -- many p       p*
--  `some p` succeeds if the first run of p succeeded.  -- some p       p+

some' :: Alternative f => f a -> f [a]
many' :: Alternative f => f a -> f [a]
-- `some' p` uses p at least 1 times
--   => `some' p` uses p once, and then uses p again any number of times.
some' p = undefined
-- `many' p` uses p any number of times
--   => `many' p` uses p either (at least 1 times) or doesn't use p.
many' p = undefined

-- Examples:
--   runParser (some (char 'a')) "aaabbb" = Just ("aaa", "bbb")
--   runParser (some (char 'a')) "bbb" = Nothing
--   runParser (many (char 'a')) "aaabbb" = Just ("aaa", "bbb")
--   runParser (many (char 'a')) "bbb" = Just ("", "bbb")

--------------------------------------------------------------------------------

-- The parser digit should parse a single digit between 0 and 9.
digit :: Parser Integer
digit = fromIntegral.digitToInt <$> satisfy isDigit

-- The parser posInt should parse a positive integer
--   Example: runParser posInt "123" == Just (123, "")
--            runParser posInt "92476abc" == Just (92476, "abc")
posInt :: Parser Integer
posInt = undefined

-- The parser int should parse a positive or negative integer
--   Example: runParser int "123" == Just (123, "")
--            runParser int "92476abc" == Just (92476, "abc")
--            runParser int "-92476abc" == Just (-92476, "abc")
int :: Parser Integer
int = undefined

-- The parser `space` should parse a single whitespace character.
--  Hint: use `isSpace :: Char -> Bool`
space :: Parser ()
space = undefined

-- The parser `ws` should parse as many whitespace characters as possible.
ws :: Parser ()
ws = undefined

--------------------------------------------------------------------------------

-- `sepBy1 p sep` parses the regular expression p (sep p)*, 
--   i.e. any sequence of the form
--     p 
--     p sep p
--     p sep p sep p
--     ...
-- It fails if it cannot parse p at least once.  
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = undefined

-- sepBy p sep parses either any sequence of the form
--     p 
--     p sep p
--     ...
--   or returns the empty list.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = undefined

-- Write a parser that parses a list of integers (in the same format as a haskell list)
--  runParser haskellList "[1,2,3]" == (Just [1,2,3], "")
--  runParser haskellList "[]" == (Just [], "")
--  runParser haskellList "[12,,]" == Nothing
--  runParser haskellList "[1,2][]" == (Just [1,2], "[]")

haskellList :: Parser [Int]
haskellList = undefined

data Tree a = Leaf a 
            | Node [Tree a]
            deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- pIntTree should parse trees of integers.
-- Examples:
--   runParser pIntTree "0"              ~~>  Leaf 0
--   runParser pIntTree "[]"             ~~>  Node []
--   runParser pIntTree "[0,1]"          ~~>  Node [Leaf 0, Leaf 1]
--   runParser pIntTree "[[],[2]]"       ~~>  Node [Node [], Node [Leaf 2]]
--   runParser pIntTree "[0,[1],[[2]]]"  ~~>  Node [Leaf 0, Node [Leaf 1], Node [Node [Leaf 2]]]
pIntTree :: Parser (Tree Integer)
pIntTree = undefined

-------------------------------------------------------------------------------