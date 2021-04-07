{-# options_ghc -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Notes08 where

import Data.Char
import Data.List
import Control.Applicative
import Control.Monad 

--------------------------------------------------------------------------------
-- Define Foldable and Traversable instances for:

data Pair a b = Pair a b
              deriving (Show)

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

data Either' a b = Left' a
                 | Right' b
                 deriving (Show)
instance Functor (Either' a) where
  fmap f (Left' x)  = Left' x
  fmap f (Right' y) = Right' (f y)


data BinTree a = BLeaf a
               | BNode (BinTree a) (BinTree a)
               deriving (Show, Eq)
instance Functor BinTree where
  fmap f (BLeaf x)   = BLeaf (f x)
  fmap f (BNode l r) = BNode (fmap f l) (fmap f r)

data T1 a = Leaf1 a
          | Node1 [T1 a]
          deriving (Show)
instance Functor T1 where
  fmap f (Leaf1 x)  = Leaf1 (f x)
  fmap f (Node1 xs) = Node1 (fmap @[] (fmap @T1 f) xs)

data T3 a = Leaf3 a
          | Node3 (T1 [T3 a])
          deriving (Show)
instance Functor T3 where
  fmap f (Leaf3 x)  = Leaf3 (f x)
  fmap f (Node3 xs) = Node3 (fmap @T1 (fmap @[] (fmap @T3 f)) xs)

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

-- Define the folling parser combinators.
--  (You don't need to use `Parser $ \s -> ...`, they can be defined using satisfy
--   and the monad combinators.)

-- The parser `char c` should succeed if the input string start with the character c.
-- Examples:
--   runParser (char 'a') "" == Nothing
--   runParser (char 'a') "abc" == Just ((), "bc")
--   runParser (char 'a') "bcd" == Nothing
char :: Char -> Parser ()
char c = undefined

-- The parser anyChar should succeed if the input string is not empty, and return its first character.
-- Examples:
--   runParser anyChar "" == Nothing
--   runParser anyChar "()" == Just ('(', ")")
--   runParser anyChar "abc" == Just ('a', "bc")
anyChar :: Parser Char
anyChar = undefined

-- The parser `string s` should succeed if the input string starts with the string s.
--   runParser (string "abc") "abdef" == Nothing
--   runParser (string "") "abcdef" == Just ((), "abcdef")
--   runParser (string "abc") "abcdef" == Just ((), "def")
string :: String -> Parser ()
string s = undefined

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

-------------------------------------------------------------------------------

-- The parser digit should parse a single digit between 0 and 9.
digit :: Parser Integer
digit = undefined

-- The parser digit should parse a positive integer
posInt :: Parser Integer
posInt = undefined

-- The parser int should parse a positive or negative integer
int :: Parser Integer
int = undefined

-- The parser `space` should parse a single whitespace character.
--  Hint: use `isSpace :: Char -> Bool`
space :: Parser ()
space = undefined

-- The parser `ws` should parse as many whitespace characters as possible.
ws :: Parser ()
ws = undefined

-------------------------------------------------------------------------------