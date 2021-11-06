{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

--------------------------------------------------------------------------------

import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Applicative
import Data.Char 

import Debug.Trace


newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
  deriving(Functor)

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return :: a -> Parser a
  return a = Parser $ \s -> Just (a, s)

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing      -> Nothing
    Just (a, s') -> runParser (g a) s'

instance Alternative Parser where
  -- The empty parser always fails.
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- The parser (p1 <|> p2) first tries to run the parser p1. 
  --   If p1 fails, the parser p2 is used instead.
  (<|>) :: Parser a -> Parser a -> Parser a
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- The parser `satisfy p` accepts any character that satisfies the predicate p.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s -> trace (msg ++ " : " ++ s) (runParser pa s)

-- The parser `char c` recognizes exactly the character c.
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- The parser anyChar accepts any character.
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

-- The parser `string s` recognizes exactly the string s.
string :: String -> Parser ()
string = traverse_ char

-- In Control.Applicative:

-- many :: Parser a -> Parser [a]
--   `many p` tries to run the parser p as many times as possible.

-- some :: Parser a -> Parser [a]
--   `some p` tries to run the parser p as many times as possible.
--   The parser p should suceed at least once.

many' :: Parser a -> Parser [a]
many' p = some' p <|> pure []

some' :: Parser a -> Parser [a]
some' p = do x <- p; xs <- many' p; pure (x:xs)

-- `sepBy pa psep` runs the parser pa as many times as possible, 
--   separated by the parser psep.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- `sepBy pa psep` runs the parser pa as many times as possible, 
--   separated by the parser psep.
--   The parser pa should suceed at least once.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

-------------------------------------------------------------------------------

-- The parser `digit` should parse a single digit between 0 and 9.
--  Ex: runParser digit "10x" == Just (1, "0x")
-- You can use:
--  isDigit :: Char -> Bool
--  digitToInt :: Char -> Int
digit :: Parser Int
-- digit = let p = satisfy isDigit
--         in fmap digitToInt p
digit = digitToInt <$> satisfy isDigit

-- The parser `posInt` should parse a positive integer.
--  Ex: runParser posInt "10" == Just (10, "")
--  Ex: runParser posInt "-10" == Nothing
posInt :: Parser Int
posInt = fmap (foldl' (\acc d -> acc*10+d) 0) (some digit)

-- The parser `negInt` should parse a negative integer (starting with a minus sign).
--  Ex: runParser negInt "-10" == Just (-10, "")
negInt :: Parser Int
negInt = do
  char '-'
  negate <$> posInt

-- The parser `int` should parse any integer.
int :: Parser Int
int = posInt <|> negInt
-- int = choice [posInt, negInt]

-- The parser `intList` should parse a list of integers, 
--   separated by commas and enclosed in square brackets.
--  Ex: runParser intList "[]" == Just ([], "")
--  Ex: runParser intList "[0]" == Just ([0], "")
--  Ex: runParser intList "[11,90]" == Just ([11,90], "")
--  Ex: runParser intList "[-7]" == Just ([-7], "")
--  Ex: runParser intList "[;]" == Nothing

brackets :: Parser a -> Parser a
-- brackets p = do
--   char '['
--   x <- p
--   char ']'
--   return x
brackets p = char '[' *> p <* char ']'

intList :: Parser [Int]
intList = brackets $ sepBy int (char ',')

data Tree a = Leaf a 
            | Node [Tree a]
            deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- The parser `intTree` should parse trees of integers.
-- Examples:
--   runParser intTree "0"              ~~>  Leaf 0
--   runParser intTree "[]"             ~~>  Node []
--   runParser intTree "[0,1]"          ~~>  Node [Leaf 0, Leaf 1]
--   runParser intTree "[[],[2]]"       ~~>  Node [Node [], Node [Leaf 2]]
--   runParser intTree "[0,[1],[[2]]]"  ~~>  Node [Leaf 0, Node [Leaf 1], Node [Node [Leaf 2]]]
intTree :: Parser (Tree Int)
intTree = undefined

-- Define variants `intList'` and `intTree'` of `intList` and `intTree` that also handle whitespace.

-- The parser `ws` removes whitespaces from the beginning of the input string.
ws :: Parser ()
ws = () <$ many (char ' ')

intList' :: Parser [Int]
intList' = undefined

intTree' :: Parser (Tree Int)
intTree' = undefined