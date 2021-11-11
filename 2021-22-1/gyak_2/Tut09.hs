{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

--------------------------------------------------------------------------------

import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Applicative
import Data.Char 

import Debug.Trace

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
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

alpha :: Parser Char
alpha = satisfy (\c -> 'a' <= c && c <= 'z')

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

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

posInt :: Parser Int
posInt = fmap (foldl' (\x d -> 10*x+d) 0) (some digit)

negInt :: Parser Int
negInt = char '-' *> (negate <$> posInt)

int :: Parser Int
int = posInt <|> negInt

-- The parser `intList` should parse a list of integers, 
--   separated by commas and enclosed in square brackets.
intList :: Parser [Int]
intList = do
  char '['
  xs <- sepBy int
              (char ',')
  char ']'
  return xs
-- intList = char '[' *> sepBy int (char ',') <* char ']'

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

-- Define a parser for integer expressions built from constants and additions.
-- (+) should be left associative:.

data Expr1 = Value1 Int
           | Add1 Expr1 Expr1
           deriving(Eq, Show)

-- Either values or parentheses
pAtom1 :: Parser Expr1
pAtom1 = undefined 

-- Arbitrary expressions
--  Built out of + and atomic expressions.
pExpr1 :: Parser Expr1
pExpr1 = undefined 

tests1 :: [Bool]
tests1 = [ runParser pExpr1 "1+2+3"
           == Just ((Value1 1 `Add1` Value1 2) `Add1` Value1 3, "")
         , runParser pExpr1 "1+(2+3)"
           == Just (Value1 1 `Add1` (Value1 2 `Add1` Value1 3), "")
         ]

-- Define a parser for integer expressions built from constants, additions and multiplications.
data Expr2 = Value2 Int
           | Add2 Expr2 Expr2
           | Mul2 Expr2 Expr2
           deriving(Eq, Show)

-- Either values or parentheses
pAtom2 :: Parser Expr2
pAtom2 = undefined 

--  Built out of * and atomic expressions.
pExpr2_mul :: Parser Expr2
pExpr2_mul = undefined

--  Built out of + and of the results of `pExpr2_mul`.
pExpr2 :: Parser Expr2
pExpr2 = undefined 

tests2 :: [Bool]
tests2 = [ runParser pExpr2 "1+2+3"
           == Just ((Value2 1 `Add2` Value2 2) `Add2` Value2 3, "")
         , runParser pExpr2 "1+(2+3)"
           == Just (Value2 1 `Add2` (Value2 2 `Add2` Value2 3), "")
         , runParser pExpr2 "1*2+3"
           == Just ((Value2 1 `Mul2` Value2 2) `Add2` Value2 3, "")
         , runParser pExpr2 "1+2*3"
           == Just (Value2 1 `Add2` (Value2 2 `Mul2` Value2 3), "")
         ]

-- Bonus:
data Expr3 = Value3 Int
           | Add3 Expr3 Expr3
           | Sub3 Expr3 Expr3
           | Mul3 Expr3 Expr3
           | Div3 Expr3 Expr3