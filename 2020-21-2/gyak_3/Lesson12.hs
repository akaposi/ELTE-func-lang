{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

import Data.Maybe
import Data.Char
import Control.Applicative
import Control.Monad
import Debug.Trace


{- Parser -}

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

evalParser :: Parser a -> String -> Maybe a
evalParser pa = (fst <$>) . runParser pa

execParser :: Parser a -> String -> Maybe String
execParser pa = (snd <$>) . runParser pa

-- (State String + Maybe) monad
-- Parser a : function, which can modify a String state and can potentially fail
--   Nothing     : parse error
--   Just (a, s) : successful parsing with result and remaining input

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  -- No input is consumed
  return :: a -> Parser a
  return a = Parser $ \s -> Just (a, s)
  -- runParser (return 10) "asdf" == Just (10,"asdf")

  -- Executing parsers sequentially
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (g a) s'

instance Alternative Parser where
  -- Parser that fails instantly
  empty :: Parser a
  empty = Parser $ const Nothing

  -- Try the first parser, in case of failure try the second one
  -- (p1 <|> p2 is practically equivalent to the `p1|p2` RegEx)
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser f) (Parser g) =
    Parser $ \s -> case f s of
      Nothing      -> g s
      Just (a, s') -> Just (a, s')


debug :: String -> Parser ()
debug msg = Parser $
  \s -> trace ("{- " ++ msg ++ " # input: " ++ show s ++ " -}") (Just ((), s))

-- Basic parsers

-- "end of file" - succeeds only on empty input ($ in RegEx)
eof :: Parser ()
eof = Parser $ \case
  [] -> Just ((), [])
  _  -> Nothing

-- Read a character if it satisfies a certain criteria
-- (And the input is not empty.)
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

-- Read a specific character
char :: Char -> Parser ()
char c = () <$ satisfy (== c)

-- Read any character (. in RegEx)
anyChar :: Parser Char
anyChar = satisfy (const True)

-- Read a specific string of characters
string :: String -> Parser ()
string = mapM_ char

-- From Control.Applicative:

-- Zero or more repetitions (* in RegEx)
-- many :: Parser a -> Parser [a]

-- One or more repetitions (+ in RegEx)
-- some :: Parser a -> Parser [a]

-- Zero or one repetition (? in RegEx)
-- optional :: Parser a -> Parser (Maybe a)

many_ :: Parser a -> Parser ()
many_ p = () <$ many p

some_ :: Parser a -> Parser ()
some_ p = () <$ some p

optional_ :: Parser a -> Parser ()
optional_ p = () <$ optional p

-- Read one or more elements with separators between them
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep >> pa)

-- Read zero or more elements with separators between them
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- Parser version of 'foldl'
chainl :: (b -> a -> b) -> Parser b -> Parser a -> Parser b
chainl f pb pa = do {b <- pb; go b} where
  go b = (do {a <- pa; go (f b a)}) <|> pure b

-- Element of a list ([...] in RegEx)
elemChar :: [Char] -> Parser Char
elemChar chars = satisfy (`elem` chars)

-- Whitespace, such as space, tab, newline (\s in RegEx)
whitespace :: Parser Char
whitespace = elemChar [' ', '\n', '\t']

-- A continous section of zero or more whitespace
ws :: Parser ()
ws = () <$ many whitespace

-- Digit character with numeric value
digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

-- Convert a list of digits into a number
digitsToNumber :: Int -> [Int] -> Int
digitsToNumber base = foldl ((+) . (*base)) 0

-- Non-negative integer number
number :: Parser Int
number = digitsToNumber 10 <$> some digit

-- RegEx match checker
match :: Parser a -> String -> Bool
match p s = isJust (runParser p s)

-- Tokenization:
char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof


{- Expression parsing -}

-- # Boolean expressions
--
-- Write a parser that reads simple boolean expressions containing literals,
-- parentheses, negation, conjunction and disjunction and equality checks!
--
-- + The precedence of the operators is given by the following decreasing order:
-- + '!' means Not, '^' means And, 'v' means Or, '=' means equality check
-- + '!', '^' and 'v' associate to the right, '=' is not associative
--
-- Valid examples:
b0 = "F"
b1 = "(T)"
b2 = "(!(F))"
b3 = "T v T ^ F"
b4 = "(T v T) ^ F"
b5 = "T v T v F v F"
b6 = "T ^ T ^ !T ^ T"
b7 = "T ^ !(F v T) = F v !T ^ T"
b8 = "!(if F = T then F else T) v F"
b9 = "if a then F else b ^ c"

data BExp
  = Lit Bool
  | Not BExp
  | And BExp BExp
  | Or BExp BExp
  | Eq BExp BExp
  | If BExp BExp BExp
  | Ref String
  deriving (Show)

bLit :: Parser BExp
bLit = Lit True <$ char' 'T' <|> Lit False <$ char' 'F'

bPar :: Parser BExp
bPar = char' '(' *> bOr <* char' ')'

bAtom :: Parser BExp
bAtom = bPar <|> bLit

bNot :: Parser BExp
bNot = prefixAssoc Not (char' '!') bAtom

bAnd :: Parser BExp
bAnd = infixRight And bNot (char' '^')

bOr :: Parser BExp
bOr = infixRight Or bAnd (char' 'v')

bEq :: Parser BExp
bEq = infixNonAssoc Eq bOr (char' '=')

bExp :: Parser BExp
bExp = topLevel bEq

evalBExp :: BExp -> Bool
evalBExp exp = undefined

-- What would be an example for an `infixLeft` operator?


-- Auxiliary operator parser functions

infixLeft :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
infixLeft f pa psep = foldl1 f <$> sepBy1 pa psep

infixRight :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
infixRight f pa psep = foldr1 f <$> sepBy1 pa psep

infixNonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
infixNonAssoc f pa psep = do
  es <- sepBy1 pa psep
  case es of
    [e]      -> pure e
    [e1, e2] -> pure $ f e1 e2
    _        -> empty

prefixAssoc :: (a -> a) -> Parser op -> Parser a -> Parser a
prefixAssoc f pop pa = (pop *> (f <$> prefixAssoc f pop pa)) <|> pa

prefixNonAssoc :: (a -> a) -> Parser op -> Parser a -> Parser a
prefixNonAssoc f pop pa = (pop *> (f <$> pa)) <|> pa


-- # Tree expressions
-- Parse tree expressions of the following form:
-- (The '+' operator is not associative!)

t1 = "(1)"
t2 = "[(1) + (2)] + (3)"
t3 = "[(1) + [(2) + (3)]] + [(4) + (5)]"

data Tree a = Node (Tree a) (Tree a) | Leaf a
              deriving (Show, Eq)

tBracket :: Parser (Tree Int)
tBracket = undefined

tLeaf :: Parser (Tree Int)
tLeaf = undefined

tAtom :: Parser (Tree Int)
tAtom = undefined

tNode :: Parser (Tree Int)
tNode = undefined

tExp :: Parser (Tree Int)
tExp = undefined


-- # Extra: Write a parser for lambda calculus terms!
-- https://en.wikipedia.org/wiki/Lambda_calculus
--
-- Examples
-- + \f x y. f y y
-- + (\x. x) (\x. x)
-- + (\f x y. f) x (g x)

data LTm = Var String | App LTm LTm | Lam String LTm
           deriving Show

lTm :: Parser LTm
lTm = undefined



{- Practice -}

-- # Rose Tree
-- https://en.wikipedia.org/wiki/Rose_tree

data RoseTree a = Branch a [RoseTree a]
                  deriving (Eq, Ord, Show)

rt :: RoseTree Int
rt =
  Branch 2
    [ Branch 3
      [ Branch 11 []
      ]
    , Branch 5 []
    , Branch 7
      [ Branch 13 []
      ]
    ]

instance Functor RoseTree where
  fmap f rt = undefined

instance Foldable RoseTree where
  foldr f acc rt = undefined

instance Traversable RoseTree where
  traverse f rt = undefined
