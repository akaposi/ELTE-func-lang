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

-- Read one or more elements with separators between them
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep >> pa)

-- Read zero or more elements with separators between them
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []


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

-- Element of a list ([...] in RegEx)
elemChar :: [Char] -> Parser Char
elemChar chars = satisfy (`elem` chars)

-- Whitespace, such as space, tab, newline (\s in RegEx)
whitespace :: Parser Char
whitespace = undefined

-- A continous section of zero or more whitespace
ws :: Parser ()
ws = undefined

-- RegEx match checker
match :: Parser a -> String -> Bool
match p s = isJust (runParser p s)


-- Implement the following RegExes:

-- \[(foo(, foo)*)?\]
p3 :: Parser ()
p3 = undefined

p3Tests :: [Bool]
p3Tests =
  [       match p3 "[]"
  ,       match p3 "[foo]"
  , not $ match p3 "foo"
  , not $ match p3 "[foo, foo, foo"
  ,       match p3 "[foo, foo, foo]"
  ]


-- Parse a description of a person in the following format:
-- + Begins with a name, which is a single uppercase letter followed by one or more lowercase letters
-- + Followed by " is a " or " is an " [hint: the 'n' is optional]
-- + Followed by a number (one or more digits)
-- + Followed by " year old " or " years old " [hint: the 's' is optional]
-- + Followed by either "girl" or "boy"
-- + Ends with a period

p4 :: Parser ()
p4 = undefined

p4Tests :: [Bool]
p4Tests =
  [       match p4 "Isti is a 24 years old boy."
  ,       match p4 "Mary is a 29 years old girl."
  ,       match p4 "Liza is an 8 years old girl."
  ,       match p4 "Peter is a 1 year old boy."
  , not $ match p4 "isti is a 24 years old boy."
  , not $ match p4 "Mary is a NaN years old girl."
  , not $ match p4 "Liza is an 8 years old N/A."
  , not $ match p4 "PeteR is an 1 year old boy."
  , not $ match p4 "P is an 1 year old boy."
  ]

-- Tokenization:

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

p3' :: Parser ()
p3' = undefined


-- Implement the following parsers:

-- [a-z]
lowercase :: Parser Char
lowercase = undefined

-- Parse an email address of the following format and return the username!
-- [a-z]+@(inf\.)?elte\.hu
emailUser :: Parser String
emailUser = undefined

-- A single digit
digit :: Parser Int
digit = undefined

-- Integer number
number :: Parser Int
number = undefined


-- Same as p3, but count the number of elements in the list!
listLength :: Parser Int
listLength = undefined


data Gender = Boy | Girl deriving (Eq, Show)
data Person = MkPerson { name :: String, age :: Int, gender :: Gender }
              deriving (Eq, Show)

-- Same as p4, but also return the acquired data!
person :: Parser Person
person = undefined


-- Write a parser that reads simple boolean expressions containing
-- literals, parenthesis, addition and multiplication!
--
-- + '^' means And, 'v' means Or
-- + And has higher precedence
-- + They both associate to the right
--
-- Valid examples:
--   F
--   (T)
--   ((F))
--   T v T ^ F
--   (T v T) ^ F
--   T v T v F v F

data Exp = Lit Bool | And Exp Exp | Or Exp Exp deriving (Show)

pLit :: Parser Exp
pLit = undefined

pPar :: Parser Exp
pPar = undefined

pAtom :: Parser Exp
pAtom = undefined

pAnd :: Parser Exp
pAnd = undefined

pOr :: Parser Exp
pOr = undefined

pExp :: Parser Exp
pExp = undefined

evalExp :: Exp -> Bool
evalExp exp = undefined


-- Homework:
-- Write a parser, that recognizes balanced parenthesis!
-- Valid examples: "", "()", "()()", "(())()", "(()())", "((()())())"
balancedPar :: Parser ()
balancedPar = undefined
