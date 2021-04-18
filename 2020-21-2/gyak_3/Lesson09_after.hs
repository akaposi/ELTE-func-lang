{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

import Data.Maybe
import Data.Char
import Control.Applicative
import Control.Monad

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

  -- (p1 <|> p2) <|> p3 = p1 <|> (p2 <|> p3)
  -- p <|> empty = p
  -- empty <|> p = p


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
-- string [] = pure ()
-- string (c:cs) = do
--   char c
--   string cs

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
elemChar = undefined

-- Whitespace, such as space, tab, newline (\s in RegEx)
whitespace :: Parser Char
whitespace = undefined

-- A continous section of zero or more whitespace
ws :: Parser ()
ws = undefined

-- RegEx match checker
match :: Parser a -> String -> Bool
match p s = isJust (runParser p s)

p :: Parser ()
p = undefined

-- Implement the following RegExes:

-- (ab|cd)+$
p1 :: Parser ()
p1 = some_ (string "ab" <|> string "cd") >> eof

p1Tests :: [Bool]
p1Tests =
  [ not $ match p1 ""
  , not $ match p1 "abasdf"
  ,       match p1 "ababcdab"
  ,       match p1 "cd"
  ]

-- (foo|bar)*baz
p2 :: Parser ()
p2 = many_ (string "foo" <|> string "bar") *> string "baz"

p2Tests :: [Bool]
p2Tests =
  [       match p2 "baz"
  ,       match p2 "foofoobaz"
  , not $ match p2 "fobarobaz"
  ,       match p2 "barfoobarbaz"
  ]

-- \[(foo(, foo)*)?\]
p3 :: Parser ()
p3 = do
  char '['
  optional_ (string "foo" >> many (string ", foo"))
  char ']'

p3Tests :: [Bool]
p3Tests =
  [       match p3 "[]"
  ,       match p3 "[foo]"
  , not $ match p3 "foo"
  , not $ match p3 "[foo, foo, foo"
  ,       match p3 "[foo, foo, foo]"
  ]


-- Implement the following parsers:

-- [a-z]
lowercase :: Parser Char
lowercase = undefined

-- Parse an email address of the following format and return the username!
-- [a-z]+@domain\.(com|org|hu)
emailUser :: Parser ()
emailUser = undefined

-- A single digit
digit :: Parser Int
digit = undefined

-- Non-negative integer number
number :: Parser Int
number = undefined

-- Parse a description of a person in the following format:
-- "Isti is a 24 year old boy"
-- "Mary is a 29 year old girl"
data Gender = Boy | Girl
data Person = MkPerson { name :: String, age :: Int, gender :: Gender }

person :: Parser Person
person = undefined

-- Same as p3, but count the number of elements in the list!
listLength :: Parser Int
listLength = undefined
