{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

import Data.Maybe
import Data.Char
import Control.Applicative
import Control.Monad
import Debug.Trace

{- Auxiliary testing function -}

test :: (Eq a, Show a) => [(a, a)] -> IO ()
test cases = do
  results <- mapM (\(value, expected) -> do
    putStrLn $ if value == expected
      then "\nPASS - " ++ show value ++ "\n    == " ++ show expected
      else "\nFAIL - " ++ show value ++ "\n    /= " ++ show expected
    return $ value == expected
    ) cases
  putStrLn "-------------------------"
  putStrLn $ if and results
    then "All tests are passed! :) \n"
    else "Some tests have failed. :(\n"

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


{- Remaining task from last lesson -}

-- \[(foo(, foo)*)?\]
p3' :: Parser ()
p3' = topLevel $ do
  char' '['
  optional (string' "foo" >> many (char' ',' >> string' "foo"))
  char' ']'

-- Same as p3, but count the number of elements in the list!
listLength :: Parser Int
listLength = undefined


{- Practice -}

-- # Address:

data Kind = Road | Street deriving (Eq, Show)
data Address = MkAddress { name :: String, kind :: Kind, count :: Int }
               deriving (Eq, Show)

address :: Parser Address
address = undefined

testAddress = test
  [ (epa "Ady Endre út 47/C.", Just (MkAddress {name = "Ady Endre", kind = Road, count = 47}))
  , (epa "Karinthy Frigyes út 8.", Just (MkAddress {name = "Karinthy Frigyes", kind = Road, count = 8}))
  , (epa "Budafoki út 3.", Just (MkAddress {name = "Budafoki", kind = Road, count = 3}))
  , (epa "Szilva utca 21/A.", Just (MkAddress {name = "Szilva", kind = Street, count = 21}))
  , (epa "Nagy Lantos Andor utca 9.", Just (MkAddress {name = "Nagy Lantos Andor", kind = Street, count = 9}))
  , (epa "???", Nothing)
  ] where epa = evalParser address


-- # Hex color:

data Color = MkColor { red :: Int, green :: Int, blue :: Int }
             deriving (Eq, Show)

hexDigit :: Parser Int
hexDigit = do
  c <- elemChar (['0'..'9'] ++ ['A'..'F'])
  return $ case c of {
      '0' -> 0; '1' -> 1; '2' -> 2; '3' -> 3; '4' -> 4;
      '5' -> 5; '6' -> 6; '7' -> 7; '8' -> 8; '9' -> 9;
      'A' -> 10; 'B' -> 11; 'C' -> 12; 'D' -> 13; 'E' -> 14; 'F' -> 15;
      _ -> error $ "Not a hexadecimal digit: " ++ [c]
    }

hexColor :: Parser Color
hexColor = undefined

testHexColor = test
  [ (ephc "0x000000", Just (MkColor {red = 0, green = 0, blue = 0}))
  , (ephc "0x33FE67", Just (MkColor {red = 51, green = 254, blue = 103}))
  , (ephc "0xFA55B8", Just (MkColor {red = 250, green = 85, blue = 184}))
  , (ephc "0x12GH89", Nothing)
  ] where ephc = evalParser hexColor


{- Expression parsing -}

-- # Boolean expressions

-- Write a parser that reads simple boolean expressions containing literals,
-- parentheses, negation, conjunction and disjunction and equality checks!
--
-- + The precedence of the operators is given by the following decreasing order:
-- + '!' means Not, '^' means And, 'v' means Or, '=' means equality check
-- + They all associate to the right
--
-- Valid examples:
b1 = "F"
b2 = "(T)"
b3 = "(!(F))"
b4 = "T v T ^ F"
b5 = "(T v T) ^ F"
b6 = "T v T v F v F"
b7 = "T ^ T ^ !T ^ T"
b8 = "T ^ !(F v T) = F v !T ^ T"

data BExp
  = Lit Bool
  | Not BExp
  | And BExp BExp
  | Or BExp BExp
  | Eq BExp BExp
  deriving (Show)

bLit :: Parser BExp
bLit = undefined

bPar :: Parser BExp
bPar = undefined

bAtom :: Parser BExp
bAtom = undefined

bNot :: Parser BExp
bNot = undefined

bAnd :: Parser BExp
bAnd = undefined

bOr :: Parser BExp
bOr = undefined

bEq :: Parser BExp
bEq = undefined

bExp :: Parser BExp
bExp = undefined

evalBExp :: BExp -> Bool
evalBExp exp = undefined



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


-- What would be an example for an `infixLeft` operator?


-- # Tree expressions
-- Parse tree expressions of the following form:

t1 = "(1)"
t2 = "[(1) + (2)] + (3)"
t3 = "[(1) + [(2) + (3)]] + [(4) + (5)]"

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving (Show, Eq)

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
