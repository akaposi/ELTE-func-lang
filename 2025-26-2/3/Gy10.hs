{-# LANGUAGE LambdaCase #-}
module Gy10 where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad
import Data.Char
import Data.List
import Data.Bifunctor
import Language.Haskell.TH.Ppr (Precedence)

-- So far:
-- Functor f 
--    <$> :: (a -> b) -> f a -> f b
-- Functor f => Applicative f
--    pure :: a -> f a
--    (<*>) :: f (a -> b) -> f a -> f b
--    liftA2 :: (a -> b -> c) -> f a -> f b -> f c, where liftA2 f x y = f <$> x <*> y
--    (*>) :: f a -> f b -> f b 
--    (<*) :: f a -> f b -> f a
-- Applicative m => Monad m
--    (>>=) :: m a -> (a -> m b) -> m b
--    return :: a -> m a
-- Applicative f => Alternative f
--    empty :: f a
--    (<|>) :: f a -> f a -> f a

-- Parser with error messages
type Parser a = StateT String (Except String) a
-- newtype Parser a = Parser { runParser :: String -> Maybe (a, String) } deriving Functor

runParser :: Parser a -> String -> Either String (a, String)
runParser p s = runExcept (runStateT p s)

(<|>) :: MonadError e m => m a -> m a -> m a
f <|> g = catchError f (const g)
infixl 3 <|>

-- In case the parser p fails, optional p still doesn't fail
optional :: MonadError e m => m a -> m (Maybe a)
optional f = Just <$> f <|> pure Nothing

-- Run parser 0 or more times
many :: MonadError e m => m a -> m [a]
many p = some p <|> pure []

-- Run parser 1 or more times
some :: MonadError e m => m a -> m [a]
some p = (:) <$> p <*> many p

-- Primitive parser combinators

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = get >>= \case
  (c:cs) | p c -> c <$ put cs
  _            -> throwError "satisfy: condition not met or string empty"

eof :: Parser ()
eof = get >>= (<|> throwError "eof: String not empty") . guard . null

char :: Char -> Parser ()
char c = void $ satisfy (== c) <|> throwError ("char: not equal to " ++ [c])

anyChar :: Parser Char
anyChar = satisfy (const True)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit <|> throwError "digit: Not a digit"

string :: String -> Parser ()
string str = mapM_ (\c -> char c <|> throwError ("string: mismatch on char " ++ [c] ++ " in " ++ str)) str

-- Parser with actually useful return values

-- Let's parse at least one digit. Return value should be the parsed list of digits in order
atLeastOneDigit :: Parser [Int]
atLeastOneDigit = some digit
-- map digitToInt <$> some (satisfy (\c -> c >= '0' && c <= '9'))

-- Use the previous parser to make a parser for natural numbers
-- "123" input --atLeastOneDigit--> [1,2,3] --> 123
natural :: Parser Int
natural = foldl1 (\a b -> a * 10 + b) <$> atLeastOneDigit

-- Use the previous parser to make a parser for (optionally) signed integers
integer :: Parser Int
integer = do 
  sig <- optional (char '-')
  nat <- natural
  case sig of
    Nothing -> return nat 
    Just _ -> return $ negate nat

-- Bonus: Float parser
float :: Parser Double
float = do
    s <- (\s -> if null s then 1 else -1) <$> optional (char '-')
    i <- natural
    char '.'
    r <- foldr1 (\a acc -> a + acc / 10) <$> some (fromIntegral <$> digit)
    pure $ s * (r / 10 + fromIntegral i)

-- Define a parser which parses a value between two other parsers
-- ex.: between (char '(') (string "apple") (char ')') parses the "(apple)" string, but not the "apple" string
-- Hint: *>, <*
between :: Parser left -> Parser a -> Parser right -> Parser a
between l a r = (l *> a) <* r
--                 ^ Sequences actions l and a and discards l's output
--                        ^ Sequences actions (l *> a) and r and discards r's output

-- Define a parser similar to some, but all values must be seperated by a given delimiter
sepBy1 :: Parser a -> Parser delim -> Parser {- non empty -} [a]
sepBy1 a delim = (:) <$> a <*> many (delim *> a)
-- ex.: "a,b,c"

-- Same as the one above, but there does not need to be at least 1 value
sepBy :: Parser a -> Parser delim -> Parser [a]
sepBy a delim = sepBy1 a delim <|> pure []

-- Define a parser which parser a list of integers
-- ex.: [1,2,30,40,-10]
listOfNumbers :: Parser [Int]
listOfNumbers = between (char '[') (sepBy integer (char ',')) (char ']')

-- Whitespace dropping
ws :: Parser ()
ws = void $ many $ satisfy isSpace

-- Tokenisation: dropping all whitespaces after a parser
tok :: Parser a -> Parser a
tok p = p <* ws

topLevel :: Parser a -> Parser a
topLevel p = ws *> tok p <* eof

-- We label tokenized parsers with '

natural' :: Parser Int
natural' = tok natural

integer' :: Parser Int
integer' = tok integer

char' :: Char -> Parser ()
char' c = tok $ char c

string' :: String -> Parser ()
string' str = tok $ string str

-- Redefine listOfNumbers with support for whitespaces
goodListofNumbers :: Parser [Int]
goodListofNumbers = topLevel $ between (char' '[') (sepBy integer' (char' ',')) (char' ']')

-- Folding parsers

-- Right associative parser
-- Collect a list of expression with sepBy1 and then fold over them in the appropriate direction
rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f a sep = foldr1 f <$> sepBy1 a sep
-- ex. runParser (rightAssoc (-) integer' (char' ',')) "10, 80, 8"
-- = 10 - (80 - 8)

-- Same as rightAssoc but left
leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f a sep = foldl1 f <$> sepBy1 a sep
-- ex. runParser (leftAssoc (-) integer' (char' ',')) "10, 80, 8"
-- = (10 - 80) - 8

-- Optional HW
-- Parser for non associative operations (ex.: == because 1 == 2 == 3 == 4 doesn't make sense either way)
nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc = undefined


-- A bit more competent version of the parsers above

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 v op = do
  val <- v
  ( do
      opr <- op
      res <- chainr1 v op
      pure (opr val res)
    )
    <|> pure val

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 v op = v >>= parseLeft
  where
    parseLeft val =
      ( do
          opr <- op
          val2 <- v
          parseLeft (opr val val2)
      )
        <|> pure val


-- Recursive Descent Parsing => algorithm for parsing expression trees
-- We model an expression language using ADTs:

data Exp
  = IntLit Int -- integer literals ex.: 1, 2, -5
  | FloatLit Double -- floating point literals ex.: 1.1
  | Var String -- variable names
  | Exp :+ Exp -- addition
  | Exp :* Exp -- multiplication
  | Exp :^ Exp -- exponentiation
  deriving (Eq, Show)

-- 2.5 + 3
-- We model it as 
-- FloatLit 2.5 :+ IntLit 3

-- Recursive Descent Parsing algorithm
-- 1, Collect all operations and their precedence in a table and sort them by their precedence
{-
+--------------------+--------------------+--------------------+
| Name               | Fixity             | Precedence         |
+--------------------+--------------------+--------------------+
| ^                  | Right              | 16                 |
+--------------------+--------------------+--------------------+
| *                  | Left               | 14                 |
+--------------------+--------------------+--------------------+
| +                  | Left               | 12                 |
+--------------------+--------------------+--------------------+
-}
-- 2, Define a parser for each precedence level, plus one extra for an atom

var' :: Parser String
var' = tok $ some (satisfy isLetter)

pAtom :: Parser Exp
pAtom = (IntLit <$> integer') <|>
        (FloatLit <$> (tok float)) <|>
        (Var <$> var') <|>
        between (char' '(') pAdd (char ')')

pPow :: Parser Exp
pPow = rightAssoc (:^) pAtom (char' '^')

pMul :: Parser Exp
pMul = leftAssoc (:*) pPow (char' '*')

pAdd :: Parser Exp
pAdd = leftAssoc (:+) pMul (char' '+')

-- ex. runParser pAdd "2 ^ 3 + 4 * z"

-- 3,
-- We use parsers based on their fixity
-- Right   => rightAssoc or chainr1
-- Left    => leftAssoc or chainl1
-- Neither => nonAssoc
-- The delimiter is the tokenized parser for the symbol (char' '+', string' "**", stb)
-- The intermediate values are the parser for the expression in the table one level up (addition -> multiplication), and pAtom for the first line

-- 4,
-- pAtom parses literals and variables and expression in brackets, which resets precedence

-- Extra
-- Add the following operators to the RDP implementation
{-
+--------------------+--------------------+--------------------+
| Name               | Fixity             | Precedence         |
+--------------------+--------------------+--------------------+
| #                  | Left               | 15                 |
+--------------------+--------------------+--------------------+
| /                  | Right              | 13                 |
+--------------------+--------------------+--------------------+
| -                  | Right              | 13                 |
+--------------------+--------------------+--------------------+
-}

