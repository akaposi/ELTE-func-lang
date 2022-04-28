{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}
module Gy10 where

import Control.Monad
import Control.Applicative
import Data.Char
import Debug.Trace

-- PARSER LIBRARY
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving (Functor)

evalParser :: Parser a -> String -> Maybe a
evalParser pa = (fst <$>) . runParser pa

execParser :: Parser a -> String -> Maybe String
execParser pa = (snd <$>) . runParser pa

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return :: a -> Parser a
  return a = Parser $ \s -> Just (a, s)

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s -> do {(a, s) <- f s; runParser (g a) s}

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (==c)

string :: String -> Parser ()
string s = mapM_ char s

instance Alternative Parser where
  -- mindig hibázó parser
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- választás két parser között
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing -> g s
    res     -> res

-- Control.Applicative-ból:
-- ∙ many  :: Parser a -> Parser [a]
-- ∙ some  :: Parser a -> Parser [a]

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

-- Control.Applicative-ból:
-- ∙ optional :: Parser a -> Parser (Maybe a)

optional_ :: Parser a -> Parser ()
optional_ pa = () <$ optional pa

inList :: [Char] -> Parser Char
inList str = satisfy (`elem` str)

inList_ :: [Char] -> Parser ()
inList_ str = () <$ inList str

------------------------------------------------------------

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
--   pa psep pa .... psep pa
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = do
  a  <- pa
  as <- many (psep *> pa)
  pure (a:as)

-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s -> trace (msg ++ " : " ++ s) (runParser pa s)

-- token/whitespace parsing segédfüggvények

ws :: Parser ()
ws = many_ (satisfy isSpace)

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' f = satisfy f <* ws

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- number parsing

-- Digit character with numeric value
digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

-- Convert a list of digits into a number
digitsToDecimalNumber :: [Int] -> Int
digitsToDecimalNumber digits = foldl (\acc curr -> acc * 10 + curr) 0 digits

digitsToDecimalFraction :: [Int] -> Double
digitsToDecimalFraction digits =
  foldr (\curr acc -> acc / 10 + fromIntegral curr) 0 digits / 10

-- "3.141592"
-- 3
-- .
-- [1,4,1,5,9,2]
-- 2
-- 9.2
-- 5.92
-- 1.592
-- 4.1592
-- 1.41592
-- 0.141592

-- Non-negative decimal integer number
decimalNumber :: Parser Int
decimalNumber = digitsToDecimalNumber <$> some digit

-- Non-negative decimal fractional number
decimalFractional :: Parser Double
decimalFractional = do
  int <- decimalNumber
  frac <- optional (char '.' *> some digit)
  return $ case frac of
    Nothing -> fromIntegral int
    Just fracDigits -> fromIntegral int + digitsToDecimalFraction fracDigits

-- Signed decimal fractional number
signedDecimalFractional :: Parser Double
signedDecimalFractional = do
  sign <- optional (char '-')
  frac <- decimalFractional
  return $ case sign of
             Nothing -> frac
             Just _  -> (- frac)

-- Create a parser that extracts information from weather status reports!
--
-- Examples:
-- ∙ evalParser weather "Sunny, 25.5°C"
--   == Just (MkWeather {description = "Sunny", temperature = 25.5, scale = Celsius})
-- ∙ evalParser weather "Rainy, 45°F"
--   == Just (MkWeather {description = "Rainy", temperature = 45.0, scale = Fahrenheit})
-- ∙ evalParser weather "Cloudy, -0.5°C"
--   == Just (MkWeather {description = "Cloudy", temperature = -0.5, scale = Celsius})
-- ∙ evalParser weather "Windy, 62°F"
--   == Just (MkWeather {description = "Windy", temperature = 62.0, scale = Fahrenheit})
-- ∙ evalParser weather "Snowy, -6°C"
--   == Just (MkWeather {description = "Snowy", temperature = -6.0, scale = Celsius})
-- ∙ evalParser weather "Foggy, 20°F"
--   == Just (MkWeather {description = "Foggy", temperature = 20.0, scale = Fahrenheit})

data Scale = Celsius | Fahrenheit deriving (Eq, Show)
data Weather = MkWeather
  { description :: String
  , temperature :: Double
  , scale :: Scale
  } deriving (Eq, Show)

weather :: Parser Weather
weather = do
  start <- inList ['A'..'Z']
  end <- many (inList ['a'..'z'])
  string ", "
  -- char' ','
  temp <- signedDecimalFractional
  unit <- (Fahrenheit <$ string "°F" <|> Celsius <$ string "°C")
  return $ MkWeather (start : end) temp unit


-- operátor segédfüggvények

rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f pa psep = foldr1 f <$> sepBy1 pa psep

leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f pa psep = foldl1 f <$> sepBy1 pa psep

nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc f pa psep = do
  exps <- sepBy1 pa psep
  case exps of
    [e]      -> pure e
    [e1,e2]  -> pure (f e1 e2)
    _        -> empty


-- Írj egy parser-t, ami zárójeleket, +-t és pozitív Int literálokat tartalmazó
-- kifejezéseket olvas! (Lásd előadás) Whitespace-t mindenhol engedj meg.
--------------------------------------------------------------------------------
--   példák: 10 + 20 + 30
--           (10 + 20) + 30
--           10 + ((20 + 5))
--
-- A + operátor jobbra asszociáljon, azaz pl. 1 + 2 + 3 olvasása legyen
--  (Plus (Lit 1) (Plus (Lit 2) (Lit 3)))

data Exp = Lit Int | Plus Exp Exp | Mult Exp Exp deriving Show

pLit :: Parser Exp
pLit = Lit <$> decimalNumber <* ws

pParen :: Parser Exp
pParen = char' '(' *> pPlus <* char' ')'

pAtom :: Parser Exp
pAtom = pLit <|> pParen

pMult :: Parser Exp
pMult = rightAssoc Mult pAtom (char' '*')

pPlus :: Parser Exp
pPlus = rightAssoc Plus pMult (char' '+')

pExp :: Parser Exp
pExp = topLevel pPlus

-- Create an evaluator for the expressions defined above!
evalExp :: Exp -> Int
evalExp (Lit i) = i
evalExp (Plus l r) = evalExp l + evalExp r
evalExp (Mult l r) = evalExp l * evalExp r

-- Combine the parse with the evaluator!
evalString :: String -> Int
evalString str = case evalParser pExp str of
                   Nothing -> error "Invalid expression!"
                   Just exp -> evalExp exp

-- Extend the parser and the evaluator with the multiplication operator '*',
-- which associates to the right and has stronger precedence, than '+'!

-- Extend the parser and the evaluator with the exponentiation operator '^',
-- which associates to the right and has stronger precedence, than '*'!
