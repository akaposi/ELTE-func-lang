
module Parser where

import Data.Functor (void)
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Char

type Parser a = StateT String Maybe a

runParser :: Parser a -> String -> Maybe (a, String)
runParser = runStateT

evalParser :: Parser a -> String -> Maybe a
evalParser p s = fmap fst $ runParser p s

eof :: Parser ()
eof = do
  str <- get
  case str of
    "" -> pure ()
    _  -> empty

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  str <- get
  case str of
    c:cs | f c -> c <$ put cs
    _          -> empty

char :: Char -> Parser Char
char c = satisfy (== c)

lowerAlpha :: Parser Char
lowerAlpha = satisfy isLower

natural :: Parser Int
natural = read <$> some (satisfy isDigit)

token :: Parser a -> Parser a
token pa = pa <* ws

string :: String -> Parser ()
string s = () <$ traverse char s

string' :: String -> Parser ()
string' s = token (string s)

ws :: Parser ()
ws = () <$ many (satisfy isSpace)
