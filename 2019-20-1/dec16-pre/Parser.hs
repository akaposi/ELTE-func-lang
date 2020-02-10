{-# LANGUAGE InstanceSigs #-}
module Parser where

import Data.Functor (void)
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.State

type Parser a = StateT String Maybe a

runParser :: Parser a -> String -> Maybe (a, String)
runParser = runStateT

eof :: Parser ()
eof = do
  str <- get
  case str of
    "" -> do
      put ""
      pure () -- lift $ Just (), lift $ pure ()
    _  -> lift $ Nothing

char :: Char -> Parser Char
char c = do
  str <- get
  case str of
    (x:xs) | x == c -> do
      put xs
      pure c
    _ -> lift $ Nothing


lowerAlpha :: Parser Char
lowerAlpha = foldr (<|>) empty $ map char ['a'..'z']

digit :: Parser Int
digit = fmap (\n -> n - 48)
      . fmap fromEnum
      . foldl (<|>) empty
      $ map char ['0'..'9']

natural :: Parser Int
natural = foldl1 (\acc cur -> 10*acc + cur) <$> some digit

token' :: String -> Parser ()
token' str = void $ traverse char str

token :: String -> Parser ()
token str = token' str <* ws

ws :: Parser ()
ws = void $ many (char ' ')
