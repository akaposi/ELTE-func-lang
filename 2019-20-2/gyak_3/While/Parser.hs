{-# LANGUAGE InstanceSigs #-}
module Parser where

import Data.Functor (void)
import Control.Applicative

newtype Parser a = P { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P $ \str -> case runParser p str of
    Nothing       -> Nothing
    Just (x,str') -> Just (f x, str')

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \str -> Just (x, str)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) p q = P $ \str -> do
    (f, str')  <- runParser p str
    (x, str'') <- runParser q str'
    pure (f x, str'')

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) p k = P $ \str -> do
    (x, str') <- runParser p str
    runParser (k x) str'

-- monoid for Applicitve functors
instance Alternative Parser where
  empty :: Parser a
  empty = P (const Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) p q = P $ \str -> case runParser p str of
    Nothing  -> runParser q str
    Just res -> Just res

eof :: Parser ()
eof = P $ \str -> case str of
  "" -> Just ((), "")
  _  -> Nothing

char :: Char -> Parser Char
char c = P $ \str -> case str of
  (x:xs) | x == c -> Just (c, xs)
  _ -> Nothing

lowerAlpha :: Parser Char
lowerAlpha = foldr (<|>) empty $ map char ['a'..'z']

digit :: Parser Int
digit = fmap (\n -> n - 48)
      . fmap fromEnum
      . foldl (<|>) empty
      $ map char ['0'..'9']

natural :: Parser Int
natural = foldl1 (\acc cur -> 10*acc + cur) <$> some digit

string :: String -> Parser String
string str = traverse char str

token' :: String -> Parser ()
token' str = void $ string str

token :: String -> Parser ()
token str = token' str <* ws

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P $ \str -> case str of
  (c:cs) | p c -> Just (c, cs)
  _ -> Nothing

ws :: Parser ()
ws = void $ many (char ' ')

-- Practice 10

runParser2 :: Parser a -> String -> Maybe a
runParser2 p str = fst <$> runParser p str


-- NOTE: natural, token, ws (?)
coordinate :: Parser (Int, Int)
coordinate = (,) <$> (ws *> token "(" *> naturalWS) <*> (token "," *> naturalWS <* token ")") where
  naturalWS = natural <* ws

{-
runParser coordinate "(1,2)"             == Just ((1,2), "")
runParser coordinate "(14,23)"           == Just ((14,23), "")
runParser coordinate "  ( 14  ,  23 )  " == Just ((14,23), "")
runParser coordinate "(14,23"  == Nothing
runParser coordinate "(14 23)" == Nothing
runParser coordinate "14 23)"  == Nothing
runParser coordinate "(14,)"   == Nothing
runParser coordinate "(,23)"   == Nothing

runParser2 coordinate "(1,2)"             == Just (1,2)
runParser2 coordinate "(14,23)"           == Just (14,23)
runParser2 coordinate "  ( 14  ,  23 )  " == Just (14,23)
runParser2 coordinate "(14,23"  == Nothing
runParser2 coordinate "(14 23)" == Nothing
runParser2 coordinate "14 23)"  == Nothing
runParser2 coordinate "(14,)"   == Nothing
runParser2 coordinate "(,23)"   == Nothing
-}
