{-# LANGUAGE InstanceSigs #-}
module Practice09 where

import Control.Applicative

newtype Parser a 
  = P { runParser :: String -> Maybe (a, String) }

instance Functor Parser where 
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (P g) = P $ \str -> case g str of
    Just (x, str') -> Just (f x, str')
    Nothing        -> Nothing 

instance Applicative Parser where 
  pure :: a -> Parser a
  pure x = P $ \str -> Just (x, str) 

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b 
  (<*>) (P pF) (P g) = P $ \str -> do
    (f, str')  <- pF str
    (x, str'') <- g str'
    pure (f x, str'')

eof :: Parser () 
eof = P $ \str -> case str of 
  "" -> Just ((), str)
  _  -> Nothing

char :: Char -> Parser Char 
char c = P $ \str -> case str of 
  (x:xs) | x == c -> Just (c, xs)
  _               -> Nothing 

instance Alternative Parser where 
  empty :: Parser a 
  empty = P (\_ -> Nothing) 

  (<|>) :: Parser a -> Parser a -> Parser a  
  (<|>) p q = P $ \str -> case runParser p str of 
    Just res -> Just res 
    Nothing  -> runParser q str

charXY :: Parser Char 
charXY = char 'x' <|> char 'y'

anyChar :: Parser Char
anyChar = foldr (<|>) empty $ map char ['a'..'z']
-- char 'a' <|> char 'b' <|> char 'c' <|> ... <|> char 'z'

-- parses a given string
string :: String -> Parser [Char]
string = traverse char

some' :: Alternative f => f a -> f [a]
some' f = (:) <$> f <*> many' f

many' :: Alternative f => f a -> f [a]
many' f = some' f <|> pure []

data Bit = F | T
  deriving (Eq, Ord, Show)
  
data ShortByte = SB Bit Bit Bit Bit
  deriving (Eq, Ord, Show)

bitF :: Parser Bit 
bitF = char '0' *> pure F

bitT :: Parser Bit 
bitT = char '1' *> pure T

bit :: Parser Bit 
bit = bitF <|> bitT

shortByte :: Parser ShortByte
shortByte = SB <$> bit <*> bit <*> bit <*> bit

instance Monad Parser where 
  return :: a -> Parser a 
  return = pure 

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) p k = P $ \str -> case runParser p str of 
    Nothing        -> Nothing 
    Just (x, str') -> runParser (k x) str'

times :: Int -> Parser a -> Parser [a]
-- times 0 p = pure []
-- times n p = (:) <$> p <*> times (n-1) p 
times n p = traverse (\_ -> p) [1..n]

digit :: Parser Int
digit = fmap (\n -> n - 48)
      . fmap fromEnum
      . foldr (<|>) empty 
      $ map char ['0'..'9']

natural :: Parser Int
natural = foldl (\acc cur -> acc*10 + cur) 0 <$> some digit

foo :: Parser a -> Parser [a]
foo p = do 
  n <- natural 
  times n p