{-# LANGUAGE InstanceSigs #-}
module Practice09 where

import Control.Applicative

-- transactional
newtype Parser a = P { runParser :: String -> Maybe (a, String) }
-- the State is always calculated
-- ~ State String (Maybe a)
-- newtype Parser a = P { runParser :: String -> (Maybe a, String) } 

instance Functor Parser where 
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P $ \str -> case runParser p str of
    Nothing       -> Nothing
    Just (x,str') -> Just (f x, str') 
  {-
  fmap f (P g) = P $ \str -> case g str of
    Nothing -> undefined
    Just _  -> undefined
  -}

instance Applicative Parser where 
  pure :: a -> Parser a 
  pure x = P $ \str -> Just (x, str)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) p q = P $ \str -> do 
    (f, str')  <- runParser p str
    (x, str'') <- runParser q str'
    pure (f x, str'')
  {-
  (<*>) p q = P $ \str -> case runParser p str of 
    Nothing -> Nothing 
    Just (f, str') -> case runParser q str' of 
      Nothing -> Nothing
      Just (x, str'') -> Just (f x, str'')
  -}

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
  empty = unit 

  (<|>) :: Parser a -> Parser a -> Parser a 
  (<|>) = combine 

many' :: Alternative f => f a -> f [a]
many' f = some f <|> pure []

some' :: Alternative f => f a -> f [a]
some' f = (:) <$> f <*> many' f

combine :: Parser a -> Parser a -> Parser a
combine p q = P $ \str -> case runParser p str of 
  Nothing  -> runParser q str
  Just res -> Just res

unit :: Parser a 
unit = P (const Nothing)

eof :: Parser () 
eof = P $ \str -> case str of 
  "" -> Just ((), "") 
  _  -> Nothing 

char :: Char -> Parser Char 
char c = P $ \str -> case str of 
  (x:xs) | x == c -> Just (c, xs)
  _ -> Nothing

ab :: Parser Char 
ab = char 'a' <|> char 'b'

-- use foldr
lowerAlpha :: Parser Char 
lowerAlpha = foldr (<|>) empty $ map char ['a'..'z'] 

-- use fmap, fromEnum
digit :: Parser Int
digit = fmap (\n -> n - 48)
      . fmap fromEnum 
      . foldl (<|>) empty 
      $ map char ['0'..'9']

-- use foldl, digit
natural :: Parser Int 
natural = undefined 

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