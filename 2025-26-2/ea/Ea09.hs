{- HLINT ignore "Use lambda-case" -}
{- HLINT ignore "Use foldr" -}
{-# LANGUAGE DerivingStrategies #-}
module Ea9 where
import Control.Applicative
import Control.Monad

--- String -> Maybe (a, String)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- A funktor parser poscomposition
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser g) = Parser $ \s -> case g s of
    Just (a, rest) -> Just (f a, rest)
    Nothing -> Nothing

instance Applicative Parser where

  pure :: a -> Parser a
  pure a = Parser $ \s -> Just (a, s)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser f <*> Parser a = Parser $ \s -> case f s of
    Nothing -> Nothing
    Just (f', s) -> case a s of
      Nothing -> Nothing
      Just (a', s) -> Just (f' a', s)



instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser a >>= f = Parser $ \s -> case a s of
    Nothing -> Nothing
    Just (a', s) -> runParser (f a') s


char :: Char -> Parser ()
char c = Parser $ \input -> case input of
  [] -> Nothing
  (k:ks) -> if k == c then Just ((), ks) else Nothing


string :: String -> Parser ()
string [] = pure ()
string (c:cs) = char c >> string cs


anychar :: Parser ()
anychar = Parser $ \s -> case s of
  [] -> Nothing
  (c:cs) -> Just ((), cs)


instance Alternative Parser where

  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser a <|> Parser b = Parser $ \s -> case a s of
    Nothing      -> b s
    Just (a', s) -> Just (a', s)

-- (a*)almafa
-- aaaaaaaaaaaaaaaaaaaalmafa


newtype NParser a = NParser { runNParser :: String -> [(a, String)] }
  deriving Functor

instance Applicative NParser where
  pure a = NParser $ \s -> [(a, s)]
  (<*>) = ap

instance Monad NParser where
  NParser a >>= f = NParser $ \s -> let p = a s in concatMap (\(a, r) -> runNParser (f a) r) p


charN :: Char -> NParser ()
charN c = NParser $ \input -> case input of
  [] -> []
  (k:ks) -> ([((), ks) | k == c])

stringN :: String -> NParser ()
stringN [] = pure ()
stringN (c:cs) = charN c >> stringN cs

anycharN :: NParser ()
anycharN = NParser $ \s -> case s of
  [] -> []
  (c:cs) -> [((), cs)]

instance Alternative NParser where
  empty = NParser $ const []
  NParser l <|> NParser r = NParser $ \s -> l s ++ r s

-- (a*)almafa
-- aaaaaaaaaaaaaaaaaaaalmafa
-- runNParser (many (charN 'a') *> stringN "almafa") "aaaaaaaaaaaaaaaaaaaalmafa"

-- [abcd]
-- ^ a-t vagy b-t vagy c-t vagy d-t parseol

oneOf :: [NParser a] -> NParser a
oneOf [] = empty
oneOf (x:xs) = x <|> oneOf xs


-- <$ = constfmap

number :: NParser Int
number = do
  x <- some $ oneOf ((\i -> i <$ stringN (show i)) <$> [0..9])
  -- x :: [Int]
  -- x = [1,2,3] -> 123?
  -- let xwi = zip [0..] (reverse x)
  --return (sum $ map (\(i, e) -> 10 ^ i * e) xwi)
  return (foldl (\acc t -> t + 10 * acc) 0 x)

{-
csv = comma seperated values
-}

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  [] -> Nothing
  (c:cs) -> case p c of
    True -> Just (c, cs)
    False -> Nothing

csv :: Parser [String]
csv = many $ do
  one <- some (satisfy (/= ','))
  many (char ',')
  return one

