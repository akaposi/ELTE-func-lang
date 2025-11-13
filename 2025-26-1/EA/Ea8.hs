module Ea8 where

import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char

-- type Parser a = StateT String Maybe a

newtype Parser a = Parser (String -> Maybe (a, String))
  deriving (Functor)

runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parser p) s = p s

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  Parser pf <*> Parser pa = Parser $ \s -> case pf s of
    Nothing -> Nothing
    Just (f, s') -> case pa s' of
      Nothing -> Nothing
      Just (a, s'') -> Just (f a, s'')

{-
class Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

  many :: f a -> f [a]
  many p = ((:) <$> p <*> many p) <|> pure []

  some :: f a -> f [a]
  some p = (:) <$> p <*> many p

optional :: Alternative f => f a -> f (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
-}

instance Alternative Parser where
  empty = Parser $ \s -> Nothing
  Parser p <|> Parser q = Parser $ \s -> case p s of
    Just (x, s') -> Just (x, s')
    Nothing -> q s

{-
pure x ~ empty regex
p <*> q ~ pq
p <|> q ~ p|q

p?
many p ~ p*
some p ~ p+
-}

char :: Char -> Parser Char
char c = Parser $ \s -> case s of
  [] -> Nothing
  c':rest
    | c == c' -> Just (c, rest)
    | otherwise -> Nothing

anyChar :: Parser Char
anyChar = Parser $ \s -> case s of
  [] -> Nothing
  c:rest -> Just (c, rest)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  [] -> Nothing
  c:rest
    | p c -> Just (c, rest)
    | otherwise -> Nothing

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), s)
  _ -> Nothing

-- void :: Functor f => f a -> f ()
-- void = fmap (const ())

-- (*>) :: Applicative f => f a -> f b -> f b
-- p *> q = (\x y -> y) <$> p <*> q

-- (a|ab)c
example1 :: Parser ()
example1 = void $ (char 'a' <|> char 'a' *> char 'b') *> char 'c'

instance Monad Parser where
  Parser p >>= f = Parser $ \s -> case p s of
    Nothing -> Nothing
    Just (x, s') -> runParser (f x) s'

-- guard :: Alternative f => Bool -> f ()
-- guard b = if b then pure () else empty

example2 :: Parser [Char]
example2 = do
  c <- anyChar
  guard $ c `elem` ['0'..'9']
  replicateM (digitToInt c) (char 'a')

-- 0|1a|2aa|3aaa|...

data Tree = Leaf | Node Tree Tree
  deriving (Show)

-- (<$) :: Functor f => a -> f b -> f a
-- (<*) :: Applicative f => f a -> f b -> f a

-- ((..).)
parseTree :: Parser Tree
parseTree =
  Leaf <$ char '.' <|>
  Node <$ char '(' <*> parseTree <*> parseTree <* char ')'

data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show)

-- 1 + 2 * 3 = 1 + (2 * 3)

parseDigit :: Parser Int
parseDigit = digitToInt <$> satisfy isDigit

digitsToInt :: [Int] -> Int
digitsToInt = foldl (\n d -> 10 * n + d) 0

parseInt :: Parser Int
parseInt = digitsToInt <$> some parseDigit

parseAtom :: Parser Expr
parseAtom =
  Lit <$> parseInt <|>
  char '(' *> parseExpr <* char ')'

-- parseTerm :: Parser Expr
-- parseTerm =
--   Mul <$> parseTerm <* char '*' <*> parseAtom <|>
--   Div <$> parseTerm <* char '/' <*> parseAtom <|>
--   parseAtom

-- parseExpr :: Parser Expr
-- parseExpr =
--   Add <$> parseExpr <* char '+' <*> parseTerm <|>
--   Sub <$> parseExpr <* char '-' <*> parseTerm <|>
--   parseTerm

choice :: Alternative f => [f a] -> f a
choice ps = foldr (<|>) empty ps

-- choice [x, y] = x <|> y

parseTerm :: Parser Expr
parseTerm = do
  e <- parseAtom
  go e
  where
    go e =
      choice
        [ do
            char '*'
            e' <- parseAtom
            go (Mul e e') -- e -> Mul e e' -> Mul (Mul e e') e''
        , do
            char '/'
            e' <- parseAtom
            go (Div e e')
        , pure e
        ]

parseExpr :: Parser Expr
parseExpr = do
  e <- parseTerm
  go e
  where
    go e =
      choice
        [ do
            char '+'
            e' <- parseAtom
            go (Add e e') -- e -> Mul e e' -> Mul (Mul e e') e''
        , do
            char '-'
            e' <- parseAtom
            go (Sub e e')
        , pure e
        ]

-- Parser a = StateT String Maybe a
type ParserN a = StateT String [] a

runParserN :: ParserN a -> String -> [(a, String)]
runParserN p s = runStateT p s

charN :: Char -> ParserN Char
charN c = do
  s <- get
  case s of
    [] -> empty
    (c':rest)
      | c == c' -> do
        put rest
        return c
      | otherwise -> empty

eofN :: ParserN ()
eofN = do
  s <- get
  if null s then pure () else empty

-- (a|ab)c
example3 :: ParserN ()
example3 = void $ (charN 'a' <|> charN 'a' *> charN 'b') *> charN 'c'
