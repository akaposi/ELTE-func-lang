module Ea10 where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Applicative
import Data.Char
import Control.Monad

type Parser a = StateT String (Except String) a

-- Functor
-- Applicative
-- Monad

-- Alternative

{-
-- monoid
class Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

optional :: f a -> f (Maybe a)
optional p = Just <$> p <|> pure Nothing

many :: f a -> f [a]
many p = some p <|> pure []

some :: f a -> f [a]
some p = liftA2 (:) p (many p)
-}

{-
regex:
pure _ ~ ε -- accepts only empty string
a <*> b ~ ab -- accept a then b
a <|> b ~ a|b

optional a ~ a?
many a ~ a*
some a ~ a+
-}

runParser :: Parser a -> String -> Either String a
runParser p s = runExcept (fst <$> runStateT p s)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  s <- get
  case s of
    "" -> throwError "end of file"
    c:rest
      | p c -> do
        put rest
        return c
      | otherwise -> throwError "char not satisfied"

char :: Char -> Parser Char
char c = satisfy (== c)

eof :: Parser ()
eof = do
  s <- get
  if null s then return () else throwError "not end of file"

example1 :: Parser Int
example1 = do
  as <- many (char 'a')
  b <- char 'b'
  eof
  return $ length as

-- (<*) :: Applicative f => f a -> f b -> f a
-- f <* g = liftA2 (\x _ -> x) f g

example1' :: Parser Int
example1' = length <$> many (char 'a') <* char 'b' <* eof
-- a*b

-- (<$) :: Functor f => a -> f b -> f a
-- a <$ f = fmap (\_ -> a) f

example2 :: Parser ()
example2 = () <$ (char 'a' <|> char 'a' <* char 'b') <* char 'c'
-- (a|ab)c

-- runParser example2 "abc" == Left _

example2' :: Parser ()
example2' = () <$ (char 'a' <* char 'b' <|> char 'a') <* char 'c'

-- type NParser a = StateT String [] a







data Op = Add | Sub | Mul | Div
  deriving (Show)

data Expr
  = Lit Int
  | Op Op Expr Expr
  deriving (Show)

-- 1+2*3+4 -> Op Add (Op Add (Op Mul (Lit 2) (Lit 3))) (Lit 4)
exampleExpr :: String
exampleExpr = "1+2*3+4"

space :: Parser ()
space = () <$ satisfy isSpace

token :: Parser a -> Parser a
token p = p <* many space

parseDigit :: Parser Int
parseDigit = digitToInt <$> satisfy isDigit

digitsToInt :: [Int] -> Int
digitsToInt digits = foldl (\r d -> 10 * r + d) 0 digits

parseLit :: Parser Int
parseLit = token $ digitsToInt <$> some parseDigit

-- 1 * 2 + 3 * 4 + 5 * 6

parseAtom :: Parser Expr
parseAtom =
  Lit <$> parseLit <|>
  token (char '(') *> parseExpr <* token (char ')')

-- parseTerm :: Parser Expr
-- parseTerm =
--   Op Mul <$> parseTerm <* char '*' <*> parseTerm <|>
--   Op Div <$> parseTerm <* char '/' <*> parseTerm <|>
--   parseAtom

-- parseExpr :: Parser Expr
-- parseExpr =
--   Op Add <$> parseExpr <* char '+' <*> parseExpr <|>
--   Op Sub <$> parseExpr <* char '-' <*> parseExpr <|>
--   parseTerm

-- parseTerm :: Parser Expr
-- parseTerm =
--   Op Mul <$> parseAtom <* char '*' <*> parseTerm <|>
--   Op Div <$> parseAtom <* char '/' <*> parseTerm <|>
--   parseAtom

-- parseExpr :: Parser Expr
-- parseExpr =
--   Op Add <$> parseTerm <* char '+' <*> parseExpr <|>
--   Op Sub <$> parseTerm <* char '-' <*> parseExpr <|>
--   parseTerm

parseTerm :: Parser Expr
parseTerm = do
  x <- parseAtom
  go x
  where
    go :: Expr -> Parser Expr
    go x =
      (do
        op <- token $ Mul <$ char '*' <|> Div <$ char '/'
        y <- parseAtom
        go (Op op x y))
      <|>
      return x

parseExpr :: Parser Expr
parseExpr = do
  x <- parseTerm
  go x
  where
    go :: Expr -> Parser Expr
    go x =
      (do
        op <- token $ Add <$ char '+' <|> Sub <$ char '-'
        y <- parseTerm
        go (Op op x y))
      <|>
      return x

exprParser :: Parser Expr
exprParser = space *> parseExpr <* eof



example3 :: Parser String
example3 = do
  n <- parseLit
  replicateM n (char 'a')

-- 0|1a|2aa|3aaa|...



-- type IParser a = StateT (String, Int, Int) (Except String) a
type IParser a = ReaderT Int (StateT (String, Int) (Except String)) a

runParserI :: IParser a -> String -> Either String a
runParserI p s = runExcept (fst <$> runStateT (runReaderT p 0) (s, 0))

satisfyI :: (Char -> Bool) -> IParser Char
satisfyI p = do
  (s, col) <- get
  case s of
    "" -> throwError "end of file"
    c:rest
      | p c -> do
        put (rest, if c == '\n' then 0 else col + 1)
        return c
      | otherwise -> throwError "char not satisfied"

charI :: Char -> IParser Char
charI c = satisfyI (== c)

stringI :: String -> IParser String
stringI s = mapM charI s

eofI :: IParser ()
eofI = do
  (s, _) <- get
  if null s then return () else throwError "not end of file"

getColumn :: IParser Int
getColumn = (\(_, col) -> col) <$> get


data Stmt
  = Assign String Int
  | If String [Stmt]
  deriving (Show)

spaceI :: IParser ()
spaceI = () <$ satisfyI isSpace

tokenI :: IParser a -> IParser a
tokenI p = p <* many spaceI

parseDigitI :: IParser Int
parseDigitI = digitToInt <$> satisfyI isDigit

parseNum :: IParser Int
parseNum = tokenI $ digitsToInt <$> some parseDigitI

{-
x = 1
y = 2
if x:
  y = 3
  if y:
    x = 6
  z = 4
z = 5
-}

parseVar :: IParser String
parseVar = tokenI $ many (satisfyI isLetter)

parseAssign :: IParser Stmt
parseAssign = Assign <$> parseVar <* tokenI (charI '=') <*> parseNum

parseIf :: IParser Stmt
parseIf = do
  _ <- tokenI (stringI "if")
  x <- parseVar
  _ <- tokenI (charI ':')
  col <- getColumn
  i <- ask
  if col > i
    then do
      stmts <- local (\_ -> col) $ many parseStmt
      return $ If x stmts
    else throwError "identation error"

parseStmt :: IParser Stmt
parseStmt = do
  col <- getColumn
  i <- ask
  if col == i
    then parseAssign <|> parseIf
    else throwError "indentation error"

exampleProg :: String
exampleProg =
  unlines
    [ "x = 1"
    , "y = 2"
    , "if x:"
    , "  y = 3"
    , "  if y:"
    , "    x = 6"
    , "  z = 4"
    , "z = 5"
    ]
