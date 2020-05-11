module Notes11 where

import Data.Functor
import Control.Monad
import Control.Applicative
import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser g) = Parser $ \s ->
       fmap (\(a, s') -> (f a, s')) (g s)

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing     -> Nothing
      Just(a, s') -> runParser (g a) s'

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (== c)

string :: String -> Parser ()
string = mapM_ char

--

-- some :: Parser a -> Parser [a]
--   some p  tries to use the parser p as many times as possible.
--           succeeds if p succeeded at least once.
-- many :: Parser a -> Parser [a]
--   many p  tries to use the parser p as many times as possible.
--           always succeeds.

-- ws : parses any sequence (possibly empty) of whitespaces
ws :: Parser ()
ws = () <$ many (satisfy isSpace)
-- Tests:
--   runParser ws "AAAA" == Just ((), "AAAA")
--   runParser ws " AAAA" == Just ((), "AAAA")
--   runParser ws "\n \t AAAA" == Just ((), "AAAA")

-- ident : parses a nonempty sequence of characters in ['a'..'z']
ident :: Parser String
ident = some (satisfy isLower)
-- Tests:
--   runParser ident " AAAA" == Nothing
--   runParser ident "" == Nothing
--   runParser ident "123" == Nothing
--   runParser ident "abc123" == Just ("abc", "123")
--   runParser ident "abc def" == Just ("abc", " def")

digit :: Parser Integer
digit = fromIntegral . digitToInt <$> satisfy isDigit
-- Tests:
--   runParser digit "" == Nothing
--   runParser digit "A1" == Nothing
--   runParser digit "a2" == Nothing
--   runParser digit "9" == Just (9, "")
--   runParser digit "123" == Just (1, "23")
--   runParser digit "1 23" == Just (1, " 23")
--   runParser digit "1a" == Just (1, "a")

int :: Parser Integer
int = foldl (\a b -> 10 * a + b) 0 <$> some digit
-- Tests:
--   runParser int "" == Nothing
--   runParser int "A1" == Nothing
--   runParser int "a2" == Nothing
--   runParser int "9" == Just (9, "")
--   runParser int "123" == Just (123, "")
--   runParser int "1 23" == Just (1, " 23")
--   runParser int "1a" == Just (1, "a")

a0 :: String
a0 = "abc=10"

a1 :: String
a1 = "def=42"

pAssign :: Parser (String, Integer)
pAssign = do
  s <- ident
  ws *> char '='
  i <- ws *> int
  pure (s, i)
-- Tests:
--   runParser pAssign "abc" == Nothing
--   runParser pAssign "abc=" == Nothing
--   runParser pAssign "=0" == Nothing
--   runParser pAssign "abc3" == Nothing
--   runParser pAssign "abc=10AAAA" == Just (("abc", 10), "AAAA")
--   runParser pAssign "def=42" == Just (("def", 42), "")

-- sepBy p sep parses zero or more occurrences of p, separated by sep. Returns a list of values returned by p.
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []
-- Tests:
--   runParser (sepBy int (char ',')) "" == Just ([], "")
--   runParser (sepBy int (char ',')) "1" == Just ([1], "")
--   runParser (sepBy int (char ',')) "1,2" == Just ([1,2], "")
--   runParser (sepBy int (char ',')) "1,2,3" == Just ([1,2,3], "")

-- sepBy1 p sep parses one or more occurrences of p, separated by sep. Returns a list of values returned by p.
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> ((sep *> sepBy1 p sep) <|> pure [])
-- Tests:
--   runParser (sepBy1 int (char ',')) "" == Nothing
--   runParser (sepBy1 int (char ',')) "1" == Just ([1], "")
--   runParser (sepBy1 int (char ',')) "1,2" == Just ([1,2], "")
--   runParser (sepBy1 int (char ',')) "1,2,3" == Just ([1,2,3], "")

pFile :: Parser [(String, Integer)]
pFile = do
  ws
  xs <- sepBy (ws *> pAssign) (ws *> char ';')
  ws
  eof
  pure xs

ex0 :: String
ex0 = ""

ex1 :: String
ex1 = "abc=10;def=42;gh=0"

ex2 :: String
ex2 = " abc = 10;  \n\t  def = 42; gh = 0\n "

ex3 :: String
ex3 = "test = 10; test = 20; abc = 10 "

--------------------------------------------------------------------------------

data Expr = Plus Expr Expr
          | Val Integer
          | Var String
          deriving (Show)

expr0 = "(x + y)"
expr1 = "(x + 2)"
expr2 = "(x + 2) + 3"
expr3 = "x + 5 + y"
expr4 = "xyz"
expr5 = "78"

pParens :: Parser a -> Parser a
pParens p = do
  char '('
  a <- p
  ws
  char ')'
  pure a

pExpr :: Parser Expr
pExpr = foldr1 Plus <$> sepBy1 pExpr0 (ws *> char '+')

pExpr0 :: Parser Expr
pExpr0 = ws *> (pParens pExpr
                <|> (Val <$> int)
                <|> (Var <$> ident))
