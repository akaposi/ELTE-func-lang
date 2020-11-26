{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MonadComprehensions #-}
module Notes10 where

import Data.Char
import Data.List
import Control.Applicative
import Control.Monad 

-------------------------------------------------------------------------------

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case p s of
    Just (x, s') -> Just (f x, s')
    Nothing      -> Nothing

instance Applicative Parser where pure = return; (<*>) = ap

instance Monad Parser where
  return x = Parser $ \s -> Just (x, s)
  Parser p >>= f = Parser $ \s -> case p s of
    Just (x, s') -> runParser (f x) s'
    Nothing      -> Nothing

-------------------------------------------------------------------------------

-- The `eof` (end of file) parser.
--  succeeds if the input string is empty, fails otherwise.
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- The parser `satisfy p` succeeds if the input string starts with a character 
--  that satisfies the predicate p (and consumes that character), and fails otherwise.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c       -> Just (c, cs)
       | otherwise -> Nothing
  [] -> Nothing

-------------------------------------------------------------------------------

-- The parser `char c` should succeed if the input string start with the character c.
-- Examples:
--   runParser (char 'a') "" == Nothing
--   runParser (char 'a') "abc" == Just ((), "bc")
--   runParser (char 'a') "bcd" == Nothing
char :: Char -> Parser ()
char c = satisfy (== c) *> pure ()

-- The parser anyChar should succeed if the input string is not empty, and return its first character.
-- Examples:
--   runParser anyChar "" == Nothing
--   runParser anyChar "()" == Just ('(', ")")
--   runParser anyChar "abc" == Just ('a', "bc")
anyChar :: Parser Char
anyChar = satisfy (const True)

-- The parser `string s` should succeed if the input string starts with the string s.
--   runParser (string "abc") "abdef" == Nothing
--   runParser (string "") "abcdef" == Just ((), "abcdef")
--   runParser (string "abc") "abcdef" == Just ((), "def")
string :: String -> Parser ()
string s = forM_ s char

space :: Parser ()
space = satisfy isSpace *> pure ()

ws :: Parser ()
ws = many space *> pure ()

-------------------------------------------------------------------------------

-- class Applicative f => Alternative f where
--   empty :: f a 
--   (<|>) :: f a -> f a -> f a

instance Alternative Parser where
  -- The parser `empty` always fails.
  empty = Parser $ \_ -> Nothing

  -- The parser `p1 <|> p2` tries the parser p1. 
  --  If p1 succeeds, it returns the result of p1.
  --  If p1 fails, then it tries the parser p2 instead.
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-------------------------------------------------------------------------------

-- `sepBy1 p sep` parses the regular expression p (sep p)*, 
--   i.e. any sequence of the form
--     p 
--     p sep p
--     p sep p sep p
--     ...
-- It fails if it cannot parse p at least once.  
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

sepByWithSep1 :: Parser a -> Parser sep -> Parser (a, [(sep, a)])
sepByWithSep1 p sep = (,) <$> p <*> many ((,) <$> sep <*> p)

-- sepBy p sep parses either any sequence of the form
--     p 
--     p sep p
--     ...
--   or returns the empty list.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-------------------------------------------------------------------------------

digit :: Parser Integer
digit = fromIntegral . digitToInt <$> satisfy isDigit

posInt :: Parser Integer
posInt = foldl' (\x y -> 10*x+y) 0 <$> some digit

int :: Parser Integer
int = negInt <|> posInt
  where negInt = char '-' *> (negate <$> posInt)

-------------------------------------------------------------------------------

data IntExpr = Value Integer
             | Plus  IntExpr IntExpr
             | Minus IntExpr IntExpr
             | Times IntExpr IntExpr
             | Div   IntExpr IntExpr
             deriving (Eq, Ord, Show)

-- Parser for expressions built from 
--   integer constants, parentheses
--   *, /    (left associative)
--   +, -    (left associative)      x-y-z = (x-y)-z

parens :: Parser a -> Parser a
parens p = char' '(' *> p <* char' ')'

-- We define several subparsers, to deal with the diffferent precedences of the operators.

-- pValueOrParens should either parse an integer value, 
--   or any expression wrapped in parentheses.
pValueOrParens :: Parser IntExpr
-- pExprTimes should parse any expression built from pValueOrParens, * and /
pExprTimes     :: Parser IntExpr
-- pExprPlus should parse any expression built from pExprTimes, + and -
pExprPlus      :: Parser IntExpr

pValueOrParens = (Value <$> int')
             <|> parens pExprPlus

-- pExprTimes = do
--   (x, ys) <- sepByWithSep1 pValueOrParens ((char '*' *> pure Times) <|> (char '/' *> pure Div))
--   pure $ foldl (\a (f, b) -> f a b) x ys

pExprTimes = do acc <- pValueOrParens; go acc
  where go acc = goTimes acc
             <|> goDiv acc
             <|> pure acc
        goTimes x = do char' '*'; y <- pValueOrParens; go (Times x y)
        goDiv x   = do char' '/'; y <- pValueOrParens; go (Div x y)

pExprPlus = do acc <- pExprTimes; go acc
  where go acc = goPlus acc
             <|> goMinus acc
             <|> pure acc
        goPlus x  = do char' '+'; y <- pExprTimes; go (Plus x y)
        goMinus x = do char' '-'; y <- pExprTimes; go (Minus x y)

-- To handle whitespace (or comments  -- ...)
--   ws = remove all whitespaces and comments from the input.

lexeme :: Parser a -> Parser a
lexeme p = p <* ws

char' :: Char -> Parser ()
char' c = lexeme (char c)

string' :: String -> Parser ()
string' s = lexeme (string s)

int' :: Parser Integer
int' = lexeme int

-- Exercise: 
--  Modify this parser to handle whitespace correctly
pList :: Parser [Integer]
pList = char '[' *> sepBy int (char ',') <* char ']'



-- Keywords and identifiers:
--   keywords in Haskell : data let in where ...

-- p = do
--   string "let"
--   x <- some (satisfy isAlpha)
--   char '='
--   x <- some (satisfy isAlpha)
--   string "in"
--   x <- some (satisfy isAlpha)
--   pure _

-- Now "let inz = 6 in letx = y inz" is accepted by p. We don't want this.
-- "let let = let in let" is also accepted.

keywords :: [String]
keywords = ["let", "in", "where"]

pLetKeyword :: Parser ()
pLetKeyword = lexeme $ do
  s <- some (satisfy isAlpha)
  guard (s == "let")
  return ()

pKeyword :: String -> Parser ()
pKeyword kw = lexeme $ do
  s <- some (satisfy isAlpha)
  guard (s == kw)
  return ()
-- pLetKeyword == pKeyword "let"

pIdent :: Parser String
pIdent = lexeme $ do
  s <- some (satisfy isAlpha)
  -- to allow identifiers with digits, such as x0:
  --     s <- some (satisfy isAlphaNum)
  --     guard (not (isDigit (head s)))
  guard (not $ s `elem` keywords)
  pure s

--------------------------------------------------------------------------------

data Expr = Var String           --   x
          | App Expr Expr        --   u v                 (left associative)
          | Let String Expr Expr --   let x = u in v
          | Lam String Expr      --   \x -> u   
          deriving(Show, Ord, Eq)

-- let x = u in v w
--  /= (let x = u in v) w
--  == let x = u in (v w)

-- let x = let y = 2 in y in x

-- Variables and parentheses
pAtom :: Parser Expr
pAtom = (Var <$> pIdent)
    <|> (parens pExpr)

pApps :: Parser Expr
pApps = foldl1 App <$> some pAtom

pLet :: Parser Expr 
pLet = do
  pKeyword "let"
  x <- pIdent
  char' '='
  u <- pExpr
  pKeyword "in"
  v <- pExpr
  pure (Let x u v)

pLam :: Parser Expr 
pLam = do
  char' '\\'
  x <- pIdent
  string' "->"
  u <- pExpr
  pure (Lam x u)

pExpr :: Parser Expr
pExpr = pLet
    <|> pLam
    <|> pApps

-- Example of a slow parser.
p :: Parser Integer
p = int' <|> parens q

q :: Parser Integer
q =  (do x <- p; char '+'; y <- q; return (x+y))
 <|> (do x <- p; char '-'; y <- q; return (x-y))
 <|> p