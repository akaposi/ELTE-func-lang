{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

--------------------------------------------------------------------------------

import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Applicative
import Data.Char 

import Debug.Trace

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving(Functor)

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return :: a -> Parser a
  return a = Parser $ \s -> Just (a, s)

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing      -> Nothing
    Just (a, s') -> runParser (g a) s'

instance Alternative Parser where
  -- The empty parser always fails.
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- The parser (p1 <|> p2) first tries to run the parser p1. 
  --   If p1 fails, the parser p2 is used instead.
  (<|>) :: Parser a -> Parser a -> Parser a
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- The parser `satisfy p` accepts any character that satisfies the predicate p.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s -> trace (msg ++ " : " ++ s) (runParser pa s)

-- The parser `char c` recognizes exactly the character c.
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

alpha :: Parser Char
alpha = satisfy (\c -> 'a' <= c && c <= 'z')

-- The parser anyChar accepts any character.
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

-- The parser `string s` recognizes exactly the string s.
string :: String -> Parser ()
string = traverse_ char

-- In Control.Applicative:

-- many :: Parser a -> Parser [a]
--   `many p` tries to run the parser p as many times as possible.

-- some :: Parser a -> Parser [a]
--   `some p` tries to run the parser p as many times as possible.
--   The parser p should suceed at least once.

many' :: Parser a -> Parser [a]
many' p = some' p <|> pure []

some' :: Parser a -> Parser [a]
some' p = do x <- p; xs <- many' p; pure (x:xs)

-- `sepBy pa psep` runs the parser pa as many times as possible, 
--   separated by the parser psep.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- `sepBy pa psep` runs the parser pa as many times as possible, 
--   separated by the parser psep.
--   The parser pa should suceed at least once.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

-------------------------------------------------------------------------------

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

posInt :: Parser Int
posInt = fmap (foldl' (\x d -> 10*x+d) 0) (some digit)

negInt :: Parser Int
negInt = char '-' *> (negate <$> posInt)

int :: Parser Int
int = posInt <|> negInt

--------------------------------------------------------------------------------

ws :: Parser ()
ws = do many (satisfy isSpace); pure ()

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

keywords :: [String]
keywords = [ "if", "then", "else", "while", "do", "end"
           , "true", "false"
           , "let", "in"
           ]

pKeyword :: String -> Parser ()
pKeyword str = do
  string str
  (do satisfy isLetter; empty) <|> ws

pIdent :: Parser String
pIdent = do
  x <- some (satisfy isLetter) <* ws
  if x `elem` keywords then empty
                       else pure x

--------------------------------------------------------------------------------
-- Parser for a small imperative language

data Exp
  = IntLit Int    -- integer literals
  | BoolLit Bool  -- boolean literals
  | Add Exp Exp   -- e1 + e2, left associative
  | Mul Exp Exp   -- e1 * e2, left associative
  | Eq  Exp Exp   -- e1 == e2, not associative
  | Or Exp Exp    -- e1 || e2, right associative
  | And Exp Exp   -- e1 && e2, right associative
  | Var String    -- variables
  deriving (Eq, Show)

type Program = [Statement]

data Statement
  = Assign String Exp               -- x := exp
  | IfThenElse Exp Program Program  -- if e1 then p1 else p2 end
  | While Exp Program               -- while e do p end
  deriving (Eq, Show)

-- precedence levels for expressions:
--  variables, literals, parentheses
--  *, left associative
--  +, left associative
--  ==, not associative
--  &&, right associative
--  ||, right associative

posInt' :: Parser Int
posInt' = posInt <* ws

bool :: Parser Bool
bool = (pKeyword "true" *> pure True) 
   <|> (pKeyword "false" *> pure False)

pAtom :: Parser Exp
pAtom = (char' '(' *> pExp <* char' ')')
    <|> (IntLit <$> posInt')
    <|> (BoolLit <$> bool)
    <|> (Var <$> pIdent)

pMul :: Parser Exp
pMul = foldl1 Mul <$> sepBy1 pAtom (char' '*')

pAdd :: Parser Exp
pAdd = foldl1 Add <$> sepBy1 pMul (char' '+')

pEq :: Parser Exp
pEq = undefined

pAnd :: Parser Exp
pAnd = undefined

pOr :: Parser Exp
pOr = foldr1 Or <$> sepBy1 pAnd (string' "||")

pExp :: Parser Exp
pExp = pOr

pStatement :: Parser Statement 
pStatement = undefined 

pProgram :: Parser Program
pProgram = sepBy1 pStatement (char' ';')

pTop :: Parser Program
pTop = ws *> pProgram <* eof

prog1 = "x := 0; while x == 0 || x == 1 do x := x + 1 end"
prog2 = "x := 0; while true do if x == 0 then x := 1 else x := 0 end end"

--------------------------------------------------------------------------------
-- Parser for a small Haskell-like functional language

data Exp2
  = Var2 String                 -- variables
  | Lambda2 String Exp2         -- \x -> e
  | App2 Exp2 Exp2              -- f t
  | Let2 String Exp2 Exp2       -- let x = u in t
  -- IfThenElse Exp2 Exp2 Exp2  -- if b then t else f
  deriving (Eq, Show)

pAtom2 :: Parser Exp2
pAtom2 = undefined

pApp2 :: Parser Exp2
pApp2 = undefined

pLam2 :: Parser Exp2
pLam2 = undefined

pLet2 :: Parser Exp2
pLet2 = undefined

pExp2 :: Parser Exp2
pExp2 = pLam2 <|> pLet2 <|> pApp2


ex1 = "\\x -> \\y -> x y x"
ex2 = "let x = y in \\z -> x z"
