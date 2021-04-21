{-# options_ghc -Wincomplete-patterns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Notes19 where

import Data.Char
import Data.List
import Data.Functor
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
char :: Char -> Parser ()
char c = satisfy (== c) $> ()

-- The parser anyChar should succeed if the input string is not empty, and return its first character.
anyChar :: Parser Char
anyChar = satisfy (const True)

-- The parser `string s` should succeed if the input string starts with the string s.
string :: String -> Parser ()
string cs = forM_ cs char

-------------------------------------------------------------------------------

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

-- The parser `some p` and `many p` both try to use the parser p as many times as possible.
--  `many p` always succeeds.                           -- many p       p*
--  `some p` succeeds if the first run of p succeeded.  -- some p       p+

some' :: Alternative f => f a -> f [a]
many' :: Alternative f => f a -> f [a]
-- `some' p` uses p at least 1 times
--   => `some' p` uses p once, and then uses p again any number of times.
some' p = (:) <$> p <*> many' p
-- `many' p` uses p any number of times
--   => `many' p` uses p either (at least 1 times) or doesn't use p.
many' p = some' p <|> pure []

--------------------------------------------------------------------------------

-- The parser digit should parse a single digit between 0 and 9.
digit :: Parser Integer
digit = fromIntegral.digitToInt <$> satisfy isDigit

-- The parser posInt should parse a positive integer
posInt :: Parser Integer
posInt = foldl1 (\a b -> a*10+b) <$> some' digit

-- The parser int should parse a positive or negative integer
int :: Parser Integer
int = (do char '-'; negate <$> posInt) <|> posInt

-- The parser `space` should parse a single whitespace character.
space :: Parser ()
space = satisfy isSpace $> ()

-- The parser `ws` should parse as many whitespace characters as possible.
ws :: Parser ()
ws = many space $> ()

--------------------------------------------------------------------------------

-- `sepBy1 p sep` parses the regular expression p (sep p)*, 
--   i.e. any sequence of the form
--     p 
--     p sep p
--     p sep p sep p
--     ...
-- It fails if it cannot parse p at least once.  
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- sepBy p sep parses either any sequence of the form
--     p 
--     p sep p
--     ...
--   or returns the empty list.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-------------------------------------------------------------------------------
-- Helpers to handle left/right associative binary operators:

--    1 + 6 + 2 + 6
-- infixLeft int (char '+') (+)   -> (((1 + 6) + 2) + 6)
-- infixRight int (char '+') (+)  -> (1 + (6 + (2 + 6)))

-- We often want left associativity
--   1 + 2 - 7 + 5

infixLeft :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixLeft pa psep combine = foldl1 combine <$> sepBy1 pa psep

infixRight :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixRight pa psep combine = foldr1 combine <$> sepBy1 pa psep

infixNonAssoc :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixNonAssoc pa psep combine = do
  exps <- sepBy1 pa psep
  case exps of
    [exp]        -> pure exp                  -- 1 db pa kifejezés
    [exp1, exp2] -> pure $ combine exp1 exp2  -- exp1 `psep` exp2
    _            -> empty                     -- exp1 `psep` exp2 `psep` exp3 ... expN

-------------------------------------------------------------------------------

-- Helpers to handle whitespace:
char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

int' :: Parser Integer
int' = int <* ws

parens :: Parser a -> Parser a
parens p = char' '(' *> p <* char' ')'

-------------------------------------------------------------------------------

-- A parser for boolean expressions, with and (&&) and or (||).
--   (&&) has a higher precedence than (||): 
--      (x && y || z) == ((x && y) || z)

data BoolExpr = Value Bool
              | Or    BoolExpr BoolExpr
              | And   BoolExpr BoolExpr
              deriving (Eq, Ord, Show)

pBool :: Parser Bool
pBool = (string' "True" $> True) 
    <|> (do string' "False"; pure False)

-- a value or an expression inside parentheses
pAtom :: Parser BoolExpr
pAtom = parens pBoolExpr      -- an expression inside parentheses 
    <|> (Value <$> pBool)     -- a boolean constant

pAnd :: Parser BoolExpr
pAnd = infixRight pAtom (string' "&&") And

pOr :: Parser BoolExpr
pOr = infixRight pAnd (string' "||") Or

pBoolExpr :: Parser BoolExpr
pBoolExpr = pOr

-- Examples:
--  runParser pBoolExpr  "((((True))))"  
--    == Value True
--  runParser pBoolExpr  "True && False && False && True"   
--    == ((Value True `And` Value False) `And` Value False) `And` Value True
--  runParser pBoolExpr  "True || False || False || True"   
--    == ((Value True `Or` Value False) `Or` Value False) `Or` Value True
--  runParser pBoolExpr  "True && False || False && True"  
--    == (Value True `And` Value False) `Or` (Value False `And` Value True)
--  runParser pBoolExpr  "True && (False || False) && True" 
--    == Value True `And` (Value False `Or` Value False) `And` Value True

-------------------------------------------------------------------------------

keywords :: [String]
keywords = ["let", "in", "where"]

pKeyword :: String -> Parser ()
pKeyword kw = do
  c <- satisfy isAlpha
  cs <- many (satisfy isAlphaNum)
  let s = c:cs
  guard (s == kw)
  ws

pIdent :: Parser String
pIdent = do
  c <- satisfy isAlpha
  cs <- many (satisfy isAlphaNum)
  let s = c:cs
  guard (not $ s `elem` keywords)
  ws
  pure s

--------------------------------------------------------------------------------

-- A parser for a sublanguage of Haskell.

data Expr = Var String           --   x
          | App Expr Expr        --   u v                 (left associative)
          | Let String Expr Expr --   let x = u in v
          | Lam String Expr      --   \ x -> u
          deriving(Show, Ord, Eq)

-- let x = z in y -> ...

pHAtom :: Parser Expr
pHAtom = parens pExpr
     <|> (Var <$> pIdent)

pApps :: Parser Expr
pApps = foldl1 App <$> some pHAtom

pLet :: Parser Expr 
pLet = do
  pKeyword "let"
  x <- pIdent
  string' "="
  u <- pExpr
  pKeyword "in"
  v <- pExpr
  pure $ Let x u v

pLam :: Parser Expr 
pLam = do
  char' '\\'
  x <- pIdent
  string' "->"
  u <- pExpr
  pure $ Lam x u

pExpr :: Parser Expr
pExpr = pLam
    <|> pLet
    <|> pApps


-- Examples:
--   runParser pExpr "x y z" 
--     ~~> App (App (Var "x") (Var "y")) (Var "z")
--   runParser pExpr "\x -> x" 
--     ~~> Lam "x" (Var "x")
--   runParser pExpr "\x -> x x" 
--     ~~> Lam "x" (App (Var "x") (Var "x"))
--   runParser pExpr "let x = \y -> z y in x" 
--     ~~> Let "x" (Lam "y" (App (Var "z") (Var "y")))
