{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}
module Gy10 where

import Control.Monad
import Control.Applicative
import Data.Char
import Debug.Trace

-- PARSER LIBRARY
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving (Functor)

evalParser :: Parser a -> String -> Maybe a
evalParser pa = (fst <$>) . runParser pa

execParser :: Parser a -> String -> Maybe String
execParser pa = (snd <$>) . runParser pa

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return :: a -> Parser a
  return a = Parser $ \s -> Just (a, s)

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s -> do {(a, s) <- f s; runParser (g a) s}

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (==c)

string :: String -> Parser ()
string s = mapM_ char s

instance Alternative Parser where
  -- mindig hibázó parser
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- választás két parser között
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing -> g s
    res     -> res

-- Control.Applicative-ból:
-- ∙ many  :: Parser a -> Parser [a]
-- ∙ some  :: Parser a -> Parser [a]

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

-- Control.Applicative-ból:
-- ∙ optional :: Parser a -> Parser (Maybe a)

optional_ :: Parser a -> Parser ()
optional_ pa = () <$ optional pa

inList :: [Char] -> Parser Char
inList str = satisfy (`elem` str)

inList_ :: [Char] -> Parser ()
inList_ str = () <$ inList str

------------------------------------------------------------

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
--   pa psep pa .... psep pa
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = do
  a  <- pa
  as <- many (psep *> pa)
  pure (a:as)

-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s -> trace (msg ++ " : " ++ s) (runParser pa s)

-- token/whitespace parsing segédfüggvények

ws :: Parser ()
ws = many_ (satisfy isSpace)

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' f = satisfy f <* ws

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- number parsing

-- intBoolTupleParser :: Parser (Int, Bool)
-- intBoolTupleParser = do
--   char' '('
--   num <- posInt
--   ws
--   char' ','
--   bool <- (True <$ string' "True" <|> False <$ string' "False")
--   char' ')'
--   return (num, bool)
-- intBoolTupleParser = (,) <$> (char' '(' *> posInt <* ws <* char' ',') <*> ((True <$ string' "True" <|> False <$ string' "False") <* char' ')')



pDigit :: Parser Int
pDigit = digitToInt <$> satisfy isDigit

-- Convert a list of digits into a number
digitsToDecimalNumber :: [Int] -> Int
digitsToDecimalNumber digits = foldl (\acc curr -> acc * 10 + curr) 0 digits

digitsToDecimalFraction :: [Int] -> Float
digitsToDecimalFraction digits = foldr (\curr acc -> acc / 10.0 + fromIntegral curr) 0 (digits) / 10
-- [1,2,6,3] -> 0.1263


-- Non-negative decimal integer number
decimalNumber :: Parser Int
decimalNumber = digitsToDecimalNumber <$> some pDigit

-- Non-negative decimal fractional number
decimalFractional :: Parser Float
decimalFractional = do
  i <- decimalNumber
  op <- optional (char '.')
  case op of
    Nothing -> return $ fromIntegral i
    Just _ -> do 
      frac <- (digitsToDecimalFraction <$> (some pDigit))
      return $ (fromIntegral i) + frac


-- Signed decimal fractional number
signedDecimalFractional :: Parser Float
signedDecimalFractional = undefined


-- operátor segédfüggvények

rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f pa psep = foldr1 f <$> sepBy1 pa psep

leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f pa psep = foldl1 f <$> sepBy1 pa psep

nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc f pa psep = do
  exps <- sepBy1 pa psep
  case exps of
    [e]      -> pure e
    [e1,e2]  -> pure (f e1 e2)
    _        -> empty

prefix :: (a -> a) -> Parser a -> Parser op -> Parser a
prefix f pa pop = (pop *> (f <$> pa)) <|> pa


-- Írj egy parser-t, ami zárójeleket, +-t és pozitív Int literálokat tartalmazó
-- kifejezéseket olvas! (Lásd előadás) Whitespace-t mindenhol engedj meg.
--------------------------------------------------------------------------------
--   példák: 10 + 20 + 30
--           (10 + 20) + 30
--           10 + ((20 + 5))
--
-- A + operátor jobbra asszociáljon, azaz pl. 1 + 2 + 3 olvasása legyen
--  (Plus (Lit 1) (Plus (Lit 2) (Lit 3)))

data Exp =
    Lit Int
  | Plus Exp Exp 
  | Mul Exp Exp 
  | Pow Exp Exp 
  | BoolLit Bool
  | Not Exp
  | Eq Exp Exp
  deriving Show

-- 5 +
-- Nem lehet tovább bontani - a legerősebbek a nyelvben
-- Atomok -----------

pBoolLit :: Parser Exp
pBoolLit = BoolLit <$> (True <$ string' "True" <|> False <$ string' "False") <* ws

pLit :: Parser Exp
pLit = Lit <$> decimalNumber <* ws

pParen :: Parser Exp
pParen = char' '(' *> pEq <* char' ')'
--              leggyengébb parser

pAtom :: Parser Exp
pAtom = pLit <|> pParen <|> pBoolLit

-- Operátorok -----------------
pNot :: Parser Exp
pNot = prefix Not pAtom (char' '!')

pPow :: Parser Exp
pPow = rightAssoc Pow pNot (char' '^')

pMul :: Parser Exp
pMul = rightAssoc Mul pPow (char' '*')

pPlus :: Parser Exp
pPlus = rightAssoc Plus pMul (char' '+')

pEq :: Parser Exp
pEq = nonAssoc Eq pPlus (string' "==")

pExp :: Parser Exp
pExp = topLevel pEq
-- leggyengébb parser

data Val = VInt Int | VBool Bool deriving (Show, Eq)

-- Create an evaluator for the expressions defined above!
evalExp :: Exp -> Val
evalExp (Lit i) = VInt i
evalExp (BoolLit b) = VBool b
evalExp (Plus e1 e2) = case (evalExp e1, evalExp e2) of
  (VInt i, VInt i2) -> VInt (i + i2)
  _ -> error "type error"
evalExp (Mul e1 e2) = case (evalExp e1, evalExp e2) of
  (VInt i, VInt i2) -> VInt (i * i2)
  _ -> error "type error"
evalExp (Pow e1 e2) = case (evalExp e1, evalExp e2) of
  (VInt i, VInt i2) -> VInt (i ^ i2)
  _ -> error "type error"
evalExp (Not e1) = case (evalExp e1) of
  VBool b -> VBool (not b)
  _ -> error "type error"
evalExp (Eq e1 e2) = case (evalExp e1, evalExp e2) of
  (VInt i, VInt i2) -> VBool (i == i2)
  (VBool b, VBool b2) -> VBool (b == b2)
  _ -> error "type error"

-- Combine the parse with the evaluator!
evalString :: String -> Val
evalString str = case runParser pExp str of
  Nothing -> error "invalid expression"
  Just (exp, _) -> evalExp exp


-- Extend the parser and the evaluator with the multiplication operator '*',
-- which associates to the right and has stronger precedence, than '+'!

-- Extend the parser and the evaluator with the exponentiation operator '^',
-- which associates to the right and has stronger precedence, than '*'!


--------------------------------------------------------------------------------


-- Írj egy parser-t, ami a következő kifejezéseket olvassa:

{-
data Exp
  = IntLit Int        -- pozitív Int
  | Plus Exp Exp      -- e + e
  | Mul Exp Exp       -- e * e
  | Var String        -- nemüres betűsorozat
  | BoolLit Bool      -- true | false
  | Not Exp           -- not e
  | Eq Exp Exp        -- e == e
   deriving Show

-- Kötési erősségek:
--   - atomok: literálok, változók, zárójelezett kifejezések
--   - not alkalmazás
--   - *     : jobbra asszociál
--   - +     : jobbra asszociál
--   - ==    : nem asszociatív

pExp :: Parser Exp
pExp = undefined

-}
-- bónusz : írj parser-t típusozatlan lambda kalkulushoz! (whitespace megengedett)
--------------------------------------------------------------------------------

-- bemenet-kimenet példák:
--    x           TVar "x"
--    y           TVar "y"
--  f x           App (TVar "f") (TVar "x")
--  f x y         App (App (TVar "f") (TVar "x")) (TVar "y")
--  \x. x         Lam "x" (TVar "x")
--  \x y. x       Lam "x" (Lam "y" (TVar "x"))
--  \x. \y. x     Lam "x" (Lam "y" (TVar "x"))
--  (\x. x) y     App (Lam "x" (TVar "x")) (TVar "y")

data Tm = TVar String | App Tm Tm | Lam String Tm deriving Show

pTm :: Parser Tm
pTm = undefined