{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}
module Gy10 where

import Control.Monad
import Control.Applicative
import Data.Char
import Debug.Trace
import Control.Arrow (ArrowChoice(right))

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

pDigit :: Parser Int
pDigit = digitToInt <$> satisfy isDigit

-- Convert a list of digits into a number
digitsToDecimalNumber :: [Int] -> Int
digitsToDecimalNumber digits = foldl (\acc curr -> acc * 10 + curr) 0 digits


digitsToDecimalFraction :: [Int] -> Double
digitsToDecimalFraction digits = foldr (\curr acc -> acc / 10 + fromInteger curr) 0 (fmap fromIntegral digits) / 10


-- Non-negative decimal integer number
decimalNumber :: Parser Int
decimalNumber = digitsToDecimalNumber <$> some pDigit

-- Non-negative decimal fractional number
-- beolvasunk 1 egész számot, ? . ....
-- fromInteger
decimalFractional :: Parser Double
decimalFractional = do
    int <- decimalNumber
    optional (char '.')
    frac <- optional (digitsToDecimalFraction <$> (some pDigit))
    pure $ case frac of
      Nothing -> fromIntegral int
      Just x -> fromIntegral int + x


-- Signed decimal fractional number
signedDecimalFractional :: Parser Double
signedDecimalFractional = do
    opt <- optional (char '-')
    fracNumber <- decimalFractional
    pure $ case opt of
       Nothing -> fracNumber
       Just _ -> (- fracNumber)
        


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

-- data Exp = Lit Int | Plus Exp Exp deriving Show

-- pExp :: Parser Exp
-- pExp = undefined

-- -- Create an evaluator for the expressions defined above!
-- evalExp :: Exp -> Int
-- evalExp = undefined

-- -- Combine the parse with the evaluator!
-- evalString :: String -> Int
-- evalString = undefined

-- Extend the parser and the evaluator with the multiplication operator '*',
-- which associates to the right and has stronger precedence, than '+'!

-- Extend the parser and the evaluator with the exponentiation operator '^',
-- which associates to the right and has stronger precedence, than '*'!


--------------------------------------------------------------------------------


-- Írj egy parser-t, ami a következő kifejezéseket olvassa:


data Exp
  = IntLit Int        -- pozitív Int  1.
  | Plus Exp Exp      -- e + e        1.
  | Mul Exp Exp       -- e * e
  | Div Exp Exp       -- e / e
  | Var String        -- nemüres betűsorozat
  | BoolLit Bool      -- true | false
  | Not Exp           -- not e
  | Eq Exp Exp        -- e == e
   deriving Show

-- Kötési erősségek:
--   - atomok: literálok, változók, zárójelezett kifejezések
--   - not alkalmazás
--   - *     : jobbra asszociál
--   - /     : jobbra asszociál
--   - +     : jobbra asszociál
--   - ==    : nem asszociatív

-- Atomok
---------------------------

pVar :: Parser Exp
pVar = Var <$> some (satisfy isAlpha) <* ws

pBoolLit :: Parser Exp
pBoolLit = BoolLit <$> (True <$ string "True" <|> False <$ string "False") <* ws

pIntLit :: Parser Exp
pIntLit = IntLit <$> decimalNumber <* ws

pParen :: Parser Exp
pParen = char' '(' *> pEq <* char' ')'
--              a leggyengébb parser

pAtom :: Parser Exp
pAtom = pIntLit <|> pParen <|> pBoolLit <|> pVar

-- Just (  Plus (Plus (IntLit 3) (IntLit 5)) (IntLit 4)  ,"")
-- Operátorok
---------------------------

-- 2. Parser megírása

pNot :: Parser Exp
pNot = prefix Not pAtom (string' "not")

pMul :: Parser Exp
pMul = rightAssoc Mul pNot (char' '*')

pDiv :: Parser Exp
pDiv = rightAssoc Div pMul (char' '/')

pPlus :: Parser Exp
pPlus = rightAssoc Plus pDiv (char' '+') -- 1

pEq :: Parser Exp
pEq = nonAssoc Eq pPlus (string' "==")

pExp :: Parser Exp
pExp = topLevel pEq
-- leggyengébb operátorral kezdjük

data Val = VInt Int | VString String | VBool Bool
  deriving (Show, Eq)

evalExp :: Exp -> Val
evalExp (BoolLit b) = VBool b
evalExp (Var s) = VString s
evalExp (IntLit i) = VInt i
-- evalExp (Plus e1 e2) = evalExp e1 + evalExp e2
evalExp (Plus e1 e2) = case (evalExp e1, evalExp e2) of
  (VInt i, VInt i2) -> VInt $ i + i2
  _ -> error "type error"
evalExp (Mul e1 e2) = case (evalExp e1, evalExp e2) of
  (VInt i, VInt i2) -> VInt $ i * i2
  _ -> error "type error"
evalExp (Div e1 e2) = case (evalExp e1, evalExp e2) of
  (VInt i, VInt i2) -> VInt $ div i i2
  _ -> error "type error"
evalExp (Not e) = case evalExp e of
  VBool b -> VBool (not b)
  _ -> error "type error"
evalExp (Eq e1 e2) = case (evalExp e1, evalExp e2) of
  (val1, val2) -> VBool $ val1 == val2
-- 3.

evalString :: String -> Val
evalString str = case runParser pExp str of
  Nothing -> error "type error"
  Just (e,str) -> evalExp e


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