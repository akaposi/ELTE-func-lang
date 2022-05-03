{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}
module Gy11 where

import Control.Monad
import Control.Applicative
import Data.Char
import Data.Maybe
import Debug.Trace

-- State
--------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))
  (>>=) (State f) g = State $ \s -> case f s of
    (a, s) -> runState (g a) s

put :: s -> State s ()
put s = State $ \_ -> ((), s)

get :: State s s
get = State $ \s -> (s, s)

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState sta s = fst (runState sta s)

execState :: State s a -> s -> s
execState sta s = snd (runState sta s)

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
prefix f pa pop = f <$> (pop *> prefix f pa pop) <|> pa

{- Task: Boolean expressions -}

-- Write a parser that reads simple boolean expressions containing
-- literals ("T" or "F"), parentheses, conjunction ("v") and disjunction ("^")!
--
-- Conjunction and disjuction should both associate to the right and
-- conjunction should have higher precedence.

-- Új operátor esetén:
-- - Konstruktort felvenni
-- - Parser-t megírni (a többi parser-t igazítani)
-- - Evaluátorban mintát felvenni

data BExp
  = BLit Bool      -- "T" or "F"
  | BAnd BExp BExp -- "l ^ r"
  | BOr BExp BExp  -- "l v r"
  | BNot BExp      -- "!b"
  | BEq BExp BExp  -- "l == r"
  deriving (Show)

bLit :: Parser BExp
bLit = BLit <$> (True <$ char' 'T' <|> False <$ char' 'F') <* ws

bPar :: Parser BExp
bPar = char' '(' *> bEq <* char' ')'

bAtom :: Parser BExp
bAtom = bPar <|> bLit

bNot :: Parser BExp
bNot = prefix BNot bAtom (char' '!')
-- bNot = BNot <$> (char' '!' *> bNot) <|> bAtom

bAnd :: Parser BExp
bAnd = rightAssoc BAnd bNot (char' '^')

bOr :: Parser BExp
bOr = rightAssoc BOr bAnd (char' 'v')

bEq :: Parser BExp
bEq = nonAssoc BEq bOr (string' "==")

bExp :: Parser BExp
bExp = topLevel bEq

evalBExp :: BExp -> Bool
evalBExp (BLit lit) = lit
evalBExp (BAnd l r) = evalBExp l && evalBExp r
evalBExp (BOr l r) = evalBExp l || evalBExp r
evalBExp (BNot b) = not $ evalBExp b
evalBExp (BEq l r) = evalBExp l == evalBExp r

evalString :: String -> Bool
evalString = evalBExp . fromJust . evalParser bExp

-- Tests:
-- ∙ evalString "F"             == False
-- ∙ evalString "(T)"           == True
-- ∙ evalString "T v T ^ F"     == True
-- ∙ evalString "(T v T) ^ F"   == False
-- ∙ evalString "T v F ^ F v T" == True
-- ∙ evalString "T ^ F v F ^ T" == False
-- ∙ evalString "T ^ T ^ F ^ T" == False
-- ∙ evalString "T v F v T v T" == True

-- Extend the boolean expression language with the following:
-- ∙ Prefix negation operator ("!"), which has the highest precedence
-- ∙ Infix equality check operator ("=="), which has the lowest precedence
--   and is not associative

-- Tests:
-- ∙ evalString "!F"               == True
-- ∙ evalString "F v T == F"       == False
-- ∙ evalString "(T v T) ^ !F"     == True
-- ∙ evalString "T v F == F v T"   == True
-- ∙ evalString "T ^ !F == !T v F" == False


{- While language -}

-- Extend the language with:
-- ∙ String literals ("asdf")
-- ∙ Concatenation operator (++)
-- ∙ Print command (and corresponding output)

data Exp =
    IntLit Int          -- int literál (pozitív)
  | Add Exp Exp         -- e + e
  | Sub Exp Exp         -- e - e
  | Mul Exp Exp         -- e * e
  | BoolLit Bool        -- true|false
  | And Exp Exp         -- e && e
  | Or Exp Exp          -- e || e
  | Not Exp             -- not e
  | Eq Exp Exp          -- e == e
  | StringLit String    -- "asdf"
  | Concat Exp Exp      -- l ++ r
  | Var String          -- (változónév)

  | Pair Exp Exp
  | Fst Exp
  | Snd Exp
  deriving (Eq, Show)

posInt' :: Parser Int
posInt' = do
  digits <- some (satisfy isDigit)
  ws
  pure (read digits)

-- változónév/kulcsszó olvasás

-- kulcsszavak:
keywords :: [String]
keywords = ["not", "true", "false", "while", "if", "do", "end", "then", "else", "fst", "snd", "print"]

ident' :: Parser String
ident' = do
  x <- some (satisfy isAlpha) <* ws
  if elem x keywords
    then empty
    else pure x

keyword' :: String -> Parser ()
keyword' str = do
  x <- some (satisfy isAlpha) <* ws
  if x == str
    then pure ()
    else empty

pPair :: Exp -> Parser Exp
pPair e1 = do
  char' ','
  e2 <- eqExp
  char' ')'
  pure (Pair e1 e2)

pPairOrParens :: Parser Exp
pPairOrParens = do
  char' '('
  e1 <- eqExp
  pPair e1 <|> (e1 <$ char' ')')

pString :: Parser String
pString = char' '"' *> many (satisfy isAlpha) <* char' '"'

atom :: Parser Exp
atom =
        (Var <$> ident')
    <|> (IntLit <$> posInt')
    <|> (BoolLit True <$ keyword' "true")
    <|> (BoolLit False <$ keyword' "false")
    <|> (StringLit <$> pString)
    <|> pPairOrParens

pNotFstSnd :: Parser Exp
pNotFstSnd =  (keyword' "not" *> (Not <$> atom))
          <|> (keyword' "fst" *> (Fst <$> atom))
          <|> (keyword' "snd" *> (Snd <$> atom))
          <|> atom

mulExp :: Parser Exp
mulExp = rightAssoc Mul pNotFstSnd (char' '*')

addExp :: Parser Exp
addExp = rightAssoc Add mulExp (char' '+')

subExp :: Parser Exp
subExp = rightAssoc Sub addExp (char' '-')

andExp :: Parser Exp
andExp = rightAssoc And subExp (string' "&&")

orExp :: Parser Exp
orExp = rightAssoc Or andExp (string' "||")

concatExp :: Parser Exp
concatExp = rightAssoc Concat orExp (string' "++")

eqExp :: Parser Exp
eqExp = nonAssoc Eq concatExp (string' "==")

pExp :: Parser Exp
pExp = eqExp

data Val = VInt Int | VBool Bool | VPair Val Val | VString String
  deriving (Eq, Show)

type Env = [(String, Val)]

evalExp :: Env -> Exp -> Val
evalExp env e = case e of
  IntLit n  -> VInt n
  BoolLit b -> VBool b
  StringLit s -> VString s
  Add e1 e2 -> case (evalExp env e1, evalExp env e2) of
    (VInt n1, VInt n2) -> VInt (n1 + n2)
    _                  -> error "type error"
  Sub e1 e2 -> case (evalExp env e1, evalExp env e2) of
    (VInt n1, VInt n2) -> VInt (n1 - n2)
    _                  -> error "type error"
  Mul e1 e2 -> case (evalExp env e1, evalExp env e2) of
    (VInt n1, VInt n2) -> VInt (n1 * n2)
    _                  -> error "type error"
  Or e1 e2 -> case (evalExp env e1, evalExp env e2) of
    (VBool b1, VBool b2) -> VBool (b1 || b2)
    _                    -> error "type error"
  And e1 e2 -> case (evalExp env e1, evalExp env e2) of
    (VBool b1, VBool b2) -> VBool (b1 && b2)
    _                    -> error "type error"
  Concat e1 e2 -> case (evalExp env e1, evalExp env e2) of
    (VString s1, VString s2) -> VString (s1 ++ s2)
    _                        -> error "type error"
  Eq e1 e2 -> case (evalExp env e1, evalExp env e2) of
    (VBool b1, VBool b2) -> VBool (b1 == b2)
    (VInt n1,  VInt n2 ) -> VBool (n1 == n2)
    (VPair fs1 sn1, VPair fs2 sn2) -> VBool (fs1 == fs2 && sn1 == sn2)
    _                    -> error "type error"
  Not e -> case evalExp env e of
    VBool b -> VBool (not b)
    _       -> error "type error"
  Var x -> case lookup x env of
    Just v  -> v
    Nothing -> error $ "name not in scope: " ++ x

  Pair e1 e2 -> VPair (evalExp env e1) (evalExp env e2)

  Fst e -> case evalExp env e of
    VPair v1 v2 -> v1
    _           -> error "type error"

  Snd e -> case evalExp env e of
    VPair v1 v2 -> v2
    _           -> error "type error"

--------------------------------------------------------------------------------

type Program = [Statement]  -- st1; st2; st3; ... st4

data Statement
  = Assign String Exp       -- x := e
  | While Exp Program       -- while e do prog end
  | If Exp Program Program  -- if e then prog1 else prog2 end
  | Print Exp               -- print exp end
  deriving (Eq, Show)

statement :: Parser Statement
statement =
        (Assign <$> ident'
                <*> (string' ":=" *> pExp))
    <|> (While <$> (keyword' "while" *> pExp <* keyword' "do")
               <*> (program <* keyword' "end"))
    <|> (If <$> (keyword' "if" *> pExp <* keyword' "then")
            <*> (program <* keyword' "else")
            <*> (program <* keyword' "end"))
    <|> (Print <$> (keyword' "print" *> pExp))

program :: Parser Program
program = sepBy statement (char' ';')

updateEnv :: String -> Val -> Env -> Env
updateEnv x v [] = [(x, v)]
updateEnv x v ((x', v'):env)
  | x == x'   = (x', v):env
  | otherwise = (x', v') : updateEnv x v env

inNewScope :: State (Env, String) a -> State (Env, String) a
inNewScope ma = do
  (env, _) <- get
  let len = length env
  a <- ma
  (env', output) <- get
  put $ (take len env', output)
  pure a

evalStatement :: Statement -> State (Env, String) ()
evalStatement st = case st of

  Assign x e -> do
    (env, output) <- get
    let val = evalExp env e
    put $ (updateEnv x val env, output)

  While e p -> do
    (env, _) <- get
    case evalExp env e of
      VBool True  -> inNewScope (evalProgram p) >> evalStatement (While e p)
      VBool False -> pure ()
      VInt _      -> error "type error"
      VPair _ _   -> error "type error"
      VString _   -> error "type error"

  If e p1 p2 -> do
    (env, _) <- get
    case evalExp env e of
      VBool True  -> inNewScope (evalProgram p1)
      VBool False -> inNewScope (evalProgram p2)
      VInt _      -> error "type error"
      VPair _ _   -> error "type error"
      VString _   -> error "type error"

  Print e -> do
    (env, output) <- get
    case evalExp env e of
      VString s -> put (env, output ++ s)
      _         -> error "type error"


evalProgram :: Program -> State (Env, String) ()
evalProgram = mapM_ evalStatement

run :: String -> (Env, String)
run str = case runParser (topLevel program) str of
  Just (prog, _) -> execState (evalProgram prog) ([], "")
  Nothing        -> error "parse error"

p1 :: String
p1 = "i := 10; acc := 0; while not (i == 0) do acc := acc + i; i := i - 1 end"

p1' :: String
p1' = "i := 10; acc := \"\"; while not (i == 0) do acc := acc ++ \"foo\"; i := i - 1 end"

p2 :: String
p2 = "x := (10, 20); y := fst x; z := snd x"

p3 :: String
p3 = "x := ((10, true), 20); y := fst (fst x)"

p4 :: String
p4 = unlines
  [ "x := 10;"
  , "y := \"nyolc\";"
  , "while not (x == 5) do"
  , "  if x == 8 then print y else print \"other\" end;"
  , "  x := x - 1"
  , "end"
  ]
