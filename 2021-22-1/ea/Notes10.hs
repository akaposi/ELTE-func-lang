
{-# language InstanceSigs, DeriveFunctor, DeriveFoldable,
    DeriveTraversable #-}

{-# options_ghc -Wincomplete-patterns #-}

import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Applicative -- many, some
import Data.Char           -- isDigit :: Char -> Bool
                           -- digitToInt :: Char -> Int

import Debug.Trace         -- trace :: String -> a -> a
                           -- traceShow :: Show b => b -> a -> a

import Control.Monad.State

-- PARSER LIBRARY
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where

  -- nem dob hibát + nem fogyaszt inputot
  return :: a -> Parser a
  return a = Parser $ \s -> Just (a, s)

  -- egymás után két parsert hívunk
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing      -> Nothing
    Just (a, s') -> runParser (g a) s'

-- parserek közötti választás
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- üres String olvasása
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- Char olvasása, amire egy feltétel teljesül
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)   -- output String 1-el rövidebb!
  _         -> Nothing

satisfy_ :: (Char -> Bool) -> Parser ()
satisfy_ f = () <$ satisfy f

-- konkrét Char olvasása
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- parser hívásakor kiír egy String üzenetet
debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s -> trace (msg ++ " : " ++ s) (runParser pa s)

-- bármilyen Char olvasása
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

-- konkrét String-et próbál olvasni
string :: String -> Parser ()
string = traverse_ char

-- Control.Applicative-ból (iterálás):
-- many :: Parser a -> Parser [a]        -- 0-szor vagy többször olvasás
-- some :: Parser a -> Parser [a]        -- 1-szer vagy többször olvasás

many_ :: Parser a -> Parser ()
many_ pa = some_ pa <|> pure ()

some_ :: Parser a -> Parser ()
some_ pa = pa >> many_ pa

   -- Functor/Applicative operátorok
   --   (<$)       kicserélni parser végeredményét adott értékre
   --   (<$>)      fmap
   --   (<*)       két parser-t futtat, az első értékét visszaadja
   --   (*>)       két parser-t futtat, a második értékét visszaadja

-- whitespace elfogyasztása
ws :: Parser ()
ws = many_ (satisfy isSpace)

-- Olvassuk pa-t 0-szor vagy többször, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- Olvassuk pa-t 1-szor vagy többször, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

-- egy számjegy olvasása
digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

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
    _            -> empty

-- Értékadás mint új változó felvétel + mutáció
--------------------------------------------------------------------------------

{-
Precedenciák:
  - literál, zárójel
  - *  (jobb asszoc)
  - +  (jobb asszoc)
  - == (nem asszoc)
  - if e1 then e2 else e3   (prefix)
-}

data Exp
  = IntLit Int
  | BoolLit Bool
  | Not Exp               -- not e (alkalmazás erősebb minden operátornál)
  | Add Exp Exp
  | Mul Exp Exp
  | Eq Exp Exp
  | Var String            -- változónév
  | Pair Exp Exp          -- Haskell szintaxis (e1, e2)
  | Fst Exp               -- fst e
  | Snd Exp               -- snd e
  deriving (Eq, Show)

type Program = [Statement]

data Statement
  = Assign String Exp               --   x := exp
  | IfThenElse Exp Program Program  --   if e1 then p1 else p2 end
  | While Exp Program               --   while e do p end
  deriving (Eq, Show)

-- legyen minden statement ;-vel elválasztva
-- cél Program-ot olvassunk


-- 1. token parserek
char' c = char c <* ws
string' s = string s <* ws

posInt' :: Parser Int
posInt' = read <$> (some (satisfy isDigit) <* ws)

-- Azonosítók vs kulcsszavak
--   nem szeretnénk összekeverni kulcszót azonosítóval
--   furcsa : \ if then -> if if then then else 0
--   diszjunkt legyen a két parser (azonosító nem lehet kulcsszó és fordítva)

keywords :: [String]
keywords = ["if", "then", "else", "while", "do", "end", "not", "fst", "snd"]

-- ki akarjuk zárni:   iffoo olvasása: "if" először, utána "foo" mint azonsító
--                     helyett       : "iffoo" egyben mint azonosító
keyword' :: String -> Parser ()
keyword' str = do
  string str
  x <- many (satisfy isLetter) <* ws
  case x of
    "" -> pure ()
    _  -> empty

ident' :: Parser String
ident' = do
  x <- some (satisfy isLetter) <* ws
  if elem x keywords then empty
                     else pure x

-- kifejezések
--------------------------------------------------------------------------------

-- ( e )
-- ( e1, e2 )

parensOrPair :: Parser Exp
parensOrPair = do
  char' '('
  es <- sepBy1 eqExp (char' ',')
  char' ')'
  case es of
    []  -> empty
    [e] -> pure e                 -- zárójelezés
    es  -> pure $ foldr1 Pair es  -- jobbra egymásba ágyazott pár

atom :: Parser Exp
atom = (IntLit <$> posInt')
   <|> parensOrPair
   <|> (BoolLit <$> (True  <$ keyword' "true"))
   <|> (BoolLit <$> (False <$ keyword' "false"))
   <|> (Var <$> ident')

-- primFun = fst, snd, not
primFunExp :: Parser Exp
primFunExp =
      (Not <$> (keyword' "not" *> atom))
  <|> (Fst <$> (keyword' "fst" *> atom))
  <|> (Snd <$> (keyword' "snd" *> atom))
  <|> atom

mulExp :: Parser Exp
mulExp = infixRight primFunExp (char' '*') Mul

addExp :: Parser Exp
addExp = infixRight mulExp (char' '+') Add

eqExp :: Parser Exp
eqExp = infixNonAssoc addExp (string' "==") Eq

-- statement olvasás
--------------------------------------------------------------------------------

pIf     = keyword' "if"
pThen   = keyword' "then"
pElse   = keyword' "else"
pWhile  = keyword' "while"
pDo     = keyword' "do"
pEnd    = keyword' "end"
pAssign = string' ":="

statement :: Parser Statement
statement =
      (Assign <$> (ident' <* pAssign) <*> eqExp)
  <|> (IfThenElse <$> (pIf *> eqExp)
                  <*> (pThen *> program)
                  <*> (pElse *> program <* pEnd))
  <|> (While <$> (pWhile *> eqExp <* pDo)
             <*> (program <* pEnd))

program :: Parser Program
program = sepBy statement (char' ';')

parseProgram :: Parser Program
parseProgram = topLevel program

p1 :: String
p1 = unlines [
  "x := 100;",
  "y := 1000;",
  "while x == 0 do",
    "y := y + 1",
  "end"
  ]

p2 :: String
p2 = unlines [
  "i := 0;",
  "sum := 0;",
  "while not (i == 100) do",
  "  sum := sum + i;",
  "  i := i + 1",
  "end"
  ]


-- Interpreter
--------------------------------------------------------------------------------

data Val = VInt Int | VBool Bool | VPair Val Val
  deriving (Eq, Show)

type Env = [(String, Val)]    -- (ha hatékonyság számít: Env tömb, nevek helyett indexek)

evalExp :: Env -> Exp -> Val
evalExp env exp = case exp of
  IntLit n  -> VInt n
  BoolLit b -> VBool b
  Add e1 e2 -> case (evalExp env e1, evalExp env e2) of
    (VInt n1, VInt n2) -> VInt (n1 + n2)
    _                  -> error "type error"
  Mul e1 e2 -> case (evalExp env e1, evalExp env e2) of
    (VInt n1, VInt n2) -> VInt (n1 * n2)
    _                  -> error "type error"
  Eq e1 e2 -> case (evalExp env e1, evalExp env e2) of
    (VInt n1, VInt n2)   -> VBool (n1 == n2)
    (VBool b1, VBool b2) -> VBool (b1 == b2)
    _                    -> error "type error"

  Var x -> case lookup x env of
    Nothing -> error $ "name not in scope: " ++ x
    Just v  -> v
  Not e -> case evalExp env e of
    VBool b -> VBool (not b)
    _       -> error "type error"
  Pair e1 e2 -> VPair (evalExp env e1) (evalExp env e2)
  Fst e -> case evalExp env e of
    VPair v1 v2 -> v1
    _           -> error "type error"
  Snd e -> case evalExp env e of
    VPair v1 v2 -> v2
    _           -> error "type error"

update :: String -> Val -> Env -> Env
update x v [] = [(x, v)]
update x v ((x', v'):env)
  | x == x'   = (x, v):env
  | otherwise = (x', v') : update x v env

evalStatement :: Statement -> State Env ()
evalStatement st = case st of

  -- értékadás: ha "x" már definiálva van, akkor módosítjuk az értékét
  --            ha még nincs definiálva, akkor vegyük fel a környezetbe
  Assign x exp -> do
    env <- get
    let v = evalExp env exp
    put $ update x v env

  IfThenElse e p1 p2 -> do
    env <- get
    case evalExp env e of
      VBool b -> if b then evalProgram p1 else evalProgram p2
      _       -> error "type error"

  While e p -> do
    env <- get
    case evalExp env e of
      VBool b -> if b then evalProgram p >> evalStatement (While e p)
                      else pure ()
      _       -> error "type error"


evalProgram :: Program -> State Env ()
evalProgram = traverse_ evalStatement

run :: String -> Env
run str = case runParser parseProgram str of
  Nothing        -> error "parse error"
  Just (prog, _) -> execState (evalProgram prog) []
