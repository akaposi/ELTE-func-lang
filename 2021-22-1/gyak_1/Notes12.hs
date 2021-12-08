
{-# LANGUAGE InstanceSigs, DeriveFunctor, DeriveFoldable,
    DeriveTraversable #-}

import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Applicative -- many, some
import Data.Char           -- isDigit :: Char -> Bool
                           -- digitToInt :: Char -> Int

import Debug.Trace         -- trace :: String -> a -> a
                           -- traceShow :: Show b => b -> a -> a

import Control.Monad.State


-- Következő canvas feladat: ismétlő kisfeladat
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

-- Canvas feladat

data List a = Nil | Cons1 a (List a) | Cons2 a a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil              = Nil
  fmap f (Cons1 a as)     = Cons1 (f a) (fmap f as)
  fmap f (Cons2 a1 a2 as) = Cons2 (f a1) (f a2) (fmap f as)

instance Foldable List where
  -- egyiket definiáld
  foldr f b Nil              = b
  foldr f b (Cons1 a as)     = f a (foldr f b as)
  foldr f b (Cons2 a1 a2 as) = f a1 (f a2 (foldr f b as))

  foldMap f Nil              = mempty
  foldMap f (Cons1 a as)     = f a <> foldMap f as
  foldMap f (Cons2 a1 a2 as) = f a1 <> f a2 <> foldMap f as

instance Traversable List where
  traverse f Nil              = pure Nil
  traverse f (Cons1 a as)     = Cons1 <$> f a <*> traverse f as
  traverse f (Cons2 a1 a2 as) = Cons2 <$> f a1 <*> f a2 <*> traverse f as


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
ws = many_ (char ' ')

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

intLit' :: Parser Int
intLit' = do
  isNeg <- (True <$ char '-') <|> pure False
  n <- read <$> some (satisfy isDigit)
  ws
  if isNeg then pure (n * (-1))
           else pure n

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' str = string str <* ws

-- min 1 pa olvasás, psep-el elválasztva, "combine" fv-el kombinálva
-- balra asszociáltan
infixLeft :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixLeft pa psep combine = foldl1 combine <$> sepBy1 pa psep

-- min 1 pa olvasás, psep-el elválasztva, "combine" fv-el kombinálva
-- jobbra asszociáltan
infixRight :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixRight pa psep combine = foldr1 combine <$> sepBy1 pa psep

infixNonAssoc :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixNonAssoc pa psep combine = do
  exps <- sepBy1 pa psep
  case exps of
    [exp]        -> pure exp                  -- 1 db pa kifejezés
    [exp1, exp2] -> pure $ combine exp1 exp2  -- exp1 `psep` exp2
    _            -> empty

-- Feladat (nyelv kifejezésekkel + állításokkal)
--------------------------------------------------------------------------------

{-
Precedenciák:
  - literál (bool/int), zárójel
  - *  (jobb asszoc)
  - +  (jobb asszoc)
  - == (nem asszoc)
-}

data Exp
  = IntLit Int
  | BoolLit Bool
  | Add Exp Exp
  | Mul Exp Exp
  | Eq Exp Exp
  | Var String            -- változónév
  deriving (Eq, Show)

type Program = [Statement]

data Statement
  = Assign String Exp               --   x := exp
  | IfThenElse Exp Program Program  --   if e1 then p1 else p2 end
  | While Exp Program               --   while e do p end
  deriving (Eq, Show)

-- legyen minden statement ;-vel elválasztva
-- cél: Program-ot olvassunk

keywords :: [String]
keywords = ["if", "then", "else", "while", "do", "end"]

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

atom :: Parser Exp
atom = (IntLit <$> intLit')
   <|> (char' '(' *> eqExp <* char' ')')
   <|> (BoolLit <$> (True  <$ keyword' "true"))
   <|> (BoolLit <$> (False <$ keyword' "false"))
   <|> (Var <$> ident')

mulExp :: Parser Exp
mulExp = infixRight atom (char' '*') Mul

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
pAssign = string' ":="     -- nincs átfedés azonosítóval

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

-- példa
p1 :: String
p1 = unlines [
  "x := 100;",
  "y := 1000;",
  "while x == 0 do",
  "  y := y + 1",
  "end"
  ]

-- Interpreter
--------------------------------------------------------------------------------

-- értékek
data Val = VInt Int | VBool Bool
  deriving (Eq, Show)

-- környezet
type Env = [(String, Val)]

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
  Eq e1 e2  -> case (evalExp env e1, evalExp env e2) of
    (VInt n1, VInt n2)   -> VBool (n1 == n2)
    (VBool b1, VBool b2) -> VBool (b1 == b2)
    _                    -> error "type error"
  Var x -> case lookup x env of
    Nothing -> error $ "name not in scope: " ++ x
    Just v  -> v

updateEnv :: String -> Val -> Env -> Env
updateEnv x v [] = [(x, v)]
updateEnv x v ((x', v'):env)
   | x == x'   = (x, v):env
   | otherwise = (x', v'): updateEnv x v env

evalStatement :: Statement -> State Env ()
evalStatement st = case st of
  Assign x e -> do
    env <- get
    let v = evalExp env e
    put $ updateEnv x v env
  IfThenElse e p1 p2 -> do
    env <- get
    case evalExp env e of
      VBool b -> if b then evalProgram p1
                      else evalProgram p2
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
  Nothing     -> error "parse error"
  Just (p, _) -> execState (evalProgram p) []


-- Canvas feladat
--------------------------------------------------------------------------------

-- atom: literál vagy ( )
-- (&&): jobb asszoc
-- (||): jobb asszoc

data Exp2 = BoolLit2 Bool | And2 Exp2 Exp2 | Or2 Exp2 Exp2
  deriving (Eq, Show)

atom2 :: Parser Exp2
atom2 =
      (BoolLit2 True <$ string' "true")
  <|> (BoolLit2 False <$ string' "false")
  <|> (char' '(' *> orExp2 <* char' ')')

andExp2 :: Parser Exp2
andExp2 = infixRight atom2 (string' "&&") And2

orExp2 :: Parser Exp2
orExp2 = infixRight andExp2 (string' "||") Or2

parseExp2 :: Parser Exp2
parseExp2 = topLevel orExp2


-- Feladat: extra műveletek a kifejezésekben
--------------------------------------------------------------------------------

-- egészítsd ki a kifejezéseket a következő konstruktorokkal:

--  And :: Exp -> Exp -> Exp
--  Or  :: Exp -> Exp -> Exp
--  Not :: Exp -> Exp

--  Egészítsd ki a parsert And, Or és Not olvasásával úgy, hogy a precedenciák a
--  következők legyenek:

{-
  - atom: literál, zárójelezés
  - not alkalmazás          ("not" kulcsszó után atom olvasása)
  - *  (jobb asszoc)
  - +  (jobb asszoc)
  - && (jobb asszoc)        (logikai And)
  - || (jobb asszoc)        (logikai Or)
  - == (nem asszoc)
  - if e1 then e2 else e3   (prefix)
-}

-- Add hozzá az interpreterhez az új műveletek értelmezését!



-- Feladat: String literálok és String műveletek
--------------------------------------------------------------------------------

-- Egészítsd ki a kifejezéseket a következő konstruktorokkal:

--   StringLit :: String -> Exp
--   Append    :: Exp -> Exp -> Exp
--   Head      :: Exp -> Exp
--   Tail      :: Exp -> Exp
--   Length    :: Exp -> Exp

-- Egészítsd ki a parser-t, a következő módon:

{-
  - atom: Int/Bool/String literál, zárójelezés
  - "not", "head", "tail", "length" alkalmazás             (kulcsszó után atom olvasása)
  - *  (jobb asszoc)
  - +  (jobb asszoc)
  - && (jobb asszoc)        (logikai And)
  - || (jobb asszoc)        (logikai Or)
  - ++ (jobb asszoc)        (String összefűzés)
  - == (nem asszoc)
  - if e1 then e2 else e3   (prefix)
-}

-- A "head", "tail" és "length" kulcsszavak!
-- String literál: 0 vagy több tetszőleges nem '"' karakter olvasva, két '"' karakter között.

-- Egészítsd ki a `Val` definíciót String értékekkel!

-- Egészítsd ki az interpretert is! Az új műveletek működjenek úgy, mint Haskell-ben,
-- nem megfelelő típusú értékek esetén dobj hibát.

--------------------------------------------------------------------------------
