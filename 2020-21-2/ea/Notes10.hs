
-- hf2 kiírva
-- hf3 jövő héten

-- maradék EA (+2 EA)
-- precedencia kombinátorok, State interpreter ("while" nyelv)

-- köv    EA: egy darab korábbi vizsgasor átnézése
-- utolsó EA: téma teams szavazás alapján

--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Control.Monad.State
import Debug.Trace -- trace, traceM

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor  -- sikeres parse végeredményén függvény alkalmazása

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  -- nem olvasunk, csak visszaadunk egy értéket
  return a = Parser $ \s -> Just (a, s)

  -- egymás után futtatunk két parser-t (a második függhet az első
  -- eredményétől)
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (g a) s'

instance Alternative Parser where

  -- rögtön hibázó parser
  empty = Parser $ \_ -> Nothing

  -- először az első parser-t futtatjuk, hiba esetén a másodikat
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing      -> g s
    Just (a, s') -> Just (a, s')

-- üres input
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- feltételnek megfelelő karakter
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

-- konkrét karakter
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- bármilyen karakter
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

-- konkrét string
string :: String -> Parser ()
string str = mapM_ char str

-- 1 vagy több "a"-t olvasunk, "sep"-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep >> pa)
                      -- a        [a]
                 -- (:) :: a -> [a] -> [a]

-- 0 vagy több "a"-t olvasunk, "set"-el szeparálva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- + Control.Applicative-ból importálva:
--  - many   : nulla vagy több érték olvasása
--  - some   : egy vagy több érték olvasása
many_ :: Parser a -> Parser ()
many_ p = () <$ many p

some_ :: Parser a -> Parser ()
some_ p = () <$ some p

-- Beolvasunk először egy b-t, utána 0 vagy több a-t, az eredményeket
-- balra asszociálva kombináljuk az adott függvénnyel.
chainl :: (b -> a -> b) -> Parser b -> Parser a -> Parser b
chainl f pb pa = do {b <- pb; go b} where
  go b = (do {a <- pa; go (f b a)}) <|> pure b

ws :: Parser ()
ws = many_ (char ' ' <|> char '\n')

string' :: String -> Parser ()
string' str = string str <* ws

char' :: Char -> Parser ()
char' c = char c <* ws

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

pDebug :: String -> Parser ()
pDebug msg = Parser $ \s -> trace (msg ++ "     " ++ s) (Just ((), s))

-- Control.Applicative-ből importálva:
-- optional :: Parser a -> Parser (Maybe a)
-- optional pa = (Just <$> pa) <|> pure Nothing

infixLeft :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
infixLeft f pa psep = foldl1 f <$> sepBy1 pa psep

infixRight :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
infixRight f pa psep = foldr1 f <$> sepBy1 pa psep

infixNonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
infixNonAssoc f pa psep = do
  es <- sepBy1 pa psep
  case es of
    [e]      -> pure e
    [e1, e2] -> pure $ f e1 e2
    _        -> empty

prefixAssoc :: (a -> a) -> Parser op -> Parser a -> Parser a  -- házi feladat: rekurzió helyett many-vel (pl)
prefixAssoc f pop pa = (pop *> (f <$> prefixAssoc f pop pa)) <|> pa

prefixNonAssoc :: (a -> a) -> Parser op -> Parser a -> Parser a
prefixNonAssoc f pop pa = (pop *> (f <$> pa)) <|> pa

-- precedecia segédfüggvények
--------------------------------------------------------------------------------

{-

{-
  - atom
  - ++    prefix asszoc
  - *     bal asszoc
  - +     bal asszoc
  - ==    nem asszoc
-}

-- példa: prefix asszoc:       ++ ++ 10
--        prefix nem-asszoc :  ++(++10)

posInt' :: Parser Int
posInt' = (read <$> some (satisfy isDigit)) <* ws

data Exp = IntLit Int | Mul Exp Exp | Add Exp Exp | Eq Exp Exp | PreInc Exp deriving Show

atom   = (char' '(' *> eq <* char' ')') <|> (IntLit <$> posInt')
preinc = (string' "++" *> (PreInc <$> preinc)) <|> atom
mul    = foldl1 Mul <$> sepBy1 preinc (char' '*')
add    = foldl1 Add <$> sepBy1 mul  (char' '+')

eq = do
  es <- sepBy1 add (string' "==")
  case es of
    [e]      -> pure e
    [e1, e2] -> pure $ Eq e1 e2
    _        -> empty

pExp = topLevel eq

-- Esetek: infix bal, infix jobb, infix nem asszoc
--   (megkötés: minden erősségi szinten pontosan egy operátor lehet)

-- Házi feladat megnézni: hogyan lehet általánosan az alábbi precedencia-kombinátorokat
--     (pl minden erősségi szinten több operátor lehet)

infixLeft :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
infixLeft f pa psep = foldl1 f <$> sepBy1 pa psep

infixRight :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
infixRight f pa psep = foldr1 f <$> sepBy1 pa psep

infixNonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
infixNonAssoc f pa psep = do
  es <- sepBy1 pa psep
  case es of
    [e]      -> pure e
    [e1, e2] -> pure $ f e1 e2
    _        -> empty

prefixAssoc :: (a -> a) -> Parser op -> Parser a -> Parser a  -- házi feladat: rekurzió helyett many-vel (pl)
prefixAssoc f pop pa = (pop *> (f <$> prefixAssoc f pop pa)) <|> pa

prefixNonAssoc :: (a -> a) -> Parser op -> Parser a -> Parser a
prefixNonAssoc f pop pa = (pop *> (f <$> pa)) <|> pa

-- házi: posztfix segédfüggvény (asszoc / nem asszoc)

--------------------------------------------------------------------------------

-- kompakt verzió:
atom2   = (char' '(' *> eq <* char' ')') <|> (IntLit <$> posInt')
preinc2 = prefixAssoc PreInc (string' "++") atom2
mul2    = infixLeft Mul preinc2 (char' '*')
add2    = infixLeft Add mul2 (char' '+')
eq2     = infixNonAssoc Eq add2 (string' "==")

pExp2 = topLevel eq2

-}


-- State interpreter + parser
--------------------------------------------------------------------------------

-- aritmetika, kifejezések, állítások (statement)

-- kifejezés: int literál, bool literál, int műveletek, bool művelet, változónevek
--     kifejezés mellékhatás nélkül kiértékelhető
-- állítás (statement) : értékadás, while ciklus, if-then-else
--     értékadás: ha új változónak adunk értéket, az új változó
--                ha meglévő változónak: mutáció

type Name    = String

type Program = [Statement]   -- e_1;e_2;....;e_n

data Exp
  = IntLit Int
  | Add Exp Exp    -- e + e
  | Mul Exp Exp    -- e * e
  | BoolLit Bool   -- true|false
  | And Exp Exp    -- e /\ e
  | Eq Exp Exp     -- e == e
  | Lt Exp Exp     -- e < e
  | Var Name
  deriving (Eq, Show)

data Statement
  = Assign Name Exp                 -- x := e
  | While Exp Program               -- while e do p end
  | IfThenElse Exp Program Program  -- if e then p else p end
  deriving (Eq, Show)


-- parser
--------------------------------------------------------------------------------

-- keyword vs. Name

keywords :: [String]
keywords = ["if", "then", "else", "end", "while", "do", "true", "false"]

pKeyword' :: String -> Parser ()
pKeyword' kw = do
  string kw
  mchar <- optional (satisfy isLetter)
  case mchar of
    Just _  -> empty
    Nothing -> ws

pName' :: Parser Name
pName' = do
  x <- some (satisfy isLetter) <* ws
  if elem x keywords
    then empty
    else pure x

posInt' :: Parser Int
posInt' = (read <$> some (satisfy isDigit)) <* ws

pAtom :: Parser Exp
pAtom =  (char' '(' *> pExp <* char' ')')
    <|> (BoolLit True  <$ pKeyword' "true")
    <|> (BoolLit False <$ pKeyword' "false")
    <|> (IntLit <$> posInt')
    <|> (Var <$> pName')

pMul = infixLeft   Mul pAtom (char' '*')
pAdd = infixLeft   Add pMul  (char' '+')            -- e1 /\ (e2 /\ (e3 /\ e4))
pAnd = infixRight  And pAdd  (string' "/\\")        -- (((e1 /\ e2) /\ e3) /\ e4)
pLt  = infixNonAssoc Lt pAnd (char' '<')
pEq  = infixNonAssoc Eq pLt (string' "==")

pExp :: Parser Exp
pExp = pEq

pStatement :: Parser Statement
pStatement =
      (Assign <$> (pName' <* string' ":=") <*> pExp)
  <|> (While <$> (pKeyword' "while" *> pExp <* pKeyword' "do")
             <*> (pProgram <* pKeyword' "end"))
  <|> (IfThenElse <$> (pKeyword' "if" *> pExp <* pKeyword' "then")
                  <*> pProgram
                  <*> (pKeyword' "else" *> pProgram <* pKeyword' "end"))

pProgram :: Parser Program
pProgram = sepBy pStatement (char' ';')

pSrc :: Parser Program
pSrc = topLevel pProgram

-- interpreter
--------------------------------------------------------------------------------

-- mi a kiértékelés típusa?
--    program : változó környezetet módosít

data Val = VInt Int | VBool Bool deriving (Eq, Show)

type Env = [(Name, Val)]

evalExp :: Exp -> (Env -> Val)   -- ha lenne mellékhatás: Exp -> State Env Val
evalExp exp env = go exp where

  go :: Exp -> Val
  go (IntLit n)  = VInt n
  go (Add e1 e2) = case (go e1, go e2) of
                     (VInt n, VInt m) -> VInt (n + m)
                     _                -> error "type error"
  go (Mul e1 e2) = case (go e1, go e2) of
                     (VInt n, VInt m) -> VInt (n * m)
                     _                -> error "type error"
  go (BoolLit b) = VBool b
  go (And e1 e2) = case (go e1, go e2) of
                     (VBool b1, VBool b2) -> VBool (b1 && b2)
                     _                    -> error "type error"
  go (Eq e1 e2)  = case (go e1, go e2) of
                     (VBool b1, VBool b2) -> VBool (b1 == b2)
                     (VInt  n,  VInt m  ) -> VBool (n  == m )
                     _                    -> error "type error"
  go (Lt e1 e2)  = case (go e1, go e2) of
                     (VInt n, VInt m) -> VBool (n < m)
                     _                -> error "type error"
  go (Var x)     = case lookup x env of
                     Just v  -> v
                     Nothing -> error ("name not in scope: " ++ x)

update :: Name -> Val -> Env -> Env
update x v [] = [(x, v)]
update x v ((x', v'):env)
  | x == x'   = (x, v):env
  | otherwise = (x', v'): update x v env

evalStatement :: Statement -> State Env ()
evalStatement st = case st of
  Assign x e -> do
    env <- get
    let v = evalExp e env
    put $ update x v env

  While e p -> do
    env <- get
    case evalExp e env of
      VBool b -> if b then evalProgram p >> evalStatement (While e p)  -- házi feladat: while legyen lokális rekurzív loop függvénnyel
                      else pure ()
      _ -> error "type error"

  IfThenElse e p1 p2 -> do
    env <- get
    case evalExp e env of
      VBool b -> if b then evalProgram p1
                      else evalProgram p2
      _ -> error "type error"

evalProgram :: Program -> State Env ()   -- másképpen: Program -> (Env -> Env)
evalProgram = mapM_ evalStatement
