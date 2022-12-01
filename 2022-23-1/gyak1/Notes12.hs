
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, InstanceSigs
    #-}
{-# options_ghc -Wincomplete-patterns #-}


------------------------------------------------------------

-- Minta vizsga. Ugyanaz, mint a "mintavizsga/Minta1.hs".
--  A vizsga két részből áll:
--    1. Kis feladatok (10-12 pont)
--    2. Parser/interpreter kiegészítés (8-10 pont)

-- Résztpontok vannak (lehet kapni nem funkcionális megoldásra is részpontot)

-- Ponthatárok:
--   2: 10-11
--   3: 12-14
--   4: 15-16
--   5: 17-20

-- Ebben a fájlban 2 darab "kis feladatok" sor van.

--------------------------------------------------------------------------------

import Control.Applicative
import Control.Monad
import Debug.Trace
import Data.Char    -- isSpace, isDigit

-- Feladat megoldás
------------------------------------------------------------

printLines :: [String] -> IO ()
printLines [] = pure ()
printLines (l:lines) = do
  printLines lines
  putStrLn l

loop :: [String] -> IO ()
loop lines = do
  l <- getLine
  printLines (l:lines)
  loop (l:lines)

f :: IO ()
f = loop []

------------------------------------------------------------

pLeft :: Parser (Either Int Int)
pLeft = do
  string "Left" <* ws
  n <- pPos <* ws
  pure (Left n)

pRight :: Parser (Either Int Int)
pRight = do
  string "Right" <* ws
  n <- pPos <* ws
  pure (Right n)

pEitherIntInt :: Parser (Either Int Int)
pEitherIntInt = pLeft <|> pRight

kisf :: Parser (Either Int Int, Int)
kisf = do
  ws
  char '(' <* ws
  eii <- pEitherIntInt
  char ',' <* ws
  n <- pPos <* ws
  char ')' <* ws
  eof
  pure (eii, n)


-- State
--------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure a  = State (\s -> (a, s))
  (<*>) = ap

instance Monad (State s) where
  return = pure
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


-- Parser lib
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) = ap

instance Monad Parser where
  return = pure
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing     -> Nothing
    Just (a, s) -> runParser (g a) s

eof :: Parser ()
eof = Parser $ \s -> case s of
  "" -> Just ((), "")
  _  -> Nothing

-- egy karaktert olvassunk az input elejéről, amire
-- igaz egy feltétel
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- olvassunk egy konkrét karaktert
char :: Char -> Parser ()
char c = () <$ satisfy (==c)
  -- satisfy (==c)   hiba: Parser Char helyett Parser () kéne

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- konkrét String olvasása:
string :: String -> Parser ()
string = mapM_ char -- minden karakterre alkalmazzuk a char-t

-- standard függvények (Control.Applicative-ból)
-- many :: Parser a -> Parser [a]
--    (0-szor vagy többször futtatunk egy parser-t)
-- some :: Parser a -> Parser [a]
--    (1-szor vagy többször futtatunk egy parser-t)

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

pDigit :: Parser Int
pDigit = digitToInt <$> satisfy isDigit

-- pozitív Int olvasása
pPos :: Parser Int
pPos = do
  ds <- some pDigit
  pure $ sum $ zipWith (*) (reverse ds) (iterate (*10) 1)

rightAssoc :: (a -> a -> a) -> Parser a            -> Parser sep -> Parser a
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

-- nem láncolható prefix operátor
prefix :: (a -> a) -> Parser a -> Parser op -> Parser a
prefix f pa pop = (pop *> (f <$> pa)) <|> pa

ws :: Parser ()
ws = many_ (satisfy isSpace)

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

topLevel :: Parser a -> Parser a
topLevel p = ws *> p <* eof


-- PARSER/INTERPRETER kiegészítés
--------------------------------------------------------------------------------

{-
1. Feladat:
  - Egészítsd ki a nyelvet pár típusú értékekkel!
  - Szintaxis:     (e1, e2)
                   fst e
                   snd e
  - fst és snd precendenciája legyen ugyanaz, mint "not"-é (tipp: not parser-t egészítsük ki!)
  - Egészítsd ki a parser-t és az interpretert.
     - Egészítsd ki az Exp típust a megfelelő konstruktorokkal, amelyek
         az fst, snd és párképzés műveleteket reprezentálják.
  - interpreter:
     - Egészíts ki a Val típust értékek párjaival.
     - Az fst és snd típushibát dob, ha nem pár értéket kap.
     - Az fst vegye egy pár első elemét, az snd a másodikat.
-}

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
  | Var String          -- (változónév)

  | Pair Exp Exp        -- (e, e)
  | Fst Exp             -- fst e
  | Snd Exp             -- snd e
  deriving (Eq, Show)

{-
Változónév: nemüres alfabetikus string

Kötési erősségek csökkenő sorrendben:
  - atom: zárójelezett kifejezés, literál, változónév
  - not alkalmazás
  - *  : jobbra asszoc
  - +  : jobbra asszoc
  - -  : jobbra asszoc
  - && : jobbra asszoc
  - || : jobbra asszoc
  - == : nem asszoc
-}

posInt' :: Parser Int
posInt' = do
  digits <- some (satisfy isDigit)
  ws
  pure (read digits)

keywords :: [String]
keywords = ["not", "true", "false", "while",
            "if", "do", "end", "then", "else", "fst", "snd"]

ident' :: Parser String
ident' = do
  x <- some (satisfy isAlpha) <* ws
  if elem x keywords
    then empty
    else pure x

keyword' :: String -> Parser ()
keyword' s = do
  string s
  (satisfy isLetter >> empty) <|> ws

pairExp :: Parser Exp
pairExp = do
  char' '('
  e1 <- pExp
  char' ','
  e2 <- pExp
  char' ')'
  pure (Pair e1 e2)

-- hatékonyabb verzió (atom-ba betesszük mint
--  1db eset)
pairOrParens :: Parser Exp
pairOrParens = do
  char' '('
  exps <- sepBy1 pExp (char' ',')
  char' ')'
  case exps of
    [e]     -> pure e
    [e1,e2] -> pure (Pair e1 e2)
    _       -> empty

atom :: Parser Exp
atom =
        (Var <$> ident')
    <|> (IntLit <$> posInt')
    <|> (BoolLit True <$ keyword' "true")
    <|> (BoolLit False <$ keyword' "false")
    <|> pairExp
    <|> (char' '(' *> pExp <* char' ')')

notExp :: Parser Exp
notExp =
        (keyword' "not" *> (Not <$> atom))
    <|> (keyword' "fst" *> (Fst <$> atom))
    <|> (keyword' "snd" *> (Snd <$> atom))
    <|> atom

mulExp :: Parser Exp
mulExp = rightAssoc Mul notExp (char' '*')

addExp :: Parser Exp
addExp = rightAssoc Add mulExp (char' '+')

subExp :: Parser Exp
subExp = rightAssoc Sub addExp (char' '-')

andExp :: Parser Exp
andExp = rightAssoc And subExp (string' "&&")

orExp :: Parser Exp
orExp = rightAssoc Or andExp (string' "||")

eqExp :: Parser Exp
eqExp = nonAssoc Eq orExp (string' "==")

pExp :: Parser Exp
pExp = eqExp

data Val = VInt Int | VBool Bool | VPair Val Val
  deriving (Eq, Show)

type Env = [(String, Val)]

evalExp :: Env -> Exp -> Val
evalExp env e = case e of
  IntLit n  -> VInt n
  BoolLit b -> VBool b
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
  Eq e1 e2 -> case (evalExp env e1, evalExp env e2) of
    (VBool b1, VBool b2) -> VBool (b1 == b2)
    (VInt n1,  VInt n2 ) -> VBool (n1 == n2)
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

program :: Parser Program
program = sepBy statement (char' ';')

-- Ha valami newScope-ban fut, akkor a futás után az újonnan felvett változókat
-- eldobjuk az Env-ből.
inNewScope :: State Env a -> State Env a
inNewScope ma = do
  l <- length <$> get
  a <- ma
  modify (take l)
  pure a

updateEnv :: String -> Val -> Env -> Env
updateEnv x v [] = [(x, v)]
updateEnv x v ((y, v'):env)
  | x == y    = (x, v):env
  | otherwise = (y, v'):updateEnv x v env


evalStatement :: Statement -> State Env ()
evalStatement st = case st of

  Assign x e -> do
    env <- get
    let val = evalExp env e
    put $ updateEnv x val env

  While e p -> do
    env <- get
    case evalExp env e of
      VBool True  -> inNewScope (evalProgram p) >> evalStatement (While e p)
      VBool False -> pure ()
      VInt _      -> error "type error"
      VPair _ _   -> error "type error"
  If e p1 p2 -> do
    env <- get
    case evalExp env e of
      VBool True  -> inNewScope (evalProgram p1)
      VBool False -> inNewScope (evalProgram p2)
      VInt _      -> error "type error"
      VPair _ _   -> error "type error"

evalProgram :: Program -> State Env ()
evalProgram = mapM_ evalStatement

run :: String -> Env
run str = case runParser (topLevel program) str of
  Just (prog, _) -> execState (evalProgram prog) []
  Nothing        -> error "parse error"

p1 :: String
p1 = "i := 10; acc := 0; while not (i == 0) do acc := acc + i; i := i - 1 end"

p2 :: String
p2 = "x := (10, 20); y := fst x; z := snd x"

p3 :: String
p3 = "x := ((10, true), 20); y := fst (fst x)"
