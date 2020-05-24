{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Data.Char

-- State monad
--------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  State f >>= g = State $ \s -> case f s of
    (a, s') -> runState (g a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= \s -> put (f s)

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--------------------------------------------------------------------------------
--                              Feladatok
--------------------------------------------------------------------------------

data Tree a = Leaf a | Node1 a (Tree a) | Node2 (Tree a) (Tree a)
  deriving (Eq, Ord, Show)

ex1 :: Tree Int
ex1 =
  Node1 0
    (Node1 2
       (Node2
          (Node1 10 (Leaf 20))
          (Leaf 30)))

-- 1
instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node1 a t) = Node1 (f a) (fmap f t)
  fmap f (Node2 l r) = Node2 (fmap f l) (fmap f r)

-- 2
instance Foldable Tree where
  foldr f z (Leaf a)    = f a z
  foldr f z (Node1 a t) = f a (foldr f z t)
  foldr f z (Node2 l r) = foldr f (foldr f z r) l

  foldMap f (Leaf a)    = f a
  foldMap f (Node1 a t) = f a <> foldMap f t
  foldMap f (Node2 l r) = foldMap f l <> foldMap f r


-- 1
rightmost :: Tree a -> a
rightmost = last . foldr (:) []

-- 1
findElem :: (a -> Bool) -> Tree a -> Maybe a
findElem f = foldr (\a ma -> if f a then Just a else ma) Nothing

-- 1
instance Traversable Tree where
  traverse f (Leaf a)   = Leaf <$> f a
  traverse f (Node1 a t) = Node1 <$> f a <*> traverse f t
  traverse f (Node2 l r) = Node2 <$> traverse f l <*> traverse f r

-- 2
numberElems :: Tree a -> Tree (a, Int)
numberElems t = evalState (traverse go t) 0 where
  go a = do
    n <- get
    put (n + 1)
    pure (a, n)

-- 3
replace :: [a] -> Tree a -> Tree a
replace as t = evalState (traverse go t) as where
  go a = do
    as <- get
    case as of
      []    -> pure a
      a':as -> put as >> pure a'

--------------------------------------------------------------------------------
--                  While nyelv parser + interpreter
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing     -> Nothing
      Just(a, s') -> runParser (g a) s'

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (==c)

string :: String -> Parser ()
string = mapM_ char

ws :: Parser ()
ws = () <$ many (char ' ' <|> char '\n')

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 pa pb = (:) <$> pa <*> many (pb *> pa)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy pa pb = sepBy1 pa pb <|> pure []

anyChar :: Parser Char
anyChar = satisfy (const True)

-- While nyelv
------------------------------------------------------------

data Exp
  = Add Exp Exp    -- a + b
  | Mul Exp Exp    -- a * b
  | Var Name       -- x
  | IntLit Int
  | BoolLit Bool   -- true|false
  | Not Exp        -- not e
  | And Exp Exp    -- a && b
  | Or Exp Exp     -- a || b
  | Eq Exp Exp     -- a == b
  | Lt Exp Exp     -- a < b

  | DoubleLit Double
  | Div Exp Exp
  deriving Show

type Program = [Statement]
type Name    = String

data Statement
  = Assign Name Exp         -- x := e
  | While Exp Program       -- while e do p1 end
  | If Exp Program Program  -- if e then p1 else p2 end
  | Block Program           -- {p1}       (lokális scope)

  deriving Show


-- While parser
--------------------------------------------------------------------------------

{-
Parser a While nyelvhez. A szintaxist az Exp és Statement definíciónál látahtó
fenti kommentek összegzik, továbbá:

  - mindenhol lehet whitespace tokenek között
  - a Statement-eket egy Program-ban válassza el ';'
  - Az operátorok erőssége és assszociativitása a következő:
      infixr 2 ||
      infixr 3 &&
      infix  4 ==
      infix  4 <
      infixl 6 +
      infixl 7 *
  - "not" erősebben köt minden operátornál.
  - A kulcsszavak: not, and, while, do, if, end, true, false.
  - A változónevek legyenek betűk olyan nemüres sorozatai, amelyek *nem* kulcsszavak.
    Pl. "while" nem azonosító, viszont "whilefoo" már az!

Példa szintaktikilag helyes programra:

  x := 10;
  y := x * x + 10;
  while (x == 0) do
    x := x + 1;
    b := true && false || not true
  end;
  z := x
-}

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

keywords :: [String]
keywords = ["not", "and", "while", "do", "if", "end", "true", "false"]

pIdent :: Parser String
pIdent = do
  x <- some (satisfy isLetter) <* ws
  if elem x keywords
    then empty
    else pure x

pBoolLit :: Parser Bool
pBoolLit = (True  <$ string' "true")
       <|> (False <$ string' "false")

pIntLit :: Parser Int
pIntLit = read <$> (some (satisfy isDigit) <* ws)

pDoubleLit :: Parser Double
pDoubleLit = do
  f <- (negate <$ char '-') <|> pure id
  n <- some (satisfy isDigit)
  char '.'
  m <- some (satisfy isDigit) <* ws
  pure $ f $ read (n ++ "." ++ m)

pAtom :: Parser Exp
pAtom = (BoolLit <$> pBoolLit)
    <|> (DoubleLit <$> pDoubleLit)
    <|> (IntLit <$> pIntLit)
    <|> (Var <$> pIdent)
    <|> (char' '(' *> pExp <* char' ')')

pNot :: Parser Exp
pNot =
      (Not <$> (string' "not" *> pAtom))
  <|> pAtom

pDiv :: Parser Exp
pDiv = foldl1 Div <$> sepBy1 pNot (char' '/')

pMul :: Parser Exp
pMul = foldl1 Mul <$> sepBy1 pDiv (char' '*')

pAdd :: Parser Exp
pAdd = foldl1 Add <$> sepBy1 pMul (char' '+')

pEqOrLt :: Parser Exp
pEqOrLt =
  pAdd >>= \e ->
        (Eq e <$> (string' "==" *> pAdd))
    <|> (Lt e <$> (string' "<"  *> pAdd))
    <|> pure e

pAnd :: Parser Exp
pAnd = foldr1 And <$> sepBy1 pEqOrLt (string' "&&")

pOr :: Parser Exp
pOr = foldr1 Or <$> sepBy1 pAnd (string' "||")

pExp :: Parser Exp
pExp = pOr

pProgram :: Parser Program
pProgram = sepBy pStatement (char' ';')

pStatement :: Parser Statement
pStatement =
        (Assign <$> pIdent <*> (string' ":=" *> pExp))
    <|> (While <$> (string' "while" *> pExp)
               <*> (string' "do" *> pProgram <* string' "end"))
    <|> (If <$> (string' "if"   *> pExp)
            <*> (string' "then" *> pProgram)
            <*> (string' "else" *> pProgram <* string' "end"))
    <|> (Block <$> (char' '{' *> pProgram <* char' '}'))

pSrc :: Parser Program
pSrc = ws *> pProgram <* eof


-- Interpreter
------------------------------------------------------------

{-
Interpreter a While nyelvhez.

Kifejezések:
  - A logikai és artimetikai műveletek kiértékelése értelemszerű. Ha nem
    megfelelő típusú értéket kapunk argumentumokra, dobjunk "error"-al hibát.
  - Az == operátor működik, ha mindkét argumentum Bool, vagy ha mindkét argumentum
    Int, az eredmény mindig Bool.

Változó scope és értékadás kezelése:
  - Új scope-nak számít:
    - minden "while" kifejezés teste
    - minden "if" kifejezés két ága
    - minden új Block (a szintaxisban pl "x := 0; {y := x; x := x}"-nél
      a kapcsos zárójeles utasítássorozat új blokkban van).

  - ha egy új változónak értéket adunk, akkor felvesszük a környezet elejére
  - ha egy meglévő változónak értéket adunk, akkor update-eljük a változó értékét
  - amikor az interpreter végez egy scope kiértékeléséval, eldobja az összes,
    scope-ban újonnan felvett változót a környezetből.
-}

data Three a b c = A a | B b | C c deriving (Eq, Show)
type Val = Three Int Bool Double
type Env  = [(Name, Val)]
type Eval = State Env


binOp :: String
      -> Exp
      -> Exp
      -> Three (Int -> Int -> Int) (Bool -> Bool -> Bool) (Double -> Double -> Double)
      -> Eval Val
binOp opName e1 e2 f = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (f, v1, v2) of
    (A f , A n1 , A n2 ) -> pure (A  (f n1 n2))
    (B f, B b1, B b2)    -> pure (B (f b1 b2))
    (C f, C b1, C b2)    -> pure (C (f b1 b2))
    _                    -> error ("type error in " ++ opName ++ " argument")

evalExp :: Exp -> Eval Val
evalExp e = case e of
  Add e1 e2 -> binOp "+"  e1 e2 (A (+))
  Mul e1 e2 -> binOp "*"  e1 e2 (A (*))
  And e1 e2 -> binOp "&&" e1 e2 (B (&&))
  Or  e1 e2 -> binOp "||" e1 e2 (B (||))
  Div e1 e2 -> binOp "/"  e1 e2 (C (/))
  Eq  e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (A  n1, A  n2) -> pure (B (n1 == n2))
      (B b1, B b2) -> pure (B (b1 == b2))
      _                    -> error "type error in == arguments"
  Lt  e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (A  n1, A  n2) -> pure (B (n1 < n2))
      _                    -> error "type error in < arguments"
  Var x -> do
    env <- get
    case lookup x env of
      Nothing -> error ("variable not in scope: " ++ x)
      Just v  -> pure v
  IntLit n -> pure (A n)
  BoolLit b -> pure (B b)
  Not e -> do
    v <- evalExp e
    case v of
      B b -> pure (B (not b))
      _       -> error "type error in \"not\" argument"
  DoubleLit n -> pure (C n)

newScope :: Eval a -> Eval a
newScope ma = do
  env <- (get :: State Env Env)
  a <- ma
  modify (\env' -> drop (length env' - length env) env')
  pure a

updateEnv :: Name -> Val -> Env -> Env
updateEnv x v env =
  case go env of
    Nothing   -> (x, v):env
    Just env' -> env'
  where
    go :: Env -> Maybe Env
    go [] = Nothing
    go ((x', v'):env)
      | x == x'   = Just ((x, v):env)
      | otherwise = ((x', v'):) <$> go env

evalSt :: Statement -> Eval ()
evalSt s = case s of
  Assign x e -> do
    v <- evalExp e
    modify (updateEnv x v)
  While e p -> do
    v <- evalExp e
    case v of
      B b -> if b then newScope (evalProg p) >> evalSt (While e p)
                      else pure ()
      _       -> error "type error: expected a Bool condition in \"while\" expression"
  If e p1 p2 -> do
    v <- evalExp e
    case v of
      B b -> if b then newScope (evalProg p1)
                      else newScope (evalProg p2)
      _       -> error "type error: expected a Bool condition in \"if\" expression"
  Block p ->
    newScope (evalProg p)

evalProg :: Program -> Eval ()
evalProg = mapM_ evalSt


-- interpreter
--------------------------------------------------------------------------------

runProg :: String -> Env
runProg str = case runParser pSrc str of
  Nothing     -> error "parse error"
  Just (p, _) -> execState (evalProg p) []

p1 :: String
p1 = "x := 10; y := 20; {z := x + y}; x := x + 100"

p2 :: String
p2 = "x := 10.0; y := 20.23; x := x / y"
