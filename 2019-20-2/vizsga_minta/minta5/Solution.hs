{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable (toList)
import Data.Tuple (swap)

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

data Tree a = Node (Either a (Tree a)) (Tree a) | TriLeaf a a a
  deriving (Eq, Ord, Show)

ex1 :: Tree Int
ex1 =
  Node
   (Left 1)
    (Node
      (Right (TriLeaf 2 3 4))
      (TriLeaf 5 6 7))

-- 1
instance Functor Tree where
  fmap f (Node (Left x) rhs)    = Node (Left $ f x) (fmap f rhs)
  fmap f (Node (Right lhs) rhs) = Node (Right $ fmap f lhs) (fmap f rhs)
  fmap f (TriLeaf x y z)        = TriLeaf (f x) (f y) (f z)

-- 2
instance Foldable Tree where
  foldMap f (Node (Left x) rhs)    = f x <> foldMap f rhs
  foldMap f (Node (Right lhs) rhs) = foldMap f lhs <> foldMap f rhs
  foldMap f (TriLeaf x y z) = f x <> f y <> f z

-- 1
leftmost :: Tree a -> a
leftmost = head . toList

-- 2
findComplementInTriLeaf :: (a -> Bool) -> Tree a -> Maybe a
findComplementInTriLeaf pred (Node (Right lhs) rhs)
  = findComplementInTriLeaf pred lhs <|> findComplementInTriLeaf pred rhs
findComplementInTriLeaf pred (Node _ rhs)
  = findComplementInTriLeaf pred rhs
findComplementInTriLeaf pred (TriLeaf _ y _)
  | not (pred y) = Just y
  | otherwise    = Nothing

-- 1
instance Traversable Tree where
  traverse f (Node (Left x) rhs)    = Node <$> (Left <$> f x) <*> (traverse f rhs)
  traverse f (Node (Right lhs) rhs) = Node <$> (Right <$> traverse f lhs) <*> (traverse f rhs)
  traverse f (TriLeaf x y z)        = TriLeaf <$> (f x) <*> (f y) <*> (f z)

-- 2
countTriLeafs :: Tree a -> Tree (a, Int)
countTriLeafs t = evalState (countTriLeafsM t) 0 where
  countTriLeafsM :: Tree a -> State Int (Tree (a, Int))
  countTriLeafsM (Node (Left x) rhs) = do
    n <- get
    rhs' <- countTriLeafsM rhs
    pure $ Node (Left (x, n)) rhs'
  countTriLeafsM (Node (Right lhs) rhs) = do
    n <- get
    lhs' <- countTriLeafsM lhs
    rhs' <- countTriLeafsM rhs
    pure $ Node (Right lhs') rhs'
  countTriLeafsM (TriLeaf x y z) = do
    n <- get
    modify succ
    pure $ TriLeaf (x,n) (y,n) (z,n)


-- 3
periodicReplace :: [a] -> Tree a -> Tree a
periodicReplace [] t = t
periodicReplace xs t = evalState (replaceFromInfListM t) (cycle xs) where
  peekPopM :: State [a] a
  peekPopM = do
    x <- head <$> get
    modify tail
    pure x

  replaceFromInfListM :: Tree a -> State [a] (Tree a)
  replaceFromInfListM (Node (Left x) rhs) = do
    x' <- peekPopM
    rhs' <- replaceFromInfListM rhs
    pure $ Node (Left x') rhs'
  replaceFromInfListM (Node (Right lhs) rhs) = do
    lhs' <- replaceFromInfListM lhs
    rhs' <- replaceFromInfListM rhs
    pure $ Node (Right lhs') rhs'
  replaceFromInfListM (TriLeaf x y z) = do
    x' <- peekPopM
    y' <- peekPopM
    z' <- peekPopM
    pure $ TriLeaf x' y' z'

-- összesen: 12

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
  | Peek
  deriving Show

type Program = [Statement]
type Name    = String

data Statement
  = Assign Name Exp         -- x := e
  | While Exp Program       -- while e do p1 end
  | If Exp Program Program  -- if e then p1 else p2 end
  | Block Program           -- {p1}       (lokális scope)
  | Push Exp
  | Pop

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
keywords = ["not", "and", "while", "do", "if", "end", "true", "false", "peek", "push", "pop"]

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

pAtom :: Parser Exp
pAtom = (BoolLit <$> pBoolLit)
    <|> (IntLit <$> pIntLit)
    <|> (Var <$> pIdent)
    <|> (char' '(' *> pExp <* char' ')')
    <|> Peek <$ string' "peek"

pNot :: Parser Exp
pNot =
      (Not <$> (string' "not" *> pAtom))
  <|> pAtom

pMul :: Parser Exp
pMul = foldl1 Mul <$> sepBy1 pNot (char' '*')

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
    <|> (Push <$> (string' "push" *> pExp))
    <|> (Pop <$ string' "pop")
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

type Val  = Either Int Bool
type Env  = [(Name, Val)]
type Eval = State ([Int], Env)

binOp :: String
      -> Exp
      -> Exp
      -> Either (Int -> Int -> Int) (Bool -> Bool -> Bool)
      -> Eval Val
binOp opName e1 e2 f = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (f, v1, v2) of
    (Left f , Left n1 , Left n2 ) -> pure (Left  (f n1 n2))
    (Right f, Right b1, Right b2) -> pure (Right (f b1 b2))
    _                             -> error ("type error in " ++ opName ++ " argument")

evalExp :: Exp -> Eval Val
evalExp e = case e of
  Add e1 e2 -> binOp "+"  e1 e2 (Left (+))
  Mul e1 e2 -> binOp "*"  e1 e2 (Left (*))
  And e1 e2 -> binOp "&&" e1 e2 (Right (&&))
  Or  e1 e2 -> binOp "||" e1 e2 (Right (||))
  Eq  e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (Left  n1, Left  n2) -> pure (Right (n1 == n2))
      (Right b1, Right b2) -> pure (Right (b1 == b2))
      _                    -> error "type error in == arguments"
  Lt  e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (Left  n1, Left  n2) -> pure (Right (n1 < n2))
      _                    -> error "type error in < arguments"
  Var x -> do
    env <- snd <$> get
    case lookup x env of
      Nothing -> error ("variable not in scope: " ++ x)
      Just v  -> pure v
  IntLit n -> pure (Left n)
  BoolLit b -> pure (Right b)
  Not e -> do
    v <- evalExp e
    case v of
      Right b -> pure (Right (not b))
      _       -> error "type error in \"not\" argument"
  Peek -> do
    x <- head . fst <$> get
    pure $ Left x

newScope :: Eval a -> Eval a
newScope ma = do
  env <- get
  a <- ma
  modify (\(stack, env') -> (stack, drop (length env' - length env) env'))
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
    modify (\(stack, env) -> (stack, updateEnv x v env))
  While e p -> do
    v <- evalExp e
    case v of
      Right b -> if b then newScope (evalProg p) >> evalSt (While e p)
                      else pure ()
      _       -> error "type error: expected a Bool condition in \"while\" expression"
  If e p1 p2 -> do
    v <- evalExp e
    case v of
      Right b -> if b then newScope (evalProg p1)
                      else newScope (evalProg p2)
      _       -> error "type error: expected a Bool condition in \"if\" expression"
  Push expr -> do
    valE <- evalExp expr
    case valE of
      Left val -> modify (\(xs, env) -> (val:xs, env))
      Right _ -> error "type mismatch at push"
  Pop ->
    modify (\((_:xs), env) -> (xs, env))
  Block p ->
    newScope (evalProg p)

evalProg :: Program -> Eval ()
evalProg = mapM_ evalSt


-- interpreter
--------------------------------------------------------------------------------

runProg :: String -> (Env, [Int])
runProg str = case runParser pSrc str of
  Nothing     -> error "parse error"
  Just (p, _) -> swap $ execState (evalProg p) ([], [])

p1 :: String
p1 = "x := 10; y := 20; {z := x + y}; x := x + 100"



-- tests
--------------------------

tests =
  [ fmap (*2) (fmap (+1) ex1) == fmap ((*2).(+1)) ex1
  , fmap id ex1 == ex1
  , fmap (const 1) (TriLeaf 0 0 0) == TriLeaf 1 1 1

  , sum ex1 == 28

  , leftmost ex1 == 1

  , traverse (\x -> Just x) ex1 == Just ex1

  , findComplementInTriLeaf (==3) ex1 == Just 6
  , findComplementInTriLeaf (/=3) ex1 == Just 3
  , findComplementInTriLeaf (<4) ex1 == Just 6
  , findComplementInTriLeaf (4<) ex1 == Just 3
  , findComplementInTriLeaf (\x -> x == 3 || x == 6) ex1 == Nothing

  , countTriLeafs (Node (Right (TriLeaf 1 2 3)) (Node (Left 4) (TriLeaf 0 0 0))) == Node (Right (TriLeaf (1,0) (2,0) (3,0))) (Node (Left (4,1)) (TriLeaf (0,1) (0,1) (0,1)))
  , countTriLeafs (Node (Right $ TriLeaf 1 1 1) (Node (Left 2) (TriLeaf 3 4 5))) == (Node (Right $ TriLeaf (1,0) (1,0) (1,0)) (Node (Left (2,1)) (TriLeaf (3,1) (4,1) (5,1))))

  , periodicReplace [7,8,9] ex1 == Node (Left 7) (Node (Right (TriLeaf 8 9 7)) (TriLeaf 8 9 7))
  , periodicReplace [] ex1 == ex1
  , periodicReplace [0,1] ex1 == periodicReplace [0,1,0,1] ex1

  , runProg "push 5; push (1+2); x := peek; pop; y := peek; z := x + y" == ([("z",Left 8),("y",Left 5),("x",Left 3)],[5])
  ]

