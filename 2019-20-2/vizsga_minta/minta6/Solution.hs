{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (maximumBy)

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

data Tree a = ANode a [Tree a] | BNode (Tree a) [a] | Leaf a
  deriving (Eq, Ord, Show)

ex1 :: Tree Int
ex1 =
  ANode 2
   [ BNode (Leaf 3) [5,7,11]
   , Leaf 11
   , ANode 13 []
   ]

-- 1
instance Functor Tree where
  fmap f (ANode a ts) = ANode (f a) (map (fmap f) ts)
  fmap f (BNode t as) = BNode (fmap f t) (map f as)
  fmap f (Leaf a) = Leaf (f a)

-- 2
instance Foldable Tree where
  foldMap f (ANode a ts) = f a <> foldMap (foldMap f) ts
  foldMap f (BNode t as) = foldMap f t <> foldMap f as
  foldMap f (Leaf a) = f a

-- 1
rightmost :: Tree a -> a
rightmost = last . toList

-- 2
mostCommon :: Eq a => Tree a -> a
mostCommon xs = fst. maximumBy (compare `on` snd) . map count $ uniqueElems where
  uniqueElems = toList xs
  count x = (x, length $ filter (==x) uniqueElems)

-- 1
instance Traversable Tree where
  traverse f (ANode a ts) = ANode <$> f a <*> traverse (traverse f) ts
  traverse f (BNode t as) = BNode <$> traverse f t <*> traverse f as
  traverse f (Leaf a) = Leaf <$> f a

-- 2
labelWithAlphabet :: Tree a -> Tree (a, Char)
labelWithAlphabet = flip evalState 'a' . traverse go where
  go :: a -> State Char (a, Char)
  go x = do
    c <- get
    modify succ
    pure (x, c)

-- 3
lookupReplace :: Eq k => [(k,v)] -> Tree k -> Maybe (Tree v)
lookupReplace xs = traverse (flip lookup xs)

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
  deriving Show

type Program = [Statement]
type Name    = String

data Statement
  = Assign Name Exp         -- x := e
  | While Exp Program       -- while e do p1 end
  | If Exp Program Program  -- if e then p1 else p2 end
  | Block Program           -- {p1}       (lokális scope)
  | Exit Int
  | Assert Exp
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
keywords = ["not", "and", "while", "do", "if", "end", "true", "false", "assert", "exit"]

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
    <|> (Block <$> (char' '{' *> pProgram <* char' '}'))
    <|> (Assert <$> (string' "assert" *> pExp))
    <|> (Exit <$> (string' "exit" *> pIntLit))

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
type Eval = State (Env, Bool)

getEnv :: Eval Env
getEnv = fst <$> get

getAssertsFlag :: Eval Bool
getAssertsFlag = snd <$> get

modifyEnv :: (Env -> Env) -> Eval ()
modifyEnv f = modify (\(env, assertsOn) -> (f env, assertsOn))

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
    env <- getEnv
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

newScope :: Eval a -> Eval a
newScope ma = do
  env <- getEnv
  a <- ma
  modifyEnv (\env' -> drop (length env' - length env) env')
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

evalSt :: Statement -> Eval Int
evalSt s = case s of
  Assign x e -> do
    v <- evalExp e
    modifyEnv (updateEnv x v)
    pure 0
  While e p -> do
    v <- evalExp e
    case v of
      Right True -> do
        code <- newScope (evalProg p)
        if code /= 0 then
          pure code
        else
          evalSt (While e p)
      Right False -> pure 0
      _       -> error "type error: expected a Bool condition in \"while\" expression"
  If e p1 p2 -> do
    v <- evalExp e
    case v of
      Right b -> if b then newScope (evalProg p1)
                      else newScope (evalProg p2)
      _       -> error "type error: expected a Bool condition in \"if\" expression"
  Block p ->
    newScope (evalProg p)
  Exit code -> pure code
  Assert exp -> do
    res <- evalExp exp
    assertsOn <- getAssertsFlag
    case (assertsOn, res) of
      (True, Right False) -> pure 1
      _ -> pure 0

evalProg :: Program -> Eval Int
evalProg (s:ss) = do
  code <- evalSt s
  if code /= 0 then pure code else evalProg ss
evalProg [] = pure 0


-- interpreter
--------------------------------------------------------------------------------

-- The exit code could also be returned here.
-- However, it was not part of the exam.
runProg :: Bool -> String -> (Env, Bool)
runProg assertsOn str = case runParser pSrc str of
  Nothing     -> error "parse error"
  Just (p, _) -> execState (evalProg p) ([], assertsOn)

p1 :: String
p1 = "x := 10; y := 20; {z := x + y}; x := x + 100"

p2 :: String
p2 = "x := 1; while x < 10 do x := x + 1 end"

p3 :: String
p3 = "x := 1; while x < 10 do x := x + 1; assert x < 6 end; exit 42; y := 1"

tests :: [Bool]
tests =
  [ rightmost ex1 == 13

  , mostCommon ex1 == 11
  , mostCommon (Leaf 2) == 2
  , mostCommon (BNode (Leaf 2) [1,1,2,3,4,1,5]) == 1
  , mostCommon (ANode 2 [BNode (Leaf 2) [1,1,2,3,4,1,5], Leaf 2]) == 2

  , labelWithAlphabet ex1 == ANode (2,'a') [ BNode (Leaf (3,'b')) [(5,'c'),(7,'d'),(11,'e')], Leaf (11,'f'), ANode (13,'g') []]
  , labelWithAlphabet (Leaf 3) == Leaf (3,'a')
  , labelWithAlphabet (BNode (Leaf 3) [7,8]) == (BNode (Leaf (3,'a')) [(7,'b'),(8,'c')])

  , lookupReplace [(2,True), (3,False), (4,False), (5,True), (7,True), (11,False), (13,False)] ex1 == Just (ANode True [ BNode (Leaf False) [True,True,False], Leaf False, ANode False []])
  , lookupReplace [(2,True), (3,False), (4,False)] ex1 == Nothing
  , lookupReplace [(1,'a')] (Leaf 1) == Just (Leaf 'a')
  , lookupReplace [(1,'a')] (BNode (Leaf 1) [1,1]) == Just (BNode (Leaf 'a') "aa")
  , lookupReplace [(1,'a')] (BNode (Leaf 1) [1,2,1]) == Nothing

  , runProg False p1 == ([("y",Left 20),("x",Left 110)],False)
  , runProg True  p1 == ([("y",Left 20),("x",Left 110)],False)

  , runProg False p2 == ([("x",Left 10)],False)
  , runProg True  p2 == ([("x",Left 10)],False)

  -- y is not set to 1
  , runProg False p3 == ([("x",Left 10)],False)
  -- y is not set to 1, x is only incremented until 6
  , runProg True  p3 == ([("x",Left 6)],True)

  ]
