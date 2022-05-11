{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Gy13 where

import Control.Applicative
import Control.Monad
import Data.Char

--------------------------------------------------------------------------------
-- Kisfeladat
--------------------------------------------------------------------------------

data D a b = Con1 Int a | Con2 Bool b | Con3 Bool (Maybe b) [b]
  deriving (Eq, Show)


-- Task #1: Impelement a Functor instance for the type given above!

instance Functor (D a) where
  fmap f (Con1 x a) = Con1 x a
  fmap f (Con2 x b) = Con2 x (f b)
  -- fmap f (Con3 x Nothing bs) = Con3 x Nothing (fmap f bs)
  -- fmap f (Con3 x (Just b) bs) = Con3 x (Just (f b)) (fmap f bs)
  fmap f (Con3 x b bs) = Con3 x (fmap f b) (fmap f bs)
-- Tests:
-- ∙ fmap (+10) (Con1 10 False)                == Con1 10 False
-- ∙ fmap (+10) (Con2 False 7)                 == Con2 False 17
-- ∙ fmap (+10) (Con3 False Nothing [0, 1, 2]) == Con3 False Nothing [10,11,12]
-- ∙ fmap (+10) (Con3 False (Just 5) [])       == Con3 False (Just 15) []


-- Task #2: Impelement a Foldable instance for the type given above!

instance Foldable (D a) where
    foldr f acc (Con1 x a) = acc
    foldr f acc (Con2 x b) = f b acc
    foldr f acc (Con3 x b bs) = foldr f (foldr f acc bs) b

--------------------------------------------------------------------------------
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

data List a b
  = Nil
  | Cons a b (List a b)
  deriving (Show)
-----------------------------------
-- Értelemszerűen kell ezeket megadni.

instance (Eq a, Eq b) => Eq (List a b) where
  Nil           == Nil              = True
  (Cons a b bs) == (Cons a' b' bs') = a == a' && b == b' && bs == bs'
  _             == _                = False

instance Functor (List a) where
  fmap f Nil           = Nil
  fmap f (Cons a b bs) = Cons a (f b) (fmap f bs)

instance Foldable (List a) where
  foldr f acc Nil           = acc
  foldr f acc (Cons a b bs) = f b (foldr f acc bs)

instance Traversable (List a) where
  traverse f Nil           = pure Nil
  traverse f (Cons a b bs) = Cons <$> pure a <*> f b <*> traverse f bs

{-
data List a b
  = Nil
  | Cons a b (List a b)
  deriving (Show)
-}
-- Írj egy függvényt, ami két listára bont egy List a b-t!
unpack :: List a b -> ([a], [b])
unpack Nil           = ([],[])
unpack (Cons a b bs) = (a:xs,b:ys) where
    (xs,ys) = unpack bs
{-
Pl.
unpack (Cons True 10 (Cons False 0 Nil)) == ([True, False], [10, 0])
unpack (Nil :: List Int Int) == ([], [])
unpack (Cons True False Nil) == ([True], [False])
-}

-- Írj egy függvényt, ami megfordítja az `a` típusú elemek sorrendjét a listában!
reverseAs :: List a b -> List a b
reverseAs xs = let (as,bs) = unpack xs in pack (reverse as, bs) where
    pack :: ([a],[b]) -> List a b
    pack ([],    []    ) = Nil
    pack ((y:ys),(z:zs)) = Cons y z $ pack (ys,zs)
{-
Pl.
reverseAs (Cons True 10 (Cons False 0 Nil)) == Cons False 10 (Cons True 0 Nil)
reverseAs (Cons True False (Cons False True Nil)) == Cons False False (Cons True True Nil)
-}

data Tree a b
  = Leaf  a b
  | Node (Tree a b) (Tree a b)
  deriving (Eq, Show)


-- Írj egy függvényt, ami Tree a b-ből visszaad egy List a b-t,
-- amiben a levelek értékei vannak balról-jobbra bejárási sorrendben!
toList :: Tree a b -> List a b
toList (Leaf a b) = Cons a b Nil
toList (Node l r) = (toList l) +++ (toList r) where
    Nil         +++ ys = ys
    Cons x y xs +++ ys = Cons x y (xs +++ ys)

toList' t = execState (go t) Nil where
    go (Leaf a b) = modify (Cons a b)
    go (Node l r) = do
        go r
        go l

{-
Pl.
toList (Node (Leaf True 10) (Leaf False 20)) == Cons True 10 (Cons False 20 Nil)
toList (Leaf 20 30) == Cons 20 30 Nil
toList (Node (Leaf False 10) (Node (Leaf True 20) (Leaf True 30))) == Cons False 10 (Cons True 20 (Cons True 30 Nil))
-}

-- Írj egy függvényt State monád használatával,
-- ami egy Tree a b-ben az a típusú értékeket beszámozza balról-jobbra bejárási sorrendben!
-- A számozás 0-ról induljon.
labelAs :: Tree a b -> Tree (a, Int) b
labelAs t = evalState (go t) 0 where
    go (Leaf a b) = do
        n <- get
        put $ n + 1
        pure $ Leaf (a,n) b
    go (Node l r) = do
        l' <- go l
        r' <- go r
        pure $ Node l' r'
{-
Pl.
labelAs (Node (Leaf True 10) (Leaf False 20)) == Node (Leaf (True,0) 10) (Leaf (False,1) 20)
labelAs (Leaf True True) == Leaf (True,0) True
labelAs (Node (Leaf False 10) (Node (Leaf True 20) (Leaf True 30))) == Node (Leaf (False,0) 10) (Node (Leaf (True,1) 20) (Leaf (True,2) 30))
-}

--------------------------------------------------------------------------------
data BinaryTree a = BinaryLeaf a | BinaryNode (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

instance Functor BinaryTree where
    fmap = undefined

instance Foldable BinaryTree where
    foldr = undefined

instance Traversable BinaryTree where
    -- traverse :: Applicative f => (a -> f b) -> Binary a -> f (Binary b)
    traverse = undefined

data RoseTree a = Branch a [RoseTree a] deriving (Show, Eq)

r1 :: RoseTree Integer
r1 = Branch 0 []

r2 :: RoseTree Integer
r2 = Branch 2
    [Branch 3 [Branch 5 []]
    ,Branch 7 [Branch 9 [], Branch 11 []]]

instance Functor RoseTree where
    fmap = undefined

instance Foldable RoseTree where
    foldr = undefined

instance Traversable RoseTree where
    -- traverse :: Applicative f => (a -> f b) -> RoseTree a -> f (RoseTree b)
    traverse = undefined


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
  deriving (Eq, Show)

type Program = [Statement]
type Name    = String

data Statement
  = Assign Name Exp         -- x := e
  | While Exp Program       -- while e do p1 end
  | For Name Exp Exp Program -- for var from e1 to e2 do p1 end
  | If Exp Program Program  -- if e then p1 else p2 end
  | Block Program           -- {p1}  (lokális scope)
  deriving (Eq, Show)


-- While parser
--------------------------------------------------------------------------------

{-
Parser a While nyelvhez. A szintaxist az Exp és Statement definíciónál látahtó
fenti kommentek összegzik, továbbá:

  - mindenhol lehet whitespace tokenek között
  - a Statement-eket egy Program-ban válassza el ';'
  - Az operátorok erőssége és assszociativitása a következő (csökkenő erősségi sorrendben):
      *  (bal asszoc)
      +  (bal asszoc)
      <  (nem asszoc)
      == (nem asszoc)
      && (jobb asszoc)
      || (jobb asszoc)
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
keywords = ["not", "while", "for", "do", "from", "to", "if", "then", "else", "end", "true", "false"]

pIdent :: Parser String
pIdent = do
  x <- some (satisfy isLetter) <* ws
  if elem x keywords
    then empty
    else pure x

pKeyword :: String -> Parser ()
pKeyword str = do
  string str
  mc <- optional (satisfy isLetter)
  case mc of
    Nothing -> ws
    Just _  -> empty

pBoolLit :: Parser Bool
pBoolLit = (True  <$ pKeyword "true")
       <|> (False <$ pKeyword "false")

pIntLit :: Parser Int
pIntLit = read <$> (some (satisfy isDigit) <* ws)

pAtom :: Parser Exp
pAtom = (BoolLit <$> pBoolLit)
      <|> (IntLit <$> pIntLit)
      <|> (Var <$> pIdent)
      <|> (char' '(' *> pExp <* char' ')')

pNot :: Parser Exp
pNot =
      (Not <$> (pKeyword "not" *> pAtom))
  <|> pAtom

pMul :: Parser Exp
pMul = foldl1 Mul <$> sepBy1 pNot (char' '*')

pAdd :: Parser Exp
pAdd = foldl1 Add <$> sepBy1 pMul (char' '+')

pLt :: Parser Exp
pLt = do
  e <- pAdd
  (Lt e <$> (string' "<"  *> pAdd)) <|> pure e

pEq :: Parser Exp
pEq = do
  e <- pLt
  (Lt e <$> (string' "==" *> pLt)) <|> pure e

pAnd :: Parser Exp
pAnd = foldr1 And <$> sepBy1 pEq (string' "&&")

pOr :: Parser Exp
pOr = foldr1 Or <$> sepBy1 pAnd (string' "||")

pExp :: Parser Exp
pExp = pOr

pProgram :: Parser Program
pProgram = sepBy pStatement (char' ';')

pStatement :: Parser Statement
pStatement =
        (Assign <$> pIdent <*> (string' ":=" *> pExp))
    <|> (While <$> (pKeyword "while" *> pExp)
               <*> (pKeyword "do" *> pProgram <* pKeyword "end"))
    <|> (For <$> (pKeyword "for" *> pIdent) <*> (pKeyword "from" *> pExp) <*> (pKeyword "to" *> pExp) <*> (pKeyword "do" *> pProgram <* pKeyword "end"))
    <|> (If <$> (pKeyword "if"   *> pExp)
            <*> (pKeyword "then" *> pProgram)
            <*> (pKeyword "else" *> pProgram <* pKeyword "end"))
    <|> (Block <$> (char' '{' *> pProgram <* char' '}'))

pSrc :: Parser Program
pSrc = ws *> pProgram <* eof

------------------------------------------------------------

type Val  = Either Int Bool
type Env  = [(Name, Val)]
type Eval = State Env

evalExp :: Exp -> Eval Val
evalExp e = case e of
  Add e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (Left n1, Left n2) -> pure (Left (n1 + n2))
      _                  -> error "type error in + argument"
  Mul e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (Left n1, Left n2) -> pure (Left (n1 * n2))
      _                  -> error "type error in * argument"
  Or e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (Right b1, Right b2) -> pure (Right (b1 || b2))
      _                    -> error "type error in || argument"
  And e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (Right b1, Right b2) -> pure (Right (b1 && b2))
      _                    -> error "type error in && argument"
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
    env <- get
    case lookup x env of
      Nothing -> error ("variable not in scope: " ++ x)
      Just v  -> pure v
  IntLit n ->
    pure (Left n)
  BoolLit b ->
    pure (Right b)
  Not e -> do
    v <- evalExp e
    case v of
      Right b -> pure (Right (not b))
      _       -> error "type error in \"not\" argument"

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
      Right b -> if b then newScope (evalProg p) >> evalSt (While e p)
                      else pure ()
      Left _  -> error "type error: expected a Bool condition in \"while\" expression"
  For x e1 e2 p -> do
      v1 <- evalExp e1
      v2 <- evalExp e2
      case (v1,v2) of
          (Left v1', Left v2') | v1' < v2' -> {-kell még: x-nek v1'-et értékül adni-} {-x új scope-ban létezzen csak-} newScope (evalProg p) {-kell még: x := x + 1; -}
          {-kell még: v1' >= v2', ekkor a végére értünk és többet nem kell futtatni. -}
          _ -> error "type error in statement \"for\""
  If e p1 p2 -> do
    v <- evalExp e
    case v of
      Right b -> if b then newScope (evalProg p1)
                      else newScope (evalProg p2)
      Left _ -> error "type error: expected a Bool condition in \"if\" expression"
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