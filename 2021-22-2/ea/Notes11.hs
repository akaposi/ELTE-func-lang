
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, InstanceSigs, BangPatterns #-}
{-# language ScopedTypeVariables #-}
{-# language LambdaCase #-}
{-# options_ghc -Wincomplete-patterns #-}


--------------------------------------------------------------------------------

-- régi minták:
--   https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2019-20-2/vizsga_minta

--------------------------------------------------------------------------------

import Control.Applicative
import Control.Monad
import Debug.Trace
import Data.Char    -- isSpace, isDigit

import Data.Foldable -- toList

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


-- FELADATOK (1. sor)
--------------------------------------------------------------------------------

data Tree a = Leaf1 a | Leaf2 a a | Node (Tree a) (Maybe (Tree a))
  deriving (Eq, Ord, Show)

   -- Node (Tree a) (Maybe (Tree a))
   --   helyett:
   -- Node1 (Tree a)
   -- Node2 (Tree a) (Tree a)

ex1 :: Tree Int
ex1 =
  Node
    (Leaf2 2 1)
    (Just (Node
      (Leaf1 10)
      (Just (Node
         (Leaf2 5 6)
         Nothing))))

-- 1
instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  -- fmap f (Leaf1 a)           = Leaf1 (f a)
  -- fmap f (Leaf2 a1 a2)       = Leaf2 (f a1) (f a2)
  -- fmap f (Node t1 Nothing)   = Node (fmap f t1) Nothing
  -- fmap f (Node t1 (Just t2)) = Node (fmap f t1) (Just (fmap f t2))

  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf1 a)     = Leaf1 (f a)
  fmap f (Leaf2 a1 a2) = Leaf2 (f a1) (f a2)
  fmap f (Node t1 mt2) = Node (fmap f t1) (fmap (fmap f) mt2)


-- 2
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f b (Leaf1 a)     = f a b
  foldr f b (Leaf2 a1 a2) = f a1 (f a2 b)
  foldr f b (Node t1 mt2) = foldr f (foldr (\t b -> foldr f b t) b mt2) t1

  -- Monoid m metódusok:
  -- (<>)   :: m -> m -> m
  -- mempty :: m

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf1 a)     = f a
  foldMap f (Leaf2 a1 a2) = f a1 <> f a2
  foldMap f (Node t1 mt2) = foldMap f t1 <> foldMap (foldMap f) mt2

-- 1
instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf1 a)     = Leaf1 <$> f a
  traverse f (Leaf2 a1 a2) = Leaf2 <$> f a1 <*> f a2
  traverse f (Node t1 mt2) = Node <$> traverse f t1 <*> traverse (traverse f) mt2

-- 1
leftmost :: Tree a -> a
-- leftmost = head . foldr (:) []

leftmost = foldr const undefined

-- leftmost (Leaf1 a) = a
-- leftmost (Leaf2 a _) = a
-- leftmost (Node t _) = leftmost t

-- 2

-- Az első "a" érték ami:
--  - Leaf2-ben van
--  - igaz rá a feltétel
findInLeaf2 :: (a -> Bool) -> Tree a -> Maybe a
findInLeaf2 f (Leaf1 _)     = Nothing
findInLeaf2 f (Leaf2 a1 a2)
  | f a1      = Just a1
  | f a2      = Just a2
  | otherwise = Nothing
findInLeaf2 f (Node t1 mt2) = case findInLeaf2 f t1 of
  Just a  -> Just a
  Nothing -> mt2 >>= findInLeaf2 f
            -- do {t2 <- mt2; findInLeaf2 f t2}

-- findInLeaf2 f (Node t1 mt2) =
--   findInLeaf2 f t1 <|> (mt2 >>= findInLeaf2 f)


-- 2
labelLeaf1s :: Tree a -> Tree (a, Int)
labelLeaf1s t = evalState (go t) 0 where
  go :: Tree a -> State Int (Tree (a, Int))
  go (Leaf1 a) = do
    n <- get
    put $ n + 1
    pure $ Leaf1 (a, n)
  go (Leaf2 a1 a2) = do
    n <- get
    pure (Leaf2 (a1, n) (a2, n))
  go (Node t1 mt2) =
    Node <$> go t1 <*> traverse go mt2


-- 3
popDefault :: a -> State [a] a
popDefault a = do
  as <- get
  case as of
    a':as -> do {put as; pure a'}
    []    -> pure a

replaceLeaf2s :: [a] -> Tree a -> Tree a
replaceLeaf2s as t = evalState (go t) as where
  go :: Tree a -> State [a] (Tree a)
  go (Leaf1 a)     = pure (Leaf1 a)
  go (Leaf2 a1 a2) = Leaf2 <$> popDefault a1 <*> popDefault a2
  go (Node t1 mt2) = Node <$> go t1 <*> traverse go mt2


-- FELADATOK (2. sor)
--------------------------------------------------------------------------------

-- (nehezebb sor, mint amit várni lehet)

data Tree' a = Node' (Either a (Tree' a)) (Tree' a) | TriLeaf a a a
  deriving (Eq, Ord, Show)

ex2 :: Tree' Int
ex2 =
  Node'
   (Left 1)
    (Node'
      (Right (TriLeaf 2 3 4))
      (TriLeaf 5 6 7))

bimapEither :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
bimapEither f g (Left a)  = Left (f a)
bimapEither f g (Right b) = Right (g b)

-- 1
instance Functor Tree' where
  fmap f (Node' t1 t2)      = Node' (bimapEither f (fmap f) t1) (fmap f t2)
  fmap f (TriLeaf a1 a2 a3) = TriLeaf (f a1) (f a2) (f a3)

-- 2
instance Foldable Tree' where
  foldr f b (Node' (Left a) t2)   = f a (foldr f b t2)
  foldr f b (Node' (Right t1) t2) = foldr f (foldr f b t2) t1
  foldr f b (TriLeaf a1 a2 a3)    = f a1 (f a2 (f a3 b))

  foldMap f (Node' (Left a) t2)   = f a <> foldMap f t2
  foldMap f (Node' (Right t1) t2) = foldMap f t1 <> foldMap f t2
  foldMap f (TriLeaf a1 a2 a3)    = f a1 <> f a2 <> f a3

-- 1
instance Traversable Tree' where
  traverse f (Node' (Left a)   t2) = Node' <$> (Left <$> f a) <*> traverse f t2
  traverse f (Node' (Right t1) t2) = Node' <$> (Right <$> traverse f t1) <*> traverse f t2
  traverse f (TriLeaf a1 a2 a3)    = TriLeaf <$> f a1 <*> f a2 <*> f a3


-- 1
leftmost' :: Tree' a -> a
leftmost' = foldr const undefined

-- 2
findComplementInTriLeaf :: (a -> Bool) -> Tree' a -> Maybe a
findComplementInTriLeaf f = go where
  go (Node' (Left _)   t2) = go t2
  go (Node' (Right t1) t2) = go t1 <|> go t2
  go (TriLeaf _ a _)
    | not (f a) = Just a
    | otherwise = Nothing

-- 2


countTriLeafs :: Tree' a -> Tree' (a, Int)
countTriLeafs t = evalState (go t) 0 where
  procVal :: a -> State Int (a, Int)
  procVal a = do
    n <- get
    put $ n + 1
    pure (a, n)

  go :: Tree' a -> State Int (Tree' (a, Int))
  go (Node' (Left a) t2) = do
    n <- get
    t2 <- go t2
    pure (Node' (Left (a, n)) t2)
  go (Node' (Right t1) t2) =
    Node' <$> (Right <$> go t1) <*> go t2
  go (TriLeaf a1 a2 a3) =
    TriLeaf <$> procVal a1 <*> procVal a2 <*> procVal a3

-- 3

-- Nemüres listával töltsünk fel egy fát periodikusan.
periodicReplace :: (a, [a]) -> Tree' a -> Tree' a
periodicReplace topAs t = evalState (traverse go t) topAs where
  go a = do
    (a', as) <- get
    case as of
      []      -> put topAs      -- kezdeti állapot
      a'':as' -> put (a'', as') -- tail-el módosítok
    pure a'


-- Parser lib
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving (Functor)

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return :: a -> Parser a
  return a = Parser $ \s -> Just (a, s)   -- nincs hatás

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s -> do {(a, s) <- f s; runParser (g a) s}

-- pontosan az üres inputot olvassuk
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- olvassunk egy karaktert az input elejéről, amire igaz egy feltétel
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- olvassunk egy konkrét karaktert
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- olvassunk egy konkrét String-et
string :: String -> Parser ()   -- String ~ [Char]
string s = mapM_ char s         -- egymás után olvasom az összes Char-t a String-ben


instance Alternative Parser where
  -- mindig hibázó parser
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- választás két parser között
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing -> g s
    res     -> res

-- Control.Applicative-ból:
--    many  :: Parser a -> Parser [a]       -- 0-szor vagy többször futtatja
--    some  :: Parser a -> Parser [a]       -- 1-szer vagy többször futtatja

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

-- Control.Applicative-ból:
-- optional :: Parser a -> Parser (Maybe a)   -- hibát értékként visszaadja (soha nem hibázik)
-- optional pa = (Just <$> pa) <|> pure Nothing

-- 0 vagy 1 eredményt olvasunk
optional_ :: Parser a -> Parser ()
optional_ pa = () <$ optional pa

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
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


-- token/ws parsing

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


-- PARSER/INTERPRETER kiegészítés
--------------------------------------------------------------------------------

{-
Feladat:

1. Egészítsük ki a nyelvet `readInt` és `readBool` kifejezésekkel!
   Ha egy ilyen kifejezést kiértékelünk, akkor egy `String`
   inputból egy `Int` vagy `Bool` típusú értéket olvasunk ki.
   A `String` inputot a kiértékelőben az állapotba vegyük fel,
   tehát az `Env` helyett az állapot legyen `(Env, String)`!

   A `readInt` kiértékelése kiolvas egy pozitív `Int` literált
   az állapotból és visszaadja azt.

   Analóg módon a `readBool` kiolvas egy `Bool` értéket,
   kisbetűs "true" és "false" literálként megadva.

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

  | ReadInt
  | ReadBool

  | Pair Exp Exp
  | Fst Exp
  | Snd Exp
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

-- változónév/kulcsszó olvasás

-- kulcsszavak:
keywords :: [String]
keywords = [
  "not", "true", "false", "while", "if", "do", "end", "then", "else", "fst", "snd",
  "readInt", "readBool"]

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

atom :: Parser Exp
atom =
        (Var <$> ident')
    <|> (IntLit <$> posInt')
    <|> (ReadInt <$ keyword' "readInt")
    <|> (ReadBool <$ keyword' "readBool")
    <|> (BoolLit True <$ keyword' "true")
    <|> (BoolLit False <$ keyword' "false")
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

eqExp :: Parser Exp
eqExp = nonAssoc Eq orExp (string' "==")

pExp :: Parser Exp
pExp = eqExp

data Val = VInt Int | VBool Bool | VPair Val Val
  deriving (Eq, Show)

type Env = [(String, Val)]

bind2 :: Monad m => m a -> m b -> ((a, b) -> m c) -> m c
bind2 ma mb f = do
  a <- ma
  b <- mb
  f (a, b)

evalExp :: Exp -> State (Env, String) Val
evalExp e = case e of
  IntLit n  -> pure $ VInt n
  BoolLit b -> pure $ VBool b
  Add e1 e2 -> bind2 (evalExp  e1) (evalExp  e2) $ \case
    (VInt n1, VInt n2) -> pure $ VInt (n1 + n2)
    _                  -> error "type error"
  Sub e1 e2 -> bind2 (evalExp  e1) (evalExp  e2) $ \case
    (VInt n1, VInt n2) -> pure $ VInt (n1 - n2)
    _                  -> error "type error"
  Mul e1 e2 -> bind2 (evalExp  e1) (evalExp  e2) $ \case
    (VInt n1, VInt n2) -> pure $ VInt (n1 * n2)
    _                  -> error "type error"
  Or e1 e2 -> bind2 (evalExp  e1) (evalExp  e2) $ \case
    (VBool b1, VBool b2) -> pure $ VBool (b1 || b2)
    _                    -> error "type error"
  And e1 e2 -> bind2 (evalExp  e1) (evalExp  e2) $ \case
    (VBool b1, VBool b2) -> pure $ VBool (b1 && b2)
    _                    -> error "type error"
  Eq e1 e2 -> bind2 (evalExp  e1) (evalExp  e2) $ \case
    (VBool b1, VBool b2) -> pure $ VBool (b1 == b2)
    (VInt n1,  VInt n2 ) -> pure $ VBool (n1 == n2)
    _                    -> error "type error"
  Not e -> evalExp  e >>= \case
    VBool b -> pure $ VBool (not b)
    _       -> error "type error"
  Var x -> do
    (env, _) <- get
    case lookup x env of
      Just v  -> pure v
      Nothing -> error $ "name not in scope: " ++ x

  Pair e1 e2 -> VPair <$> evalExp  e1 <*> evalExp  e2

  Fst e -> evalExp  e >>= \case
    VPair v1 v2 -> pure v1
    _           -> error "type error"

  Snd e -> evalExp  e >>= \case
    VPair v1 v2 -> pure v2
    _           -> error "type error"

  ReadInt -> _
  ReadBool -> _

--------------------------------------------------------------------------------

type Program = [Statement]  -- st1; st2; st3; ... st4

data Statement
  = Assign String Exp       -- x := e
  | While Exp Program       -- while e do prog end
  | If Exp Program Program  -- if e then prog1 else prog2 end
  deriving (Eq, Show)

-- Statement szintaxisban nem kell precendenciával foglalkozni, mert valójában
-- csak "atomi" konstrukció van
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
  (env', inp') <- get
  put (take len env', inp')
  pure a

-- evalStatement :: Statement -> Env -> Env
evalStatement :: Statement -> State (Env, String) ()
evalStatement st = case st of

  -- ha x nincs env-ben, akkor vegyük fel az értékkel,
  -- egyébként pedig írjuk át az értékét
  Assign x e -> do
    env <- get
    -- val = evalExp env e
    -- put $ updateEnv x val env
    undefined

  -- while-on belüli új változók kívül nem látszanak
  While e p -> do
    env <- get
    case evalExp env e of
      VBool True  -> inNewScope (evalProgram p) >> evalStatement (While e p)
      VBool False -> pure ()
      VInt _      -> error "type error"
      VPair _ _   -> error "type error"

  -- if ágakban új változók kívül nem látszanak
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
