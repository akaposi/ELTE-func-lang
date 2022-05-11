
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, InstanceSigs, BangPatterns #-}
{-# language ScopedTypeVariables #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Debug.Trace
import Data.Char    -- isSpace, isDigit

--------------------------------------------------------------------------------

-- régi minták:
-- https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2019-20-2/vizsga_minta


--------------------------------------------------------------------------------

data List12 a = Nil12 | Cons1 a (List12 a) | Cons2 a a (List12 a)
  deriving (Eq, Show)

instance Functor List12 where
  fmap :: (a -> b) -> List12 a -> List12 b
  fmap f Nil12 = Nil12
  fmap f (Cons1 a as) = Cons1 (f a) (fmap f as)
  fmap f (Cons2 a1 a2 as) = Cons2 (f a1) (f a2) (fmap f as)

instance Foldable List12 where
  -- egyiket definiáld

  foldr :: (a -> b -> b) -> b -> List12 a -> b
  foldr f b Nil12 = b
  foldr f b (Cons1 a as) = f a (foldr f b as)
  foldr f b (Cons2 a1 a2 as) = f a1 (f a2 (foldr f b as))

  foldMap = undefined

instance Traversable List12 where
  traverse :: Applicative f => (a -> f b) -> List12 a -> f (List12 b)
  traverse f Nil12 = pure Nil12
  traverse f (Cons1 a as) = Cons1 <$> f a <*> traverse f as
  traverse f (Cons2 a1 a2 as) = Cons2 <$> f a1 <*> f a2 <*> traverse f as



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


-- Kis feladatok 1. (nehezebb, mint ami vizsgában várható)
--------------------------------------------------------------------------------

data RoseTree a = Branch a [RoseTree a]
  deriving (Ord, Show)

ex1 :: RoseTree Int
ex1 = Branch 2 $
      [ Branch 3 $
          [ Branch 11 [] -- Leaf x ~ Branch x []
          ]
      , Branch 5 $ []
      , Branch 7 $
          [ Branch 13 []
          ]
      ]

-- Írd meg az alábbi instance-okat!
instance Eq a => Eq (RoseTree a) where
  Branch a ts == Branch a' ts' = a == a' && ts == ts'

instance Functor RoseTree where
  fmap f (Branch a ts) = Branch (f a) (fmap (fmap f) ts)

instance Foldable RoseTree where
  -- elég az egyiket definiálni

  foldr f b (Branch a ts) = f a (foldr (\t b -> foldr f b t) b ts)

  foldMap f (Branch a ts) = f a <> foldMap (foldMap f) ts

instance Traversable RoseTree where
  traverse f (Branch a ts) = Branch <$> f a <*> traverse (traverse f) ts

-- Add vissza az "a" típusú elemek számát egy fában!
countElems :: RoseTree a -> Int
countElems = length

-- Add vissza a maximális "a" értéket egy fából!
maxElem :: Ord a => RoseTree a -> a
maxElem = maximum

-- Számozd be bal-jobb bejárási sorrendben a fát!
label :: RoseTree a -> RoseTree (a, Int)
label t = evalState (traverse go t) 0 where
  go :: a -> State Int (a, Int)
  go a = do
    n <- get
    put $ n + 1
    pure (a, n)

-- Írj egy függvényt, ami egy fában az összes "n :: Int" értéket kicseréli az
-- adott "[a]" lista n-edik elemére! Ha "n" bárhol nagyobb vagy egyenlő mint a
-- lista hossza, akkor legyen a végeredmény Nothing.
transformWithList :: [a] -> RoseTree Int -> Maybe (RoseTree a)
transformWithList as t = traverse go t where
  len  = length as
  go n = if n < len then Just $ as !! n else Nothing


-- Feladatok 2. (könnyebb sor, inkább tipikus)
--------------------------------------------------------------------------------

data MaybeTree a
  = Node (MaybeTree a) (Maybe a) (MaybeTree a)
  | Leaf a
  deriving (Show)

-- Írd meg az instance-okat!
instance (Eq a) => Eq (MaybeTree a) where
  Node l a r == Node l' a' r' = l == l' && a == a' && r == r'
  Leaf a     == Leaf a'       = a == a'
  _          == _             = False

instance Functor MaybeTree where
  fmap f (Node l a r) = Node (fmap f l) (fmap f a) (fmap f r)
  fmap f (Leaf a)     = Leaf (f a)

instance Foldable MaybeTree where
  -- egyiket elég definiálni
  foldr f b (Node l a r) = foldr f (foldr f (foldr f b r) a) l
  foldr f b (Leaf a)     = f a b

  foldMap f (Node l a r) = foldMap f l <> foldMap f a <> foldMap f r
  foldMap f (Leaf a)     = f a

instance Traversable MaybeTree where
  traverse f (Node l a r) = Node <$> traverse f l <*> traverse f a <*> traverse f r
  traverse f (Leaf a)     = Leaf <$> f a

-- Számold meg a tárolt `Just` konstruktorokat a fában.
countJusts :: MaybeTree a -> Int
countJusts = go where
  go (Leaf _)     = 0
  go (Node l a r) = go l + (case a of Just _ -> 1; _ -> 0) + go r

-- Add vissza csak a `Leaf`-ekben tárolt értékek listáját.
leaves :: MaybeTree a -> [a]
leaves t = go t [] where
  go (Node l _ r) as = go l (go r as)
  go (Leaf a)     as = a:as

-- Egy fában told el az összes "a" elemet egy pozícióval jobbra, a balról-jobbra
-- bejárási sorrendben. A leginkább baloldali elem helyére kerüljön egy megadott
-- "default" érték.

-- Tipp: használd a `State a`-t a bejáráshoz! Az állapot legyen legutóbb bejárt
-- `a` típusú érték, vagy pedig az adott default érték, ha még nem jártunk be
-- egy elemet sem.

shiftElems :: a -> MaybeTree a -> MaybeTree a
shiftElems a t = evalState (traverse (\a -> get <* put a) t) a

-- példák a működésre:
--   shiftElems 10 (Leaf 0) == Leaf 10
--   shiftElems 10 (Node (Leaf 0) (Just 1) (Leaf 2)) == Node (Leaf 10) (Just 0) (Leaf 1)


-- Feladatok 3.
--------------------------------------------------------------------------------

data List a b
  = Nil
  | Cons a b (List a b)
  deriving (Show)

-- Írd meg az instance-okat!
instance (Eq a, Eq b) => Eq (List a b) where
  Nil          == Nil             = True
  Cons a b abs == Cons a' b' abs' = a == a' && b == b' && abs == abs'
  _            == _               = False

instance Functor (List c) where
  fmap f Nil = Nil
  fmap f (Cons c a cas) = Cons c (f a) (fmap f cas)

instance Foldable (List c) where
  foldr f b Nil = b
  foldr f b (Cons _ a cas) = f a (foldr f b cas)

  foldMap f Nil = mempty
  foldMap f (Cons _ a cas) = f a <> foldMap f cas

instance Traversable (List c) where
  traverse f Nil = pure Nil
  traverse f (Cons c a cas) = Cons c <$> f a <*> traverse f cas

-- Add vissza a tárolt értékek listáit!
unpack :: List a b -> ([a], [b])
unpack Nil = ([], [])
unpack (Cons a b abs) = case unpack abs of
  (as, bs) -> (a:as, b:bs)

-- pl:
-- unpack (Cons True 10 (Cons False 0 Nil)) == ([True, False], [10, 0])
-- unpack (Nil :: List Int Int) == ([], [])
-- unpack (Cons True False Nil) == ([True], [False])

-- Fordítsd meg csak az "a" típusú értékek sorrendjét egy listában, a "b"
-- típusúkat hagyd változatlanul!
reverseAs :: List a b -> List a b
reverseAs abs = pack (reverse as) bs where

  pack :: [a] -> [b] -> List a b
  pack (a:as) (b:bs) = Cons a b (pack as bs)
  pack _      _      = Nil

  (as, bs) = unpack abs

-- pl:
-- reverseAs (Cons True 10 (Cons False 0 Nil)) == Cons False 10 (Cons True 0 Nil)
-- reverseAs (Cons True False (Cons False True Nil)) == Cons False False (Cons True True Nil)

data Tree' a b
  = Leaf'  a b
  | Node' (Tree' a b) (Tree' a b)
  deriving (Show)

-- Írd meg a következő instance-okat!
instance (Eq a, Eq b) => Eq (Tree' a b) where
  Leaf' a b == Leaf' a' b' = a == a' && b == b'
  Node' l r == Node' l' r' = l == l' && r == r'
  _         == _           = False

instance Functor (Tree' c) where
  fmap f (Leaf' c a) = Leaf' c (f a)
  fmap f (Node' l r) = Node' (fmap f l) (fmap f r)

instance Foldable (Tree' c) where
  foldr f b (Leaf' c a) = f a b
  foldr f b (Node' l r) = foldr f (foldr f b r) l

  foldMap f (Leaf' c a) = f a
  foldMap f (Node' l r) = foldMap f l <> foldMap f r

instance Traversable (Tree' c) where
  traverse f (Leaf' c a) = Leaf' c <$> f a
  traverse f (Node' l r) = Node' <$> traverse f l <*> traverse f r

-- Add vissza "List"-ben a fák leveleit, balról-jobbra bejárási sorrendben!
treeToList :: Tree' a b -> List a b
treeToList t = go t Nil where
  go (Leaf' a b) accum = Cons a b accum
  go (Node' l r) accum = go l (go r accum)

-- Számozd meg csak az "a" típusú értékeket egy fában balról-jobbra bejárási
-- sorrendben!
labelAs :: Tree' a b -> Tree' (a, Int) b
labelAs t = evalState (go t) 0 where
  go :: Tree' a b -> State Int (Tree' (a, Int) b)
  go (Leaf' a b) = do
    n <- get
    put $ n + 1
    pure $ Leaf' (a, n) b
  go (Node' l r) =
    Node' <$> go l <*> go r


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

1. Feladat:
  - Egészítsd ki a nyelvet pár típusú értékekkel!
  - Szintaxis:  (e1, e2)   : pár képzés
                fst e      : első mező
                snd e      : második mező
  - fst és snd precendenciája legyen ugyanaz, mint "not"-é (tipp: a not parser függvényt egészítsük ki!)
  - Egészítsd ki a parser-t és az interpretert.
    1. Egészítsd ki az Exp definiciót három konstruktorral, ami reprezentálja párképzést, fst-t és snd-t.
    2. Egészítsd ki a parsert.
    3. Egészítsd ki a Val definíciót olyan konstruktorral, ami értékek párját reprezentálja.
    4. Egészítsd ki az interpretert. A kiértékelés dobjon hibát, ha fst és snd nem pár típusú értéket kap,
       egyébként értelemszerűen az első/második mezőt vegyük ki.


2. Egészítsük ki a nyelvet `readInt` és `readBool` kifejezésekkel!
   Ha egy ilyen kifejezést kiértékelünk, akkor egy `String`
   inputból egy `Int` vagy `Bool` típusú értéket olvasunk ki.

   A `String` inputot a kiértékelőben az állapotba vegyük fel,
   tehát az `Env` helyett az állapot legyen mindenhol `(Env, String)`!

   - A `readInt` kiértékelése kiolvas egy pozitív `Int` literált
     az állapotból és visszaadja azt.

   - Analóg módon a `readBool` kiolvas egy `Bool` értéket,
     kisbetűs "true" és "false" literálként megadva.

   Az olvasáshoz felhasználhatjuk a parser-ben már létező
   függvényeket!

   Az `evalExp` új típusa legyen `Exp -> State (Env, String) Val`,
   mivel az új műveletek kiértékelése megváltoztatja az állapotot.

   A `run` új típusa legyen `String -> String -> (Env, String)`. Az első
   `String` paraméter a forrsákód, a második pedig az input.  A végeredményben
   szerepel a végső `String` állapot.
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

  | ReadInt
  | ReadBool
  deriving (Eq, Show)

{-
Változónév: nemüres alfabetikus string

Kötési erősségek csökkenő sorrendben:
  - atom: zárójelezett kifejezés, pár kifejezés, literál, változónév
  - not/fst/snd alkalmazás
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
keywords = ["not", "fst", "snd", "true", "false", "while", "if", "do", "end", "then", "else", "readInt", "readBool"]

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

pFinishPair :: Exp -> Parser Exp
pFinishPair e1 = do
  char' ','
  e2 <- eqExp
  char' ')'
  pure (Pair e1 e2)

pFinishParens :: Exp -> Parser Exp
pFinishParens e = do
  char' ')'
  pure e

pPairOrParens :: Parser Exp
pPairOrParens = do
  char' '('
  e1 <- eqExp
  pFinishPair e1 <|> pFinishParens e1

-- pPairOrParensSlow :: Parser Exp
-- pPairOrParensSlow =
--         (char' '(' *> eqExp <* char' ')')
--    <|>  (Pair <$> (char' '(' *> eqExp)                   -- feleslegesen olvassuk kétszer az első eqExp-et
--               <*> (char' ',' *> eqExp <* char' ')'))


atom :: Parser Exp
atom =
        (Var <$> ident')
    <|> (IntLit <$> posInt')
    <|> (BoolLit True <$ keyword' "true")
    <|> (BoolLit False <$ keyword' "false")
    <|> (ReadInt <$ keyword' "readInt")
    <|> (ReadBool <$ keyword' "readBool")
    <|> pPairOrParens

pNotFstSnd :: Parser Exp
pNotFstSnd =
       (keyword' "not" *> (Not <$> atom))
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

evalExp :: Exp -> State (Env, String) Val       -- alternatív: Env -> Exp -> State String Val
evalExp e = do
  (env, str) <- get
  case e of
    IntLit n  -> pure $ VInt n
    BoolLit b -> pure $ VBool b
    Add e1 e2 -> do
      v1 <- evalExp e1
      v2 <- evalExp e2
      case (v1, v2) of
        (VInt n1, VInt n2) -> pure $ VInt (n1 + n2)
        _                  -> error "type error"
    Sub e1 e2 -> do
      v1 <- evalExp e1
      v2 <- evalExp e2
      case (v1, v2) of
        (VInt n1, VInt n2) -> pure $ VInt (n1 - n2)
        _                  -> error "type error"
    Mul e1 e2 -> do
      v1 <- evalExp e1
      v2 <- evalExp e2
      case (v1, v2) of
        (VInt n1, VInt n2) -> pure $ VInt (n1 * n2)
        _                  -> error "type error"
    Or e1 e2 -> do
      v1 <- evalExp e1
      v2 <- evalExp e2
      case (v1, v2) of
        (VBool b1, VBool b2) -> pure $ VBool (b1 || b2)
        _                    -> error "type error"
    And e1 e2 -> do
      v1 <- evalExp e1
      v2 <- evalExp e2
      case (v1, v2) of
        (VBool b1, VBool b2) -> pure $ VBool (b1 && b2)
        _                    -> error "type error"
    Eq e1 e2 -> do
      v1 <- evalExp e1
      v2 <- evalExp e2
      case (v1, v2) of
        (VBool b1, VBool b2) -> pure $ VBool (b1 == b2)
        (VInt n1,  VInt n2 ) -> pure $ VBool (n1 == n2)
        _                    -> error "type error"
    Not e -> do
      v <- evalExp e
      case v of
        VBool b -> pure $ VBool (not b)
        _       -> error "type error"

    Var x -> case lookup x env of
      Just v  -> pure v
      Nothing -> error $ "name not in scope: " ++ x

    Pair e1 e2 -> VPair <$> evalExp e1 <*> evalExp e2

    Fst e -> do
      v <- evalExp e
      case v of
        VPair v1 _ -> pure v1
        _ -> error "type error"

    Snd e -> do
      v <- evalExp e
      case v of
        VPair _ v2 -> pure v2
        _ -> error "type error"

    ReadInt ->
      case runParser posInt' str of
        Just (n, str') -> do
          put (env, str')
          pure (VInt n)
        Nothing ->
          error "can't read Int from input"

    ReadBool ->
      let parseBool = (True  <$ string' "true")
                  <|> (False <$ string' "false")

      in case runParser parseBool str of
        Just (b, str') -> do
          put (env, str')
          pure (VBool b)
        Nothing ->
          error "can't read Int from input"


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
  len <- fmap (length . fst) get
  a <- ma
  modify $ \(env', str) -> (take len env', str)
  pure a

evalStatement :: Statement -> State (Env, String) ()
evalStatement st = case st of

  -- ha x nincs env-ben, akkor vegyük fel az értékkel,
  -- egyébként pedig írjuk át az értékét
  Assign x e -> do
    val <- evalExp e
    modify $ \(env, str) -> (updateEnv x val env, str)

  -- while-on belüli új változók kívül nem látszanak
  While e p -> do
    v <- evalExp e
    case v of
      VBool True  -> inNewScope (evalProgram p) >> evalStatement (While e p)
      VBool False -> pure ()
      VInt _      -> error "type error"
      VPair _ _   -> error "type error"

  -- if ágakban új változók kívül nem látszanak
  If e p1 p2 -> do
    v <- evalExp e
    case v of
      VBool True  -> inNewScope (evalProgram p1)
      VBool False -> inNewScope (evalProgram p2)
      VInt _      -> error "type error"
      VPair _ _   -> error "type error"

evalProgram :: Program -> State (Env, String) ()
evalProgram = mapM_ evalStatement

run :: String -> String -> (Env, String)
run source inpstr = case runParser (topLevel program) source of
  Just (prog, _) -> execState (evalProgram prog) ([], inpstr)
  Nothing        -> error "parse error"

p1 :: String
p1 = "i := 10; acc := 0; while not (i == 0) do acc := acc + i; i := i - 1 end"

p2 :: String
p2 = "x := (10, 20); y := fst x; z := snd x"

p3 :: String
p3 = "x := ((10, true), 20); y := fst (fst x)"

p4 :: String
p4 = "x := readInt; y := x + 100"

p5 :: String
p5 = "x := (readInt, readBool); y := readInt; z := fst x + y"
