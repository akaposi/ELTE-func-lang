
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, InstanceSigs, BangPatterns #-}
{-# language ScopedTypeVariables #-}
{-# options_ghc -Wincomplete-patterns #-}


-- Teams chat: szavazás/javaslat utolsó 2 előadás témájára
--   hatékonyság
--   monád transzformerek
--   generikus programozás
--   lencsék (lenses)
--   fordítás Haskell-ről gépi kódra (nagy vonalakban)
--   parser lib kiegészítése informatív hibaüzenetekkel

--------------------------------------------------------------------------------


-- Vizsga jellegű feladatok:
--   1. Kis feladatok (10-12 pont)
--   2. Parser/interpreter kiegészítés (8-10 pont)

-- Résztpontok vannak (lehet kapni nem funkcionális megoldásra is részpontot)

-- Ponthatár
--   2: 10-11
--   3: 12-14
--   4: 15-16
--   5: 17-20
--------------------------------------------------------------------------------

import Control.Applicative
import Control.Monad
import Debug.Trace
import Data.Char    -- isSpace, isDigit

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

data Either' a b = Left' a | Right' b | Both a b
  deriving (Eq, Show)

instance Functor (Either' c) where
  fmap :: (a -> b) -> Either' c a -> Either' c b
  fmap f (Left' c)  = Left' c
  fmap f (Right' a) = Right' (f a)
  fmap f (Both c a) = Both c (f a)

class Eq' a where            -- instance-nál elég csak az egyik függvényt megadni (a másik definíció default lesz)
  eq :: a -> a -> Bool
  eq  x y = not (neq x y)

  neq :: a -> a -> Bool
  neq x y = not (eq x y)

instance Foldable (Either' c) where
  -- választhatok foldr és foldMap implementáció között

  foldr :: (a -> b -> b) -> b -> Either' c a -> b    -- (a -> b -> b) -> b -> [a] -> b
  foldr f b (Left' c)  = b
  foldr f b (Right' a) = f a b
  foldr f b (Both c a) = f a b

  -- Monoid m: két metódust
  --     mempty :: m                 mempty <> x = x    és    x <> mempty = x
  --     (<>)   :: m -> m -> m       asszociatív

  -- instance Monoid [a] where
  --   mempty = []
  --   (<>) = (++)

  -- instance Monoid (a -> a) where
  --   mempty = id
  --   (<>)   = (.)

  foldMap :: Monoid m => (a -> m) -> Either' c a -> m
  foldMap f (Left' c)  = mempty
  foldMap f (Right' a) = f a
  foldMap f (Both c a) = f a

  -- foldMap :: Monoid m => (a -> m) -> [a] -> m
  -- foldMap f []     = mempty
  -- foldMap f (a:as) = f a <> foldMap f as

  -- Opcionális feladat:
  --    hogyan lehet foldMap-ből foldr-t definiálni és fordítva
  --    (default definíciók a Foldable osztályban)
  --    tipp: használjuk a Monoid instance-t (a -> a) típusra

  -- foldMap-et könnyebb definiálni sok esetben mint foldr-t!


instance Traversable (Either' c) where
  traverse :: Applicative f => (a -> f b) -> Either' c a -> f (Either' c b)
  traverse f (Left' c)  = pure (Left' c)
  traverse f (Right' a) = Right' <$> f a
  traverse f (Both c a) = Both c <$> f a   -- Both <$> pure c <*> f a

  -- traverse = fmap + Applicative mellékhatás

-- Bontsuk szét a listát a három lehetséges Either' konstruktor szerint
partition :: [Either' a b] -> ([a], [b], [(a, b)])
partition []         = ([], [], [])
partition (eab:eabs) = case partition eabs of
  (as, bs, abs) -> case eab of
    Left' a  -> (a:as, bs  , abs)
    Right' b -> (as,   b:bs, abs)
    Both a b -> (as,   bs  , (a, b):abs)

partition' :: [Either' a b] -> ([a], [b], [(a, b)])
partition' = foldr go ([], [], []) where
  go eab (as, bs, abs) = case eab of
    Left' a  -> (a:as, bs  , abs)
    Right' b -> (as,   b:bs, abs)
    Both a b -> (as,   bs  , (a, b):abs)

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]


-- Írf olyan zipWith' függvényt, ami kezelni tudja azokat az eseteket, amikor
-- valamelyik input lista üres (és a másik nemüres).
zipWith' :: (Either' a b -> c) -> [a] -> [b] -> [c]
zipWith' f []     []     = []
zipWith' f (a:as) (b:bs) = f (Both a b) : zipWith' f as bs
zipWith' f []     (b:bs) = f (Right' b) : zipWith' f [] bs
zipWith' f (a:as) []     = f (Left' a)  : zipWith' f as []


-- alkalmazzunk egy (a -> Maybe b) függvényt az "a" értékekre,
-- ha bárhol Nothing-ot ad, legyen Nothing az eredmény, egyébként
-- Just <map-elt eredmény>.

-- Tipp: bejárás Maybe Applicative-ben
mapMaybeLeft :: forall a b c. (a -> Maybe b) -> [Either' a c] -> Maybe [Either' b c]
mapMaybeLeft f xs = traverse go xs where

  -- (hivatkozás konkrét típusváltozóra külső scope-ból: language ScopedTypeVariables,
  --   "forall a b c. ..." szintaxis

  go :: Either' a c -> Maybe (Either' b c)
  go (Left' a)  = Left' <$> f a
  go (Right' b) = pure (Right' b)
  go (Both a b) = Both <$> f a <*> pure b

-- vagy go helyett:

-- traverseLeft :: Applicative f => (a -> f b) -> Either' a c -> f (Either' b c)
-- traverseLeft f (Left' a)  = Left' <$> f a
-- traverseLeft f (Right' b) = pure (Right' b)
-- traverseLeft f (Both a b) = Both <$> f a <*> pure b


data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Functor, Foldable, Traversable)

-- Levélbe tegyük be a levéltől balra levő levelek összegét.
-- Tipp: legyen az összeg State-ben tárolva.
treeSums :: Tree Int -> Tree Int
treeSums t = evalState (traverse go t) 0 where
  go :: Int -> State Int Int
  go n = do
    sum <- get
    put $ n + sum
    pure sum


-- FELADATOK (2. sor)
--------------------------------------------------------------------------------

-- (nehezebb kicsit, mint ami vizsgában várható)

data RoseTree a = Branch a [RoseTree a]
  deriving (Eq, Ord, Show)

ex1 :: RoseTree Int
ex1 = Branch 2 $
      [ Branch 3 $
          [ Branch 11 []
          ]
      , Branch 5 $ []
      , Branch 7 $
          [ Branch 13 []
          ]
      ]

instance Functor RoseTree where
  fmap = undefined

instance Foldable RoseTree where
  foldr = undefined
  foldMap = undefined

instance Traversable RoseTree where
  traverse = undefined

countElems :: RoseTree a -> Int
countElems = undefined

maxElem :: Ord a => RoseTree a -> a
maxElem = undefined

label :: RoseTree a -> RoseTree (a, Int)
label = undefined

transformWithList :: [a] -> RoseTree Int -> Maybe (RoseTree a)
transformWithList = undefined


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
  - Szintaxis:     (e1, e2)
                   fst e
                   snd e
  - fst és snd precendencia legyen ugyanaz, mint "not"-é (tipp: not parser-t egészítsük ki)
  - Egészítsd ki a parser-t és az interpretert.

  - interpreter: fst/snd típushibát dob, ha nem pár értéket kap
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
keywords = ["not", "true", "false", "while", "if", "do", "end", "then", "else", "fst", "snd"]

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
    <|> (BoolLit True <$ keyword' "true")
    <|> (BoolLit False <$ keyword' "false")
    <|> pPairOrParens

-- trade-off: ha nem faktorálunk, akkor tömörebb + modulárisabb a definíció, viszont lassabb
--   parser generátorok: hatékonyásg automatikus, viszont más hátrányok (pl hibaüzenetek, bonyolult/ronda nyelvtanok kezelése)

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


inNewScope :: State Env a -> State Env a
inNewScope ma = do
  env <- get
  let len = length env
  a <- ma
  env' <- get
  put $ take len env'
  pure a

-- evalStatement :: Statement -> Env -> Env
evalStatement :: Statement -> State Env ()
evalStatement st = case st of

  -- ha x nincs env-ben, akkor vegyük fel az értékkel,
  -- egyébként pedig írjuk át az értékét
  Assign x e -> do
    env <- get
    let val = evalExp env e
    put $ updateEnv x val env

  -- while-on belüli új változók kívül nem látszanak
  While e p -> do
    env <- get
    case evalExp env e of
      VBool True  -> inNewScope (evalProgram p) >> evalStatement (While e p)
      VBool False -> pure ()
      VInt _      -> error "type error"
      VPair _ _   -> error "type error"     -- AST/compiler fejlesztés: default _ minták helyett érdemes
                                            -- minden esetet kiírni, mivel ha AST-kiegészítjük, akkor
                                            -- minden mintaillesztést szeretnénk warning-ban látni


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
