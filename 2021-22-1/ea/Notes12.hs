
{-# language InstanceSigs, ScopedTypeVariables #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# options_ghc -Wincomplete-patterns #-}

import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Applicative -- many, some
import Data.Char           -- isDigit :: Char -> Bool
                           -- digitToInt :: Char -> Int

import Debug.Trace         -- trace :: String -> a -> a
                           -- traceShow :: Show b => b -> a -> a

import Control.Monad.State

-- 2019-20-2/vizsga_minta/minta6


-- vizsga előtt legyen meg a min 13 gyak pont, egyébként le fog dobni a neptun

-- jövő héten még egy EA szerda 16:00, akit érdekel megnézi, (szintén felveszem)
-- témára lehet még szavazni (új témát is lehet javasolni)


import Data.List
import Control.Monad.State

--------------------------------------------------------------------------------

data Tree a = Node (Tree a) (Tree a) a [a] | Leaf a
  deriving (Eq, Ord, Show)

-- 1
instance Functor Tree where
  fmap f (Node l r a as) = Node (fmap f l) (fmap f r) (f a) (fmap f as)
  fmap f (Leaf a)        = Leaf (f a)

-- 1
instance Foldable Tree where
  -- választásunk: vagy a foldr vagy a foldMap implementációt kell megadni

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f b (Node l r a as) =
    let asRes  = foldr f b as
        aasRes = f a asRes
        rRes   = foldr f aasRes r
    in  foldr f rRes l
  -- tömören:
  -- foldr f (foldr f (f a (foldr f b as)) r) l

  foldr f b (Leaf a) =
    f a b

  -- Tipp: foldMap általában könnyebb
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Node l r a as) = foldMap f l <> foldMap f r <> f a <> foldMap f as
  foldMap f (Leaf a)        = f a


--------------------------------------------------------------------------------
-- Miért elég vagy a foldMap-et vagy a foldr-t megadni?
--  Egyik függvénnyel definiálható a másik ("ekvivalens" típusok)

-- 1. verzió ("csalás" listákon keresztül)
foldr' :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr' f b ta = foldr f b (foldMap (\a -> [a]) ta)
                -- lista foldr-t az elemek listáján

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f ta = foldMap f (foldr (:) [] ta)


newtype Fun a = Fun {unFun :: a -> a}

instance Semigroup (Fun a) where
  Fun f <> Fun g = Fun (f . g)

instance Monoid (Fun a) where
  mempty = Fun id

-- 2. verzió, listák nélkül!
foldr'' :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr'' f b ta = unFun (foldMap (\a -> Fun (\b -> f a b)) ta) b
  -- class Foldable forráskód: a fenti definíciót lehet látni

foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap'' f ta = foldr (\a m -> f a <> m) mempty ta

-- class Foldable t where
--   foldr :: ...
--   foldr = ...

--   foldMap ::
--   foldMap = ...

-- Példa arra, hogy metódusok levezethetők más metódusokból
class Eq' a where
  eq :: a -> a -> Bool
  eq a a' = not (neq a a')

  neq :: a -> a -> Bool
  neq a a' = not (eq a a')

-- elég csak 1 metódus
instance Eq' Int where
  eq = (==)

--------------------------------------------------------------------------------


-- 1
instance Traversable Tree where
  -- fmap-et másoljuk, Applicative végeredményre átírni

  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Node l r a as) =
    Node <$> traverse f l <*> traverse f r <*> f a <*> traverse f as
  traverse f (Leaf a) =
    Leaf <$> f a


-- 1
rightmost :: Tree a -> a
rightmost ta = foldr1 (\_ a -> a) ta

-- rightmost (Node l r a as) = case as of
--   [] -> a
--   _  -> last as
-- rightmost (Leaf a) = a

toList :: Foldable t => t a -> [a]
toList = foldr (:) []

-- melyik "a" értékre kapom a legnagyobb "b" eredményt?
maximumBy' :: Ord b => (a -> b) -> [a] -> a
maximumBy' f []     = error "empty list"
maximumBy' f (a:as) = go a as where
  go amax []     = amax
  go amax (a:as)
    | f a > f amax = go a as
    | otherwise    = go amax as

-- -- 2 pont: add vissza a fában legtöbbször előforduló "a" értéket
-- --   (mindegy melyiket adjuk vissza, ha több ilyen van)
-- --   (kicsit trükkös vizsgához)
-- mostCommon :: forall a. Eq a => Tree a -> a
-- mostCommon ta =
--   let as = toList as

--       freqs :: [(a, Int)]
--       freqs = map (\a -> (a, length (filter (==a) as))) as

--       -- Data.List.maximumBy, vagy segédfüggvényt definiálunk
--       (a, n) = maximumBy' snd freqs
--   in a

-- 2 címkézzük 'a', 'b', 'c' stb. karakterekkel balról jobbra a fában az értékeket
-- succ :: Char -> Char
labelWithAlphabet :: Tree a -> Tree (a, Char)
labelWithAlphabet ta = evalState (traverse go ta) 'a' where
  go a = do
    c <- get
    put (succ c)
    pure (a, c)


-- 3 minden kulcsot ("k" értéket a fában) lookup-oljunk egy listából.
--   ha bármelyik lookup sikertelen, legyen Nothing a végeredmény.
lookupReplace :: Eq k => [(k, v)] -> Tree k -> Maybe (Tree v)
lookupReplace kvs t = traverse (\k -> lookup k kvs) t

-- 11 pont
----------------------------------------------------------------------


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
ws = many_ (satisfy isSpace)

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

infixLeft :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixLeft pa psep combine = foldl1 combine <$> sepBy1 pa psep

infixRight :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixRight pa psep combine = foldr1 combine <$> sepBy1 pa psep

infixNonAssoc :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixNonAssoc pa psep combine = do
  exps <- sepBy1 pa psep
  case exps of
    [exp]        -> pure exp                  -- 1 db pa kifejezés
    [exp1, exp2] -> pure $ combine exp1 exp2  -- exp1 `psep` exp2
    _            -> empty

-- Értékadás mint új változó felvétel + mutáció
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

-- 1   pont: add hozzá a következő konstruktorokat az Exp/Stamenthez
-- 2-3 pont: egészítsd ki a parser-t
-- 2   pont: írd át a "data Val" definíciót
--         type Val = Either Int Bool --> data Val = VInt Int | VBool Bool | VPair Val Val
--         írd át az interpreter-t úgy, hogy jól típusozott legyen
-- 3-4 pont: egészítsd ki az interpretert új műveletekkel


-- max 20 pont
-- - __2__: 10-11
-- - __3__: 12-14
-- - __4__: 15-16
-- - __5__: 17-20


--------------------------------------------------------------------------------

{-
Precedenciák:
  - literál, zárójel
  - *  (jobb asszoc)
  - +  (jobb asszoc)
  - == (nem asszoc)
  - if e1 then e2 else e3   (prefix)
-}

data Exp
  = IntLit Int
  | BoolLit Bool
  | Not Exp               -- not e (alkalmazás erősebb minden operátornál)
  | Add Exp Exp
  | Mul Exp Exp
  | Eq Exp Exp
  | Var String            -- változónév
  | Pair Exp Exp          -- Haskell szintaxis (e1, e2)
  | Fst Exp               -- fst e
  | Snd Exp               -- snd e
  deriving (Eq, Show)

type Program = [Statement]

data Statement
  = Assign String Exp               --   x := exp
  | IfThenElse Exp Program Program  --   if e1 then p1 else p2 end
  | While Exp Program               --   while e do p end
  | Print Exp                       --   print e
  deriving (Eq, Show)

-- legyen minden statement ;-vel elválasztva
-- cél Program-ot olvassunk


-- 1. token parserek
char' c = char c <* ws
string' s = string s <* ws

posInt' :: Parser Int
posInt' = read <$> (some (satisfy isDigit) <* ws)

-- Azonosítók vs kulcsszavak
--   nem szeretnénk összekeverni kulcszót azonosítóval
--   furcsa : \ if then -> if if then then else 0
--   diszjunkt legyen a két parser (azonosító nem lehet kulcsszó és fordítva)

keywords :: [String]
keywords = ["if", "then", "else", "while", "do", "end", "not", "fst", "snd", "print"]

-- ki akarjuk zárni:   iffoo olvasása: "if" először, utána "foo" mint azonsító
--                     helyett       : "iffoo" egyben mint azonosító
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

-- ( e )
-- ( e1, e2 )

parensOrPair :: Parser Exp
parensOrPair = do
  char' '('
  es <- sepBy1 eqExp (char' ',')
  char' ')'
  case es of
    []  -> empty
    [e] -> pure e                 -- zárójelezés
    es  -> pure $ foldr1 Pair es  -- jobbra egymásba ágyazott pár

atom :: Parser Exp
atom = (IntLit <$> posInt')
   <|> parensOrPair
   <|> (BoolLit <$> (True  <$ keyword' "true"))
   <|> (BoolLit <$> (False <$ keyword' "false"))
   <|> (Var <$> ident')

-- primFun = fst, snd, not
primFunExp :: Parser Exp
primFunExp =
      (Not <$> (keyword' "not" *> atom))
  <|> (Fst <$> (keyword' "fst" *> atom))
  <|> (Snd <$> (keyword' "snd" *> atom))
  <|> atom

mulExp :: Parser Exp
mulExp = infixRight primFunExp (char' '*') Mul

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
pPrint  = keyword' "print"
pAssign = string' ":="


statement :: Parser Statement
statement =
      (Assign <$> (ident' <* pAssign) <*> eqExp)
  <|> (IfThenElse <$> (pIf *> eqExp)
                  <*> (pThen *> program)
                  <*> (pElse *> program <* pEnd))
  <|> (While <$> (pWhile *> eqExp <* pDo)
             <*> (program <* pEnd))
  <|> (Print <$> (pPrint *> eqExp))

program :: Parser Program
program = sepBy statement (char' ';')

parseProgram :: Parser Program
parseProgram = topLevel program

p1 :: String
p1 = unlines [
  "x := 100;",
  "y := 1000;",
  "while x == 0 do",
    "y := y + 1",
  "end"
  ]

p2 :: String
p2 = unlines [
  "i := 0;",
  "sum := 0;",
  "while not (i == 100) do",
  "  print sum;",
  "  sum := sum + i;",
  "  i := i + 1",
  "end"
  ]


-- Interpreter
--------------------------------------------------------------------------------

data Val = VInt Int | VBool Bool | VPair Val Val
  deriving (Eq, Show)

type Env = [(String, Val)]    -- (ha hatékonyság számít: Env tömb, nevek helyett indexek)

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
  Eq e1 e2 -> case (evalExp env e1, evalExp env e2) of
    (VInt n1, VInt n2)   -> VBool (n1 == n2)
    (VBool b1, VBool b2) -> VBool (b1 == b2)
    _                    -> error "type error"

  Var x -> case lookup x env of
    Nothing -> error $ "name not in scope: " ++ x
    Just v  -> v
  Not e -> case evalExp env e of
    VBool b -> VBool (not b)
    _       -> error "type error"
  Pair e1 e2 -> VPair (evalExp env e1) (evalExp env e2)
  Fst e -> case evalExp env e of
    VPair v1 v2 -> v1
    _           -> error "type error"
  Snd e -> case evalExp env e of
    VPair v1 v2 -> v2
    _           -> error "type error"

update :: String -> Val -> Env -> Env
update x v [] = [(x, v)]
update x v ((x', v'):env)
  | x == x'   = (x, v):env
  | otherwise = (x', v') : update x v env

evalStatement :: Statement -> State (Env, [String]) ()
evalStatement st = case st of

  -- értékadás: ha "x" már definiálva van, akkor módosítjuk az értékét
  --            ha még nincs definiálva, akkor vegyük fel a környezetbe
  Assign x exp -> do
    env <- fst <$> get
    let v = evalExp env exp
    modify (\(_, y) -> (update x v env, y))

  IfThenElse e p1 p2 -> do
    env <- fst <$> get
    case evalExp env e of
      VBool b -> if b then evalProgram p1 else evalProgram p2
      _       -> error "type error"

  While e p -> do
    env <- fst <$> get
    case evalExp env e of
      VBool b -> if b then evalProgram p >> evalStatement (While e p)
                      else pure ()
      _       -> error "type error"

  Print e -> do
    (env, printout) <- get
    let v = evalExp env e
    put (env, show v : printout)

evalProgram :: Program -> State (Env, [String]) ()
evalProgram = traverse_ evalStatement

run :: String -> (Env, [String])
run str = case runParser parseProgram str of
  Nothing        -> error "parse error"
  Just (prog, _) -> case execState (evalProgram prog) ([], []) of
    (env, printout) -> (env, reverse printout)
