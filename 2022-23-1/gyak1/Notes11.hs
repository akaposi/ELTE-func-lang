
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, InstanceSigs
    #-}
{-# options_ghc -Wincomplete-patterns #-}

-- Köv feladat: bármi (nem parser/interpreter), ami eddig
--              szerepelt óra eleji feladatban


--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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


-- FELADATOK (1. sor)
--------------------------------------------------------------------------------

data Either' a b = Left' a | Right' b | Both a b
  deriving (Show)

-- Definiáld a következő instance-okat

instance Functor (Either' c) where
  fmap :: (a -> b) -> Either' c a -> Either' c b
  fmap = undefined

instance (Eq a, Eq b) => Eq (Either' a b) where
  (==) :: Either' a b -> Either' a b -> Bool
  (==) = undefined

instance Foldable (Either' c) where

  -- választhatok foldr és foldMap implementáció között
  foldr :: (a -> b -> b) -> b -> Either' c a -> b
  foldr = undefined

  foldMap :: Monoid m => (a -> m) -> Either' c a -> m
  foldMap = undefined

instance Traversable (Either' c) where
  traverse :: Applicative f => (a -> f b) -> Either' c a -> f (Either' c b)
  traverse = undefined

-- Bontsuk szét a listát a három lehetséges Either' konstruktor szerint!
-- Példa:
--   partition [Left' 0, Left' 2, Right' True, Both 10 False]
--      == ([0, 2], [True], [(10, False)])

partition :: [Either' a b] -> ([a], [b], [(a, b)])
partition = undefined

-- Írj olyan zipWith' függvényt, ami kezelni tudja azokat az eseteket, amikor
-- valamelyik input lista üres (és a másik nemüres). Példa a működésre:
--
--  go :: Either' Int Int -> Int
--  go (Left' x) = x
--  go (Right' x) = x
--  go (Both x y) = x + y
--
--  zipWith' go [1, 2, 3] [10] == [11, 2, 3]
--  zipWith' go [10, 20] [20] == [30, 20]
--  zipWith' go [] [0, 1, 2] == [0, 1, 2]

zipWith' :: (Either' a b -> c) -> [a] -> [b] -> [c]
zipWith' = undefined


-- Alkalmazzunk egy (a -> Maybe b) függvényt a listaelemekre.  Nothing-ot
-- kapunk, legyen a végeredmény Nothing, egyébként Just-ban a map-elt lista.
mapMaybeLeft :: (a -> Maybe b) -> [Either' a c] -> Maybe [Either' b c]
mapMaybeLeft = undefined


data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Functor, Foldable, Traversable)


-- Tegyük be egy fában az összes levélbe az adott levéltől balra levő Int-ek összegét.
-- Pl: treeSums (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1)) ==
--              (Node (Node (Leaf 0) (Leaf 1)) (Leaf 2))
treeSums :: Tree Int -> Tree Int
treeSums = undefined


-- FELADATOK (2. sor)
--------------------------------------------------------------------------------

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

-- Definiáld az instance-okat.

instance Functor RoseTree where
  fmap :: (a -> b) -> RoseTree a -> RoseTree b
  fmap = undefined

instance Foldable RoseTree where
  -- elég az egyiket
  foldr :: (a -> b -> b) -> b -> RoseTree a -> b
  foldr = undefined

  foldMap :: Monoid m => (a -> m) -> RoseTree a -> m
  foldMap = undefined

instance Traversable RoseTree where
  traverse :: Applicative f => (a -> f b) -> RoseTree a -> f (RoseTree b)
  traverse = undefined

-- Add vissza a tárolt "a" típusú értékek számát!
countElems :: RoseTree a -> Int
countElems = undefined

-- Add vissza az elemek maximumát.
maxElem :: Ord a => RoseTree a -> a
maxElem = undefined

-- Számozd be a fában tártolt értékeket balról-jobbra bejárási sorrendben.
label :: RoseTree a -> RoseTree (a, Int)
label = undefined


-- A fában tárolt minden N szám helyére tedd be a kapott [a] lista N-edik
-- értékét. Ha az N index bárhol kimutat a listából, akkor legyen a végeredmény
-- Nothing. Példák a működésre:
--  transformWithList [2, 3, 4] (Branch 0 [Branch 2 [Branch 1 []]])
--                          ==  Just (Branch 2 [Branch 4 [Branch 3 []]])
--  transformWithList [2, 3, 4] (Branch 3 []) == Nothing

transformWithList :: [a] -> RoseTree Int -> Maybe (RoseTree a)
transformWithList = undefined


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
keywords = ["not", "true", "false", "while", "if", "do", "end", "then", "else"]

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

atom :: Parser Exp
atom =
        (Var <$> ident')
    <|> (IntLit <$> posInt')
    <|> (BoolLit True <$ keyword' "true")
    <|> (BoolLit False <$ keyword' "false")
    <|> (char' '(' *> pExp <* char' ')')

notExp :: Parser Exp
notExp =  (keyword' "not" *> (Not <$> atom))
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

data Val = VInt Int | VBool Bool
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
  If e p1 p2 -> do
    env <- get
    case evalExp env e of
      VBool True  -> inNewScope (evalProgram p1)
      VBool False -> inNewScope (evalProgram p2)
      VInt _      -> error "type error"

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
