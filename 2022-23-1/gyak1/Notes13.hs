

{-# language DeriveFunctor, InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable
import Debug.Trace

-- Feladat megoldás
--------------------------------------------------------------------------------

data Tree' a = Leaf' a | Node' a (Tree' a) (Tree' a) deriving (Eq, Show)

-- Cseréld ki egy fában a tárolt értékeket Int-ekre, amelyek 0-tól kezdve
-- jobbról-balra bejárási sorrendben növekednek. Használj State-et.

f :: Tree' a -> Tree' Int
f t = evalState (go t) 0 where

  goElem :: State Int Int
  goElem = do
    n <- get
    put $ n + 1
    pure n

  go :: Tree' a -> State Int (Tree' Int)
  go (Leaf' _) =
    Leaf' <$> goElem
  go (Node' _ l r) = do
    r' <- go r
    l' <- go l
    n  <- goElem
    pure $ Node' n l' r'


tests :: [Bool]
tests = [
    f (Leaf' ()) == Leaf' 0
  , f (Node' () (Leaf' ()) (Leaf' ())) == Node' 2 (Leaf' 1) (Leaf' 0)
  , f (Node' () (Node' () (Leaf' ()) (Leaf' ())) (Leaf' ())) == Node' 4 (Node' 3 (Leaf' 2) (Leaf' 1)) (Leaf' 0)
  ]


--------------------------------------------------------------------------------
-- Minta vizsga 3
--------------------------------------------------------------------------------


-- State monad
--------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

instance Applicative (State s) where
  pure  a = State (\s -> (a, s))
  (<*>) = ap

instance Monad (State s) where
  return = pure
  State f >>= g = State (\s -> case f s of (a, s') -> runState (g a) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--------------------------------------------------------------------------------

data Tree a = Leaf [a] | Node (Tree a) (Tree a)
  deriving (Show)

t1 :: Tree Int
t1 = Node (Leaf [1, 2, 3]) (Leaf [10])

t2 :: Tree Int
t2 = Leaf []

t3 :: Tree Int
t3 = Node (Leaf []) (Node (Leaf [0, 0]) (Leaf [10, 20]))

-- Írd meg a következő instance-okat!
instance (Eq a) => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) = undefined

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap = undefined

instance Foldable Tree where
  -- az egyiket definiáld
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr = undefined

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap = undefined

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse = undefined

-- Add meg a fában tárolt listák listáját!
-- Példák:
--   toLists t1 == [[1,2,3],[10]]
--   toLists t2 == [[]]
--   toLists t3 == [[],[0,0],[10,20]]

toLists :: Tree a -> [[a]]
toLists = undefined

-- Add meg a fában található első (balról-jobbra sorrendben) elemet, amire igaz
-- egy feltétel függvény! Ha nincs ilyen érték, adj Nothing-ot. Példák:
--    findElem even t1 == Just 2
--    findElem (==100) t1 == Nothing
--    findElem (>10) t3 == Just 20

findElem :: (a -> Bool) -> Tree a -> Maybe a
findElem = undefined


-- Írj egy függvényt, ami egy fa összes tálrolt értékét kicseréli egy
-- terminálról beolvasott sorral. Használj `getLine`-t a beolvasáshoz.

treeGetLines :: Tree a -> IO (Tree String)
treeGetLines = undefined

--------------------------------------------------------------------------------


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

A feladatunk, hogy kiegészítsük a `while` nyelvet lebegőpontos értékekkel,
amelyek reprezentációja a Haskell-beli `Double` típus lesz. Ehhez módosítanunk
kell a szintaxist, a parsert és az interpretert.

Vegyünk fel egy `DoubleLit :: Double -> Expr` konstruktort. Vegyünk fel továbbá
egy `Div :: Expr -> Expr -> Expr` konstruktort az eddigi kifejezések mellé. Ez
fogja reprezentálni a lebegőpontos osztás (`/`) műveletet.

Literálok olvasása:
  - Pontosan egy tizedes pontot tartalmazzon egy Double literál.
  - Opcionálisan lehet '-' egy literál előtt.
  - Tipp: használjuk a `read :: String -> Double` függvényt
    a végeredmény kiszámolásához.
  - Ne feledkezzünk meg a `ws` olvasásáról literálok után.

Osztás olvasás:
  - A `/` operátor asszociáljon *balra*, kössön erősebben a `*`-nál
   és gyengébben a `not`-nál.

Kiértékelés:
  - Egészítsük ki a `Val` típust úgy, hogy `Double`-is tároljon.
  - A literálok és az osztás kiértékelése értelemszerű. Dobjunk típushibát,
    ha az osztás paraméterei nem `Double`-ök.
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
