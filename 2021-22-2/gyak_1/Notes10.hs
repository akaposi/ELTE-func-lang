
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, InstanceSigs, BangPatterns #-}
{-# language ScopedTypeVariables #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Debug.Trace
import Data.Char    -- isSpace, isDigit

-- köv canvas feladat: random kisfeladat, jelenlegi feladatsorhoz hasonló
--------------------------------------------------------------------------------

-- mai canvas feladat
--------------------------------------------------------------------------------


-- FELADAT
--------------------------------------------------------------------------------

-- Írj parser-t, ami Bool kifejezéseket olvas be. Whitespace mindenhol
-- előfordulhat. Használd a `topLevel` parser-t.  Erősség csökkenő
-- sorrendjében a nyelvi konstrukciók a következők:
--   - atom: literál (true vagy false)
--   - logikai és   (&&) : jobb asszociatív
--   - logikai vagy (||) : jobb asszociatív
--
-- Nem kell kulcsszavakkal foglalkozni, a Bool literálok olvashatók egyszerűen
-- string' függvénnyel. Nincs zárójelezés.

data Exp'
  = BoolLit' Bool   -- true vagy false
  | And' Exp' Exp'  -- e1 && e2
  | Or' Exp' Exp'   -- e1 || e2
  deriving (Eq, Show)

pAtom' :: Parser Exp'
pAtom' = (BoolLit' True <$ string' "true")
     <|> (BoolLit' False <$ string' "false")

pAnd' :: Parser Exp'
pAnd' = rightAssoc And' pAtom' (string' "&&")

pOr' :: Parser Exp'
pOr' = rightAssoc Or' pAnd' (string' "||")

parseExp :: Parser Exp'
parseExp = topLevel pOr'

tests :: [Bool]
tests = [
    runParser parseExp "true" == Just (BoolLit' True,"")
  , runParser parseExp "false" == Just (BoolLit' False,"")
  , runParser parseExp "false || true && false" == Just (Or' (BoolLit' False) (And' (BoolLit' True) (BoolLit' False)),"")
  , runParser parseExp "true && false && false" == Just (And' (BoolLit' True) (And' (BoolLit' False) (BoolLit' False)),"")
  , runParser parseExp "true || false || false" == Just (Or' (BoolLit' True) (Or' (BoolLit' False) (BoolLit' False)),"")
  ]


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
  (==) = undefined

instance Functor RoseTree where
  fmap = undefined

instance Foldable RoseTree where
  -- elég az egyiket definiálni
  foldr   = undefined
  foldMap = undefined -- lásd 10. előadás

instance Traversable RoseTree where
  traverse = undefined

-- Add vissza az "a" típusú elemek számát egy fában!
countElems :: RoseTree a -> Int
countElems = undefined

-- Add vissza a maximális "a" értéket egy fából!
maxElem :: Ord a => RoseTree a -> a
maxElem = undefined

-- Számozd be bal-jobb bejárási sorrendben a fát!
label :: RoseTree a -> RoseTree (a, Int)
label = undefined

-- Írj egy függvényt, ami egy fában az összes "n :: Int" értéket kicseréli az
-- adott "[a]" lista n-edik elemére! Ha "n" bárhol nagyobb vagy egyenlő mint a
-- lista hossza, akkor legyen a végeredmény Nothing.
transformWithList :: [a] -> RoseTree Int -> Maybe (RoseTree a)
transformWithList = undefined


-- Feladatok 2. (könnyebb sor, inkább tipikus)
--------------------------------------------------------------------------------

data MaybeTree a
  = Node (MaybeTree a) (Maybe a) (MaybeTree a)
  | Leaf a
  deriving (Show)

-- Írd meg az instance-okat!
instance (Eq a) => Eq (MaybeTree a) where
  (==) = undefined

instance Functor MaybeTree where
  fmap = undefined

instance Foldable MaybeTree where
  -- egyiket elég definiálni
  foldr = undefined
  foldMap = undefined

instance Traversable MaybeTree where
  traverse = undefined

-- Számold meg a tárolt `Just` konstruktorokat a fában.
countJusts :: MaybeTree a -> Int
countJusts = undefined

-- Add vissza csak a `Leaf`-ekben tárolt értékek listáját.
leaves :: MaybeTree a -> [a]
leaves = undefined

-- Egy fában told el az összes "a" elemet egy pozícióval jobbra, a balról-jobbra
-- bejárási sorrendben. A leginkább baloldali elem helyére kerüljön egy megadott
-- "default" érték.

-- Tipp: használd a `State a`-t a bejáráshoz! Az állapot legyen legutóbb bejárt
-- `a` típusú érték, vagy pedig az adott default érték, ha még nem jártunk be
-- egy elemet sem.

shiftElems :: a -> MaybeTree a -> MaybeTree a
shiftElems = undefined

-- példák a működésre:
--   shiftElems 10 (Leaf 0) == Leaf 10
--   shiftElems 10 (Node (Leaf 0) (Just 1) (Leaf 2)) == Node (Leaf 10) (Just 0) (Leaf 1)


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

   A `run` új típusa legyen `String -> String -> (String, Env)`. Az első
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

  --
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
keywords = ["not", "true", "false", "while", "if", "do", "end", "then", "else"]

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

pParens :: Parser Exp
pParens = do
  char' '('
  e1 <- eqExp
  char' ')'
  pure e1

atom :: Parser Exp
atom =
        (Var <$> ident')
    <|> (IntLit <$> posInt')
    <|> (BoolLit True <$ keyword' "true")
    <|> (BoolLit False <$ keyword' "false")
    <|> pParens

pNotFstSnd :: Parser Exp
pNotFstSnd =  (keyword' "not" *> (Not <$> atom))
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

  -- if ágakban új változók kívül nem látszanak
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
