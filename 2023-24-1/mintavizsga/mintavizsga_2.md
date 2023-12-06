A feladatsor megoldására 2 óra áll rendelkezésre. Külső segítség és kollaboráció
nem megengedett. A megoldást akárhányszor be lehet küldeni, az utolsó megoldás
számít. Részpontszám kapható minden feladatra. A leírás végén megtaláljátok a
teljes Haskell fájlt, amit ki kell egészíteni és feltölteni.

# Ponthatárok

  - __2__: 10-11
  - __3__: 12-14
  - __4__: 15-16
  - __5__: 17-20e

# Feladatok (12 pont)

Adott az alábbi típus:

```hs
data Pipe a = Wait (Pipe a) | ProduceOne a (Pipe a) | ProduceMany [a] (Pipe a) | EOF
    deriving Show

infixr 5 `ProduceOne`, `ProduceMany`
```

Példák:

```hs
p1 :: Pipe Int
p1 = 1 `ProduceOne` 2 `ProduceOne` (Wait $ [3,4] `ProduceMany` EOF)

p2 :: Pipe String
p2 = [] `ProduceMany` "Hello" `ProduceOne` "World" `ProduceOne` EOF

p3 :: Pipe Int
p3 = Wait $ ProduceOne 1 $ Wait $ Wait $ ProduceOne 2 EOF

p4 :: Pipe Bool
p4 = ProduceOne True $ Wait $ not <$> p4
```

Írd meg a következő instance-okat! __(4 pont)__

```hs
instance Eq a => Eq (Pipe a) where
  (==) :: Pipe a -> Pipe a -> Bool
  (==) = undefined

-- elég a foldr/foldMap közül az egyik
instance Foldable Pipe where
  -- foldr :: (a -> b -> b) -> b -> Pipe a -> b
  -- foldr = undefined

  -- foldMap :: Monoid m => (a -> m) -> Pipe a -> m
  -- foldMap = undefined

instance Functor Pipe where
  fmap :: (a -> b) -> Pipe a -> Pipe b
  fmap = undefined

-- elég a sequenceA/traverse közül az egyik
instance Traversable Pipe where
  -- traverse :: Applicative f => (a -> f b) -> Pipe a -> f (Pipe b)
  -- traverse = undefined

  -- sequenceA :: Applicative f => Pipe (f a) -> f (Pipe a)
  -- sequenceA = undefined
```

Tesztek:

```hs
p1 == p1
p1 /= p3
p1 /= ((+1) <$> p1)
sum p1 == 10
not $ and p4
fmap (+1) p3 == (Wait $ ProduceOne 2 $ Wait $ Wait $ ProduceOne 3 EOF)
(3 <$ p2) == ([] `ProduceMany` 3 `ProduceOne` 3 `ProduceOne` EOF)
traverse (\a -> if a > 5 then Just (a ^ 2) else Nothing) p1 == Nothing
traverse (\a -> if a < 5 then Just (a ^ 2) else Nothing) p1 == Just (fmap (^2) p1)
```

Definiálj egy függvényt, ami addig szedi ki az elemeket a pipe-ból, amíg várásra
nincs kényszerítve (egy `Wait` konstruktorig el nem jut). Adjuk vissza a maradék
csövet is az eredményben! __(1 pont)__

```hs
readUntilSemaphore :: Pipe a -> ([a], Pipe a)
readUntilSemaphore = undefined
```

Tesztek a működésre:

```hs
readUntilSemaphore p1 == ([1,2], [3,4] `ProduceMany` EOF)
readUntilSemaphore p2 == (["Hello", "World"], EOF)
readUntilSemaphore p3 == ([], ProduceOne 1 $ Wait $ Wait $ ProduceOne 2 EOF)
readUntilSemaphore (ProduceMany [1..10] p1) == ([1..10] ++ [1,2], [3,4] `ProduceMany` EOF)
```

Definiálj egy függvényt, ami a várások közti `ProduceMany` és `ProduceOne`
konstruktorokban tárolt elemeket aggregálja egy `ProduceMany` konstruktorba!
__(2 pont)__

```hs
flattenPipe :: Pipe a -> Pipe a
flattenPipe = undefined
```

Tesztek a működésre:

```hs
flattenPipe p1 == ProduceMany [1,2] (Wait (ProduceMany [3,4] EOF))
flattenPipe p2 == ProduceMany ["Hello","World"] EOF
flattenPipe p3 == ProduceMany [] (Wait (ProduceMany [1] (Wait (ProduceMany [] (Wait (ProduceMany [2] EOF))))))
```

Definiálj egy függvényt, ami egy csövet beolvas a stdin-ról, majd kiírja, hogy
mennyi elem és mennyi `Wait` konstruktor van a csőben! A csövet
eredményül adjuk vissza! Beolvasásnál az elemeket soronként olvassuk:

- Ha a sorban csak egy elem van, akkor `ProduceOne` konstruktort.
- Ha a sorban 2 vagy több elem van, akkor `ProduceMany` konstruktort.
- Ha a sorban nincs elem, akkor `Wait` konstruktort
- Ha a sorban az EOF szó van, akkor a csőnek vége van és ne olvassunk több sort.

Feltehetjük, hogy a formátum mindig helyes és csak pozitív egész számok lesznek
az elemek! __(2 pont)__

__Segítség:__ A `read` függvény segítségével tudunk stringből számot csinálni.

__Segítség:__ A `words` függvény a `Data.List` modulból fel tud bontani egy
`String`-et space-el elválasztott szavak listájára.

```hs
processPipe :: IO (Pipe Int)
processPipe = undefined
```

Példa `p1` csőre:

```
-- STDIN
1
2

3 4
EOF
-- STDOUT
Elemek száma: 4
Várások száma: 1
```

Példa a `p3` csőre:

```
-- STDIN

1


2
EOF
-- STDOUT
Elemek száma: 2
Várások száma: 3
```

Definiálj egy függvényt, ami megcímkézi egy cső elemeit aszerint, hogy hanyadik
konstruktorban vannak a cső végétől számolva. A `Wait` és `EOF` konstruktorokat
ne számoljuk bele! __(3 pont)__

```hs
labelPipe :: Pipe a -> Pipe (a, Int)
labelPipe = undefined
```

Tesztek a működésre:
```hs
labelPipe p1 == (1,3) `ProduceOne` (2,2) `ProduceOne` (Wait $ [(3,1),(4,1)] `ProduceMany` EOF)
labelPipe p2 == [] `ProduceMany` ("Hello", 2) `ProduceOne` ("World", 1) `ProduceOne` EOF
labelPipe p3 == (Wait $ ProduceOne (1,2) $ Wait $ Wait $ ProduceOne (2,1) EOF)
```

# `while` nyelv kiegészítése (8 pont)

A feladat a `while` nyelv kiegészítése vermekkel.

Adjuk a szintaxishoz a `EmptyStack :: Exp`, `PopInto :: String -> String -> Statement`, `Push :: String -> Exp -> Statement` és `IsEmpty :: Exp -> Exp` konstruktorokat! __(1 pont)__

### Parser

A szintaxis a következő:

- Az `EmptyStack`-et egy literál reprezentálja a `<>` szimbólummal. A
  relációjelek között is lehessen space.
- A `PopInto` állítás kezdődjön a `pop` szóval, majd egy változónév kövesse,
  ezek után a `into` kulcsszó és még egy változónév következzen. Ezeket a
  kulcsszavakat adjuk hozzá a kulcsszavak listájához.
- A `Push` állítás a `push` kulcsszóval kezdődjön, amit egy változónév és egy
  kifejezés követ. A `push` kulcsszót adjuk hozzá a kulcsszavak listájához.
- Az `IsEmpty` kifejezés egy `empty` prefix operátor legyen, ugyanazzal a kötési
  erősséggel mint a `not`. Az `empty` kulcsszót adjuk hozzá a kulcsszavak
  listájához.

__(3 pont)__

Tesztek a működésre:

```hs
runParser program "x := <>" == Just ([Assign "x" EmptyStack],"")
runParser program "x := empty < > " == Just ([Assign "x" (IsEmpty EmptyStack)],"")
runParser program "x := <>; push x 1; push x 2; push x (x + 1)" == Just ([Assign "x" EmptyStack,Push "x" (IntLit 1),Push "x" (IntLit 2),Push "x" (Add (Var "x") (IntLit 1))],"")
runParser program "pop x into y; pop y into x" == Just ([PopInto "x" "y",PopInto "y" "x"],"")
```

### Interpreter

Egészítsd ki a `Val` típust egy `VStack :: [Val] -> Val` konstruktorral. __(1 pont)__

A működés a következő:

- Az `EmptyStack` értékelődjön ki `VStack []`-ra.
- Az `IsEmpty` kifejezés értékelje ki a paraméterül kapott kifejezést; ha az egy
  `VStack`, akkor adjuk vissza egy `VBool`-ba, hogy üres-e vagy nem. Ha nem
  `VStack`, dobjunk típushibát.
- A `Push` állítás szúrja be a paraméterül kapott változó __elejére__ a második
  parméter értékét. Ha a változó nem `VStack`, dobjunk típushibát.
- A `PopInto` állítás szedje ki a fejelemet az első paraméterül kapott listából
  és mentse el a második paraméterül kapott névvel (akkor is, ha ilyen nevű
  változó nem létezik). Ha az első paraméter nem `VStack`, dobjunk
  típushibát. Ha a `VStack` érték üres, akkor az állítás ne csináljon semmit.

__(3 pont)__

Tesztek a működésre:
```hs
run "x := <>; push x 1" == [("x",VStack [VInt 1])]
run "x := <>; push x x" == [("x",VStack [VStack []])]
run "x := <>; y := 0; while not (y == 10) do push x (y + 1); y := y + 1 end; while not (empty x) do pop x into a; y := y + a * a end" == [("x",VStack []),("y",VInt 395)]
```

Beadandó fájl:

```haskell
{-# LANGUAGE DeriveFunctor, InstanceSigs #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Data.Char

newtype State s a = State { runState :: s -> (a, s) } deriving Functor

instance Applicative (State s) where
    pure a = State (\s -> (a, s))
    (<*>) = ap

instance Monad (State s) where
    (State f) >>= g = State $ \s -> let (a, s') = f s in runState (g a) s'

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

-- FELADATOK
--------------------------------------------------------------------------------

data Pipe a = Wait (Pipe a) | ProduceOne a (Pipe a) | ProduceMany [a] (Pipe a) | EOF
    deriving Show

infixr 5 `ProduceOne`, `ProduceMany`

p1 :: Pipe Int
p1 = 1 `ProduceOne` 2 `ProduceOne` Wait ([3,4] `ProduceMany` EOF)

p2 :: Pipe String
p2 = [] `ProduceMany` "Hello" `ProduceOne` "World" `ProduceOne` EOF

p3 :: Pipe Int
p3 = Wait $ ProduceOne 1 $ Wait $ Wait $ ProduceOne 2 EOF

p4 :: Pipe Bool
p4 = ProduceOne True $ Wait $ not <$> p4


instance Eq a => Eq (Pipe a) where
  (==) :: Pipe a -> Pipe a -> Bool
  (==) = undefined

-- elég a foldr/foldMap közül az egyik
instance Foldable Pipe where
  -- foldr :: (a -> b -> b) -> b -> Pipe a -> b
  -- foldr = undefined

  -- foldMap :: Monoid m => (a -> m) -> Pipe a -> m
  -- foldMap = undefined

instance Functor Pipe where
  fmap :: (a -> b) -> Pipe a -> Pipe b
  fmap = undefined

-- elég a sequenceA/traverse közül az egyik
instance Traversable Pipe where
  -- traverse :: Applicative f => (a -> f b) -> Pipe a -> f (Pipe b)
  -- traverse = undefined

  -- sequenceA :: Applicative f => Pipe (f a) -> f (Pipe a)
  -- sequenceA = undefined

readUntilSemaphore :: Pipe a -> ([a], Pipe a)
readUntilSemaphore = undefined

flattenPipe :: Pipe a -> Pipe a
flattenPipe = undefined

processPipe :: IO (Pipe Int)
processPipe = undefined

labelPipe :: Pipe a -> Pipe (a, Int)
labelPipe = undefined

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
  m <- optional (satisfy isLetter)
  case m of
    Just _ -> empty
    _      -> ws

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
```
