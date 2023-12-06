A feladatsor megoldására 2 óra áll rendelkezésre. Külső segítség és kollaboráció
nem megengedett. A megoldást akárhányszor be lehet küldeni, az utolsó megoldás
számít. Részpontszám kapható minden feladatra. A leírás végén megtaláljátok a
teljes Haskell fájlt, amit ki kell egészíteni és feltölteni.

# Ponthatárok

  - __2__: 10-11
  - __3__: 12-14
  - __4__: 15-16
  - __5__: 17-20

# Feladatok (12 pont)

# Feladatok (12 pont)

Adott az alábbi típus:
```hs
data BiList a = ConsSnoc a (BiList a) a | Nil
  deriving Show
```

Példák:
```hs
b1 :: BiList Int
b1 = ConsSnoc 0 (ConsSnoc 1 Nil 2) 3

b2 :: BiList Bool
b2 = ConsSnoc True Nil False

b3 :: BiList Char
b3 = ConsSnoc 'h' (ConsSnoc 'e' (ConsSnoc 'l' (ConsSnoc 'l' (ConsSnoc 'o' (ConsSnoc ' ' Nil 'w') 'o') 'r') 'l') 'd') '!'

b4 :: BiList Int
b4 = ConsSnoc 1 b4 2
```

Írd meg a következő instance-okat! __(4 pont)__
```hs
instance Eq a => Eq (BiList a) where
    -- (==) :: BiList a -> BiList a -> Bool
    -- (==) = undefined

instance Foldable BiList where
    -- Elég az egyik
    -- foldr :: (a -> b -> b) -> b -> BiList a -> b
    -- foldr = undefined

    -- foldMap :: Monoid m => (a -> m) -> BiList a -> m
    -- foldMap = undefined

instance Functor BiList where
    -- fmap :: (a -> b) -> BiList a -> BiList b
    -- fmap = undefined

instance Traversable BiList where
    -- Elég az egyik
    -- traverse :: Applicative f => (a -> f b) -> BiList a -> f (BiList b)
    -- traverse = undefined

    -- sequenceA :: Applicative f => BiList (f a) -> f (BiList a)
    -- sequenceA = undefined
```

Tesztek a működésre:
```hs
b1 == b1
b2 /= ConsSnoc False b2 True
sum b1 == 6
any (>0) b4
fmap (+1) b1 == ConsSnoc 1 (ConsSnoc 2 Nil 3) 4
(1 <$ b2) == ConsSnoc 1 Nil 1
traverse (\a -> if a > 4 then Just (a ^ 2) else Nothing) b1 == Nothing
traverse (\a -> if a < 4 then Just (a ^ 2) else Nothing) b1 == Just (fmap (^2) b1)
```

Definiálj egy függvényt, ami egy `BiList`-nek az első legfeljebb `n` darab `ConsSnoc` konstruktorát veszi, a többit eldobja! __(1 pont)__
```hs
takeConsSnoc :: Int -> BiList a -> BiList a
takeConsSnoc = undefined
```

Tesztek a működésre:
```hs
takeConsSnoc 1 b1 == ConsSnoc 0 Nil 3
takeConsSnoc 0 b2 == Nil
takeConsSnoc 3 b4 == ConsSnoc 1 (ConsSnoc 1 (ConsSnoc 1 Nil 2) 2) 2
```

Definiálj egy függvényt, ami egy `BiList`-et listává alakít! Egy `ConsSnoc` konstruktor első paramétere a lista elejére, az utolsó paramétere a lista végére kerüljön! __(2 pont)__
```hs
convertToList :: BiList a -> [a]
convertToList = undefined
```
Tesztek a működésre:
```hs
convertToList b3 == "hello world!"
convertToList b1 == [0,1,2,3]
convertToList b2 == [True, False]
```

Definiálj egy függvényt, ami megfordítja az elemek sorrendjét egy `BiList`-ben! __(2 pont)__
```
reverseBilist :: BiList a -> BiList a
reverseBilist = undefined
```
Tesztek a működésre:
```hs
convertToList (reverseBilist b3) == "!dlrow olleh"
reverseBilist (reverseBilist b1) == b1
reverseBilist b2 == ConsSnoc False Nil True
```

Definiálj egy függvényt, ami egy `BiList`-et megcímkéz *preorder* bejárási sorrendben, ahol
egy `ConsSnoc`-ban a bal és jobb értéket járjuk be először, és ezután járjuk be a rekurzív listát.
Kezd 0-ról a címkézést.
__(3 pont)__
```hs
label :: BiList a -> BiList (a, Int)
label = undefined
```

Tesztek a működésre:
```hs
labelLR b1 == ConsSnoc (0,0) (ConsSnoc (1,2) Nil (2,3)) (3,1)
labelLR b3 == ConsSnoc ('h',0) (ConsSnoc ('e',2) (ConsSnoc ('l',4) (ConsSnoc ('l',6) (ConsSnoc ('o',8) (ConsSnoc (' ',10) Nil ('w',11)) ('o',9)) ('r',7)) ('l',5)) ('d',3)) ('!',1)
```


# `while` nyelv kiegészítése (8 pont)

A feladat a `while` nyelv kiegészítése nullable értékekkel.

Adjuk a szintaxishoz a `Null :: Exp`, `AssignIfNull :: String -> Exp -> Statement` és `IsNull :: Exp -> Exp` konstruktorokat! __(1 pont)__

### Parser

A szintaxis a következő:

- A `Null` egy literál legyen `null` néven. Ezt vegyük fel a kulcsszavak listájába.
- Az `AssignIfNull` szintaxisa egy változónév, majd egy `?=` szimbolúm és végül egy kifejezés.
- Az `IsNull` kifejezés egy postfix `?` operátor legyen ugyanazzal az erősséggel mint a `not`.

__(3 pont)__

Tesztek a működésre:
```
runParser pExp "null " == Just (Null,"")
runParser pExp "null + 2" == Just (Add Null (IntLit 2),"")
runParser pExp "null?" == Just (IsNull Null,"")
runParser pExp "1? + null" == Just (Add (IsNull (IntLit 1)) Null,"")
runParser statement "x ?= 1" == Just (AssignIfNull "x" (IntLit 1),"")
runParser statement "null ?= 1" == Nothing
runParser statement "x ?= x?" == Just (AssignIfNull "x" (IsNull (Var "x")),"")
```

### Interpreter

Egészítsük ki a `Val` típust egy `VNull :: Val` konstruktorral! __(1 pont)__

A működés a következő:
- `Null` értéke legyen `VNull`.
- Ha `VNull`-t kap bármilyen művelet inputként, akkor `VNull` legyen az eredmény.
- Ez alól az `IsNull` művelet kivétel, ami adja vissza `VBool`-ban, hogy a
  kifejezés `VNull`-e.
- Az `AssignIfNull` állítás állítsa be az adott változó értékét a kifejezés
  értékére, ha a változó értéke `VNull` vagy pedig nincs a változónak értéke a
  környezetben.

__(3 pont)__

Tesztek a működésre:
```
run "x := null" == [("x",VNull)]
run "x ?= null; x ?= x?" == [("x",VBool True)]
run "x := null; y := 1; while x? do y := y + 1; if x? && (y == 10) then x ?= y else end end" == [("x",VInt 10),("y",VInt 10)]
```

Beadandó fájl:

```haskell
{-# LANGUAGE DeriveFunctor, InstanceSigs #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List.NonEmpty ( NonEmpty(..) )

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


data BiList a = ConsSnoc a (BiList a) a | Nil
  deriving Show

b1 :: BiList Int
b1 = ConsSnoc 0 (ConsSnoc 1 Nil 2) 3

b2 :: BiList Bool
b2 = ConsSnoc True Nil False

b3 :: BiList Char
b3 = ConsSnoc 'h' (ConsSnoc 'e' (ConsSnoc 'l' (ConsSnoc 'l' (ConsSnoc 'o' (ConsSnoc ' ' Nil 'w') 'o') 'r') 'l') 'd') '!'

b4 :: BiList Int
b4 = ConsSnoc 1 b4 2

instance Eq a => Eq (BiList a) where
    -- (==) :: BiList a -> BiList a -> Bool
    -- (==) = undefined

instance Foldable BiList where
    -- Elég az egyik
    -- foldr :: (a -> b -> b) -> b -> BiList a -> b
    -- foldr = undefined

    -- foldMap :: Monoid m => (a -> m) -> BiList a -> m
    -- foldMap = undefined

instance Functor BiList where
    -- fmap :: (a -> b) -> BiList a -> BiList b
    -- fmap = undefined

instance Traversable BiList where
    -- Elég az egyik
    -- traverse :: Applicative f => (a -> f b) -> BiList a -> f (BiList b)
    -- traverse = undefined

    -- sequenceA :: Applicative f => BiList (f a) -> f (BiList a)
    -- sequenceA = undefined

takeConsSnoc :: Int -> BiList a -> BiList a
takeConsSnoc = undefined

convertToList :: BiList a -> [a]
convertToList = undefined

reverseBilist :: BiList a -> BiList a
reverseBilist = undefined

label :: BiList a -> BiList (a, Int)
label xs = undefined


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
