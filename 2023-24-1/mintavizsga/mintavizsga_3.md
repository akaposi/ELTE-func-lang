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

Adott az alábbi típus:
```hs
data Stream a = Append a (Stream a) | Cycle [a]
    deriving Show

infixr 5 `Append`
```

Példák a streamre:
```hs
s1 :: Stream Int
s1 = 1 `Append` 2 `Append` (Cycle [3])

s2 :: Stream Int
s2 = Cycle [1..10]

s3 :: Stream Bool
s3 = True `Append` (not <$> s3)
```

Írd meg a következő instance-okat! __(4 pont)__
```hs
instance Eq a => Eq (Stream a) where

-- elég a foldr/foldMap közül az egyik
instance Foldable Stream where

instance Functor Stream where

-- elég a sequenceA/traverse közül az egyik
instance Traversable Stream where

```

Tesztek a működésre:
```hs
s1 == s1
s1 /= s2
s1 /= ((+1) <$> s1)
sum s1 == 6
not $ and s3
fmap (+1) s1 == (2 `Append` 3 `Append` (Cycle [4]))
(3 <$ s2) == Cycle (replicate 10 3)
traverse (\a -> if a > 4 then Just (a ^ 2) else Nothing) s1 == Nothing
traverse (\a -> if a < 4 then Just (a ^ 2) else Nothing) s1 == Just (fmap (^2) s1)
```

Definiálj egy függvényt, amely egy streamet listává konvertál. A `Cycle`
konstruktor esetén végtelenszer ismételjük a lista elemeit! (Feltehetjük, hogy a
`Cycle` konstruktor paramétere nem üres) __(1 pont)__

```hs
streamToList :: Stream a -> [a]
streamToList = undefined
```

Tesztek a működésre:
```hs
take 4 (streamToList s1) == [1,2,3,3]
take 4 (streamToList s2) == [1,2,3,4]
take 20 (streamToList s2) == ([1..10] ++ [1..10])
```

Definiálj egy függvényt, amely egy streamet és egy listát össsezfűz. A
listaelemek beszúrása a `Cycle` konstruktor elé kerüljön! __(2 pont)__

```hs
insertBeforeCycle :: Stream a -> [a] -> Stream a
insertBeforeCycle = undefined
```

Tesztek a működésre:
```hs
insertBeforeCycle s1 [1,2,3] == (1 `Append` 2 `Append` 1 `Append` 2 `Append ` 3 `Append` (Cycle [3]))
insertBeforeCycle s1 [] == s1
take 12 (streamToList (insertBeforeCycle s2 [0])) == ([0..10] ++ [1])
```

Definiálj egy függvényt, amely a standard bemenetről beolvas egy streamet! A
függvény olvasson be két sor számot, az első a stream nem ismédlődő, a másik az
ismétlődő része lesz (A `Cycle` konstruktor paramétere)! __(2 pont)__

Feltehetjük, hogy a bemenet mindig helyes és a számok space-el vannak elválasztva.

__Segítség:__ A `read` függvény segítségével tudunk stringből számot csinálni.

__Segítség:__ Az `unwords` függvény a `Data.List` modulból fel tud bontani egy string-et spacenként.

```hs
readStream :: IO (Stream Int)
readStream = undefined
```

Példa bemenet (s1):

```
1 2
3
```

Példa bemenet (s2):

```

1 2 3 4 5 6 7 8 9 10
```

Definiálj egy függvényt, ami megfordítja egy `Stream`-ben az elemek
sorrendjét. Az eredmény `Cycle` konstruktorában legyen ugyanannyi elem mint a
bemeneti paraméter `Cycle` konstruktorában! __(3 pont)__

```hs
reverseSt :: Stream a -> Stream a
reverseSt = undefined
```

Tesztek a működésre:
```hs
reverseSt s1 == (3 `Append` 2 `Append` (Cycle [1]))
reverseSt s2 == Cycle [10,9..1]
reverseSt (1 `Append` s2) == (10 `Append` Cycle ([9,8..1] ++ [1]))
```

# `while` nyelv kiegészítése (8 pont)

A feladat a `while` nyelv kiegészítése listákkal.

Adjuk a szintaxishoz az `Index :: Exp -> Exp ->  Exp`, `List :: [Exp] -> Exp` és `AssignAt :: String -> Exp -> Exp -> Statement` konstruktorokat __(1 pont)__

### Parser

A szintaxis a következő:

- Az `Index` egy balra asszociáló operátor legyen, melynek a kötési erőssége az
  '+' és '*' műveletek között van. Az operátorszimbólum legyen a `!!`.
- A `List` konstruktor reprezentáljon lista literálokat (amelyeket a literálok
  szintjén parseoljunk). Egy list literál `[]`-en belül legyen (0 vagy több
  elem) és a kifejezések legyenek `,`-vel elválasztva. Minden elem kifejezésben
  a precedencia szint kezdődjön előröl!
- Az `AssignAt` állítás szintaxisa egyezen meg az értékadáséval, kivéve, hogy az
  értékadás bal oldalán egy változónév legyen, utána egy `!!` szimbólum, majd
  egy kifejezés, ami az indexet reprezentálja.

__(3 pont)__

Tesztek a működésre:
```hs
runParser pExp "1 + [2,3   , 4]" == Just (Add (IntLit 1) (List [IntLit 2,IntLit 3,IntLit 4]),"")
runParser pExp "[] !! 3" == Just (Index (List []) (IntLit 3),"")
runParser pExp "[                      [       [ ] ]  ]" == Just (List [List [List []]],"")
runParser pExp "[1 + 2, 3 + 4 !! 2 * 1]" == Just (List [Add (IntLit 1) (IntLit 2),Add (IntLit 3) (Index (IntLit 4) (Mul (IntLit 2) (IntLit 1)))],"")
runParser program "x !! 1 := 2" == Just ([AssignAt "x" (IntLit 1) (IntLit 2)],"")
runParser program "x !! 1 := x !! 2" == Just ([AssignAt "x" (IntLit 1) (Index (Var "x") (IntLit 2))],"")
```

### Interpreter

Egészítsd ki a `Val` típust egy `VList :: [Val] -> Val` konstruktorral. __(1 pont)__

Az új műveletek működése a következő:

- A `List` literál értékelje ki a benne tárolt összes kifejezést és adja őket
  egy `VList`-ben vissza.
- Az `Index` művelet csak akkor helyes, ha az első paraméter egy `VList`-re, a
  második pedig egy `VInt`-re értékelődik ki. Egyéb esetben dobjunk `error`-t.
- Ha a típusok stimmelnek, akkor az első paraméter értékébe indexeljünk bele a
  második paraméter értékével.
- Az `AssignAt` művelet első paramétere egy változónevet reprezentáljon, ami egy
  `VList`-re értékelődik majd ki, a második paramétere pedig egy `VInt`-re. Ha a
  típusok stimmelnek, akkor írjuk fölül az adott változó i-edik elemét a harmadik
  paraméterrel kiértékelt értékre.
- Az indexelés mindkét műveletnél 0-tól kezdődik.
- A lista literálok minden más művelettel típushibát dobnak (kivéve az
  egyenlőségnél, ott két lista akkor egyenlő, ha az összes elemük és a hosszuk
  egyenlő).

__(3 pont)__

Tesztek a működésre:
```hs
run "x := 1; y := [1,2,3,4,5]" == [("x",VInt 1),("y",VList [VInt 1,VInt 2,VInt 3,VInt 4,VInt 5])]
run "x := [1,2]; y := [3,x]; if x == (y !! 1) then x := [1,2,3] else x := x + 1 end" == [("x",VList [VInt 1,VInt 2,VInt 3]),("y",VList [VInt 3,VList [VInt 1,VInt 2]])]
run "x := 0; y := [1,2,3,4,5]; while not (x == 5) do y !! x := (y !! x) + x; x := x + 1 end" == [("x",VInt 5),("y",VList [VInt 1,VInt 3,VInt 5,VInt 7,VInt 9])]
```


Haskell kód:
```hs
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

data Stream a = Append a (Stream a) | Cycle [a]
    deriving Show

infixr 5 `Append`

s1 :: Stream Int
s1 = 1 `Append` 2 `Append` (Cycle [3])

s2 :: Stream Int
s2 = Cycle [1..10]

s3 :: Stream Bool
s3 = True `Append` (not <$> s3)


instance Eq a => Eq (Stream a) where

-- elég a foldr/foldMap közül az egyik
instance Foldable Stream where

instance Functor Stream where

-- elég a sequenceA/traverse közül az egyik
instance Traversable Stream where


streamToList :: Stream a -> [a]
streamToList = undefined

insertBeforeCycle :: Stream a -> [a] -> Stream a
insertBeforeCycle = undefined

readStream :: IO (Stream Int)
readStream = undefined

reverseSt :: Stream a -> Stream a
reverseSt = undefined

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
```
