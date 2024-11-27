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
data JaggedList a = JCons [a] (JaggedList a) | JNil
  deriving Show

infixr 5 `JCons`
```

Példák a listára:

```hs
j1 :: JaggedList Int
j1 = [1,2,3] `JCons` [4,5,6] `JCons` JNil

j2 :: JaggedList Char
j2 = "alma" `JCons` "barack" `JCons` "kókuszdió" `JCons` JNil

j3 :: JaggedList Int
j3 = JCons [1] j3
```

Írd meg a fenti típusa az `Eq, Foldable, Functor` és `Traversable` következő instance-okat! __(4 pont)__

Példák:

```hs
j1 == j1
j3 /= j1
fmap (*2) j1 ==  [2,4,6] `JCons` [8,10,12] `JCons` JNil
fmap (const "text") j2 == ["text","text","text","text"] `JCons` ["text","text","text","text","text","text"] `JCons` ["text","text","text","text","text","text","text","text","text"] `JCons` JNil
sum j1 == 21
not $ all (>1) j3
sequenceA (fmap (\x -> if x < 6 then Just x else Nothing) j1) == Nothing
traverse (\x -> if x < 7 then Just x else Nothing) j1 == Just j1
evalState (traverse (\x -> if odd x then get >>= \n -> Just (n,x) <$ put (n + 1) else pure Nothing) j1) 0 == [Just (0,1),Nothing,Just (1,3)] `JCons` [Nothing,Just (2,5),Nothing] `JCons` JNil
```

Definiálj egy függvényt, ami egy `JaggedList` leghosszab oszlopának hosszát visszaadja (itt az oszlop a `JCons` konstruktorban lévő `[a]` típusú paramétert jelenti!) __(1 pont)__

```hs
longestColumn :: JaggedList a -> Int
longestColumn = undefined
```

Példák a működésre:

```hs
longestColumn j1 == 3
longestColumn j2 == 9
longestColumn JNil == 0
longestColumn ([1..10] `JCons` [] `JCons` JNil) == 10
```

Definiálj egy függvényt, ami egy paraméterül kapott függvényt alkalmaz egy `JaggedList` összes oszlopára! __(2 pont)__

```hs
mapOverColumns :: ([a] -> [b]) -> JaggedList a -> JaggedList b
mapOverColumns = undefined
```

Példák a működésre:

```hs
mapOverColumns (map (+1)) j1 == fmap (+1) j1
mapOverColumns (\xs -> case xs of { [] -> [1..10]; (y:ys) -> [y]; }) ([] `JCons` j1) == [1,2,3,4,5,6,7,8,9,10] `JCons` [1] `JCons` [4] `JCons` JNil
mapOverColumns (concatMap (\x -> replicate x x)) j1 == [1,2,2,3,3,3] `JCons` [4,4,4,4,5,5,5,5,5,6,6,6,6,6,6] `JCons` JNil
```

Definiálj egy függvényt, amely kitölti egy `JaggedList` összes oszlopát úgy, hogy a hosszuk megegyezzen a `JaggedList` leghosszabb oszlopával! A kitöltéshez használjuk a (már létező) `mempty :: Monoid a => a` függvényt. __(2 pont)__

```hs
matricize :: Monoid a => JaggedList a -> JaggedList a
matricize = undefined
```

Példák a működésre (ezek futtatásához a `Data.Semigroup` modul importálása szükséges):

```hs
matricize (JCons [] $ JCons ["a", "b", "c"] JNil) == JCons ["","",""] (JCons ["a","b","c"] JNil)
matricize (() <$ j2) == [(),(),(),(),(),(),(),(),()] `JCons` [(),(),(),(),(),(),(),(),()] `JCons` [(),(),(),(),(),(),(),(),()] `JCons` JNil
fmap getSum (matricize $ Sum <$> (JCons [1..5] j1)) == [1,2,3,4,5] `JCons` [1,2,3,0,0] `JCons` [4,5,6,0,0] `JCons` JNil
fmap getProduct (matricize $ Product <$> (JCons [1..5] j1)) == [1,2,3,4,5] `JCons` [1,2,3,1,1] `JCons` [4,5,6,1,1] `JCons` JNil
fmap getSum (matricize $ Sum <$> (JCons [] j1)) == [0,0,0] `JCons` [1,2,3] `JCons` [4,5,6] `JCons` JNil
```

Definiálj egy függvényt, ami a konzolba kirajzol egy `JaggedList` értéket. Az
elemek space-el legyenek elválasztva és minden lista, ami a `JCons`
konstruktorokban van külön sorba legyen kiírva (extra whitespace lehet a sor
végén)! __(3 pont)__

**Segítség:** A feladatmegoldásban érdemes a `putStr` és `putStrLn` függvényeket használni.
```hs
prettyPrint :: Show a => JaggedList a -> IO ()
prettyPrint = undefined
```
A `j1` kirajzolása:
```
1 2 3
4 5 6
```
A `j2` kirajzolása:
```
'a' 'l' 'm' 'a'
'b' 'a' 'r' 'a' 'c' 'k'
'k' 'ó' 'k' 'u' 's' 'z' 'd' 'i' 'ó'
```

# `while` nyelv kiegészítése (8 pont)

A feladat a `while` nyelv kiegészítése mintaillesztéssel.

Másold be az alábbi deklarációt a kódba:
```hs
data Pattern = ByValue Val | Wildcard | Named String
  deriving (Eq, Show)
```
Add a szintaxishoz a `PatternMatch :: Exp -> [(Pattern, Program)] -> Statement` konstruktort! __(1 pont)__

### Parser

A szintaxis a következő:

- A mintaillesztés a `match` kulccszóval kezdődik. A kulcsszó után space-el
  elválasztva egy kifejezés lesz, amire majd mintaillesztünk. A kifejezés után
  egy `:` legyen.
- Ezután `;`-kel elválasztva legyenek a minták és az azokhoz asszociált programok:
  - Először a mintát, utána egy jobbra nyilat (`->`), majd egy programot parse-olunk.
  - A minta egy literál kifejezés (`1`, `2`, `3`, `true`, `false`, stb.) egy alsóvonal a
    wildcardnak (`_`) vagy egy tetszőleges változonév lehet.
- Az állítás az `end` kulcsszóval legyen bezárva.
- A `match` kulcsszót adjuk hozzá a kulcsszavak listájához.

__(3 pont)__
Példák a működésre:
```hs
runParser statement "match 1: 1 -> x := 0 end" == Just (PatternMatch (IntLit 1) [(ByValue (VInt 1),[Assign "x" (IntLit 0)])],"")
runParser statement "match x: 2 -> x := 0; y := 1; _ -> x := 1 end" == Just (PatternMatch (Var "x") [(ByValue (VInt 2),[Assign "x" (IntLit 0),Assign "y" (IntLit 1)]),(Wildcard,[Assign "x" (IntLit 1)])],"")
runParser statement "match := 1" == Nothing
runParser statement "match x: x -> x := x end" == Just (PatternMatch (Var "x") [(Named "x",[Assign "x" (Var "x")])],"")
runParser statement "match y + x: true -> match true: false -> x := 1; _ -> x := 2 end; 2 -> x := 3; x -> x := 4 end" == Just (PatternMatch (Add (Var "y") (Var "x")) [(ByValue (VBool True),[PatternMatch (BoolLit True) [(ByValue (VBool False),[Assign "x" (IntLit 1)]),(Wildcard,[Assign "x" (IntLit 2)])]]),(ByValue (VInt 2),[Assign "x" (IntLit 3)]),(Named "x",[Assign "x" (IntLit 4)])],"")
```

### Interpreter

Egészítsd ki az interpretert az új konstrukciók kiértékelésével! __(4 pont)__

A működés a következő:

- Értékeljük ki először az illesztendő kifejezést.
- A kiértékelés az első illeszkedő mintához tartozó programmal folytatódik.
- Illeszkedés vizsgálata:
  - Ha a minta `ByValue` és az érték típusa nem egyezik az illesztendő érték típusúval, akkor
    a minta nem illeszkedik, és **nem dobunk típushibát**. Ha a típusok egyeznek és az értékek is
	egyenlők, akkor a minta illeszkedik.
  - `Wildcard` minta minden értékre illeszkedik.
  - `Named` minta is illeszkedik minden értékre, viszont ekkor a környezetbe lokálisan felvesszük
    a név-érték párost. Használd az `inNewScope` és az `updateEnv` függvényeket.
- Ha az érték egyik mintára sem illeszkedik, akkor az állítás nem csinál semmit.

Példák a működésre:
```hs
run "x := 1; match x: 1 -> x := 2 end" == [("x",VInt 2)]
run "x := 1; match x: true -> x := 3; y -> x := y + 1 end" == [("x",VInt 2)]
run "x := 1; match x: _ -> x := 2; _ -> x := 3 end" == [("x",VInt 2)]
run "x := 1; y := 0; while not (x == 5) do match (x + 1): 3 -> y := 10000; 5 -> y := 2; x := 5; 2 -> x := 4 end end" == [("x",VInt 5),("y",VInt 2)]
```

Haskell kód:

```haskell
{-# LANGUAGE DeriveFunctor, InstanceSigs #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Semigroup

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

data JaggedList a = JCons [a] (JaggedList a) | JNil deriving Show

infixr 5 `JCons`

j1 :: JaggedList Int
j1 = [1,2,3] `JCons` [4,5,6] `JCons` JNil

j2 :: JaggedList Char
j2 = "alma" `JCons` "barack" `JCons` "kókuszdió" `JCons` JNil

j3 :: JaggedList Int
j3 = JCons [1] j3

longestColumn :: JaggedList a -> Int
longestColumn = undefined

mapOverColumns :: ([a] -> [b]) -> JaggedList a -> JaggedList b
mapOverColumns = undefined

matricize :: Monoid a => JaggedList a -> JaggedList a
matricize = undefined

prettyPrint :: Show a => JaggedList a -> IO ()
prettyPrint = undefined

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
