A feladatsor megoldására 2 óra áll rendelkezésre. A vizsgán tetszőleges
segédeszköz használható. Ennek a feladatleírásnak a végén megtalálható a
kitöltendő Haskell fájl. Megjegyzés: a pontozás nem az automatikus tesztesetek
alapján történik, hanem a megoldás figyelembe vételével. Részpontszámok
lehetségesek.

Ponthatárok:

  - __2__: 10-11
  - __3__: 12-14
  - __4__: 15-16
  - __5__: 17-20

Adott az alábbi fa típus:

```haskell
data Tree a = Leaf1 a | Leaf2 a a | Node (Tree a) (Maybe (Tree a))
  deriving (Eq, Ord, Show)
```

Egy lehetséges értéke:

```haskell
ex1 :: Tree Int
ex1 =
  Node
    (Leaf2 2 1)
    (Just (Node
      (Leaf1 10)
      (Just (Node
         (Leaf2 5 6)
         Nothing))))
```

Írjunk `Functor` példányt a `Tree` típushoz! __(1 pont)__

Írjunk `Foldable` példányt a `Tree` típushoz! __(2 pont)__

Definiáljuk azt a függvényt, ami visszaadja a balról első
`a` értéket egy fából. Példa: `leftmost ex1 == 2`. __(1 pont)__

```haskell
leftmost :: Tree a -> a
```

Definiáljunk egy függvényt, ami visszadja balról az első `a` típusú értéket, ami
egy `Leaf2` konstruktorban található és igaz rá egy feltétel.  Ha nincs ilyen
érték, akkor `Nothing` az eredmény. __(2 pont)__

```haskell
findInLeaf2 :: (a -> Bool) -> Tree a -> Maybe a
```

Példák:

```haskell
findInLeaf2 (==2) ex1 == Just 2
findInLeaf2 (==10) ex1 == Nothing
findInLeaf2 (>4) ex1 == Just 5
```

Írjunk `Traversable` példányt a `Tree` típushoz! __(1 pont)__

Definiáljunk egy függvényt, ami minden `a` értéket felcímkéz az attól balra levő
`Leaf1` konstruktorok számával! Tipp: használjuk a `State` monádot. __(2 pont)__

```haskell
countLeaf1s :: Tree a -> Tree (a, Int)
```

Példák:

```haskell
countLeaf1s ex1 ==
  Node
    (Leaf2 (2,0) (1,0))
    (Just (Node
      (Leaf1 (10,0))
      (Just (Node (Leaf2 (5,1) (6,1)) Nothing))))

countLeaf1s (Node (Leaf2 1 2) (Just (Leaf2 3 4))) ==
  Node (Leaf2 (1,0) (2,0)) (Just (Leaf2 (3,0) (4,0)))

countLeaf1s (Node (Leaf1 10) (Just (Node (Leaf1 20) (Just (Leaf1 30))))) ==
  Node (Leaf1 (10,0)) (Just (Node (Leaf1 (20,1)) (Just (Leaf1 (30,2)))))
```

Cseréljük ki egy fában levő `Leaf2` konstruktorokban található `a` értékeket egy
adott lista elemeire, balról-jobbra bejárási sorrendben. Tipp: használjuk a
`State` monádot. __(3 pont)__

```haskell
replaceLeaf2s :: [a] -> Tree a -> Tree a
```

Példák a működésre:

```haskell
replaceLeaf2s [8,9,10,11] ex1 ==
  Node (Leaf2 8 9) (Just (Node (Leaf1 10) (Just (Node (Leaf2 10 11) Nothing))))

replaceLeaf2s [] ex1 == ex1
replaceLeaf2s [8..20] ex1 == replaceLeaf2s [8,9,10,11] ex1
```

## `While` nyelv (8 pont)

A feladatunk, hogy kiegészítsük a `While` nyelvet `Int` és `Bool` értékek
beolvasásával egy `[String]`-el reprezentált input stream-ről.

### Absztrakt szintaxis

Vegyünk fel `ReadInt :: Exp` és `ReadBool :: Exp` konstruktorokat! __(1 pont)__

### Parser

Egészítsük ki a kifejezések olvasását. Példa a `ReadInt` és `ReadBool` konkrét szintaxisára:

```haskell
x := readInt; y := readBool; z := x + readInt
```
Tehát egyszerűen `readInt` és `readBool` kulcsszavakat vezetünk be. Ne felejtsük el kiegészíteni
a kulcsszavak kezelését. __(2 pont)__

### Interpretáció

A `readInt` és `readBool` műveletek egy `[String]` által reprezentált állapotból
olvasnak be. Egészítsük ki az állapotot úgy, hogy az `Env` mellett tartalmazzon
egy `[String]` értéket is! Ehhez módosítani kell a program olyan részeit, ahol
az állapotot használjuk. A `runProg` függvény új típusa legyen `String ->
[String] -> (Env, [String])`.  __(2 pont)__

Egészítsük ki az `evalExp` függvényt `ReadInt` és `ReadBool` értelmezésével! A
működés a következő:

- Ha a `[String]` állapot üres, dobjunk hibát, egyébként vegyük le az első
  `String`-et és módosítsuk a maradék listára az állapotot.
- `ReadInt` esetén, ha a levett `String` parsolható `Int` literálként, akkor
  visszaadjuk az olvasott `Int`-et, egyébként dobjunk hibát.
- `ReadBool` esetén, ha a levett `String` parsolható `Bool` literálként, akkor
  visszaadjuk az olvasott `Bool`-t, egyébként dobjunk hibát.

Az olvasáshoz felhasználhatjuk a korábban definiált parser-eket és a
`runParser`-t.  A levett `String` teljes egészét ki kell olvasnunk ahhoz, hogy a
művelet sikeres legyen! Használjuk az `eof` parser-t ennek az
ellenőrzéséhez. __(3 pont)__

Példa a működésre:

```haskell
runProg "x := readInt; y := x + readInt" ["10", "20"] == ([("y",Left 30),("x",Left 10)],[])
```

# Ponthatárok

  - __2__: 10-11
  - __3__: 12-14
  - __4__: 15-16
  - __5__: 17-

## Vizsgafájl


```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Data.Char

-- State monad
--------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  State f >>= g = State $ \s -> case f s of
    (a, s') -> runState (g a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= \s -> put (f s)

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--------------------------------------------------------------------------------
--                              Feladatok
--------------------------------------------------------------------------------

data Tree a = Leaf1 a | Leaf2 a a | Node (Tree a) (Maybe (Tree a))
  deriving (Eq, Ord, Show)

ex1 :: Tree Int
ex1 =
  Node
    (Leaf2 2 1)
    (Just (Node
      (Leaf1 10)
      (Just (Node
         (Leaf2 5 6)
         Nothing))))

-- 1
-- instance Functor Tree where

-- 2
-- instance Foldable Tree where

-- 1
leftmost :: Tree a -> a
leftmost = undefined

-- 2
findInLeaf2 :: (a -> Bool) -> Tree a -> Maybe a
findInLeaf2 = undefined

-- 1
-- instance Traversable Tree where

-- 2
countLeaf1s :: Tree a -> Tree (a, Int)
countLeaf1s = undefined

-- 3
replaceLeaf2s :: [a] -> Tree a -> Tree a
replaceLeaf2s = undefined

-- összesen: 12

--------------------------------------------------------------------------------
--                  While nyelv parser + interpreter
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing     -> Nothing
      Just(a, s') -> runParser (g a) s'

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (==c)

string :: String -> Parser ()
string = mapM_ char

ws :: Parser ()
ws = () <$ many (char ' ' <|> char '\n')

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 pa pb = (:) <$> pa <*> many (pb *> pa)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy pa pb = sepBy1 pa pb <|> pure []

anyChar :: Parser Char
anyChar = satisfy (const True)

-- While nyelv
------------------------------------------------------------

data Exp
  = Add Exp Exp    -- a + b
  | Mul Exp Exp    -- a * b
  | Var Name       -- x
  | IntLit Int
  | BoolLit Bool   -- true|false
  | Not Exp        -- not e
  | And Exp Exp    -- a && b
  | Or Exp Exp     -- a || b
  | Eq Exp Exp     -- a == b
  | Lt Exp Exp     -- a < b
  deriving Show

type Program = [Statement]
type Name    = String

data Statement
  = Assign Name Exp         -- x := e
  | While Exp Program       -- while e do p1 end
  | If Exp Program Program  -- if e then p1 else p2 end
  | Block Program           -- {p1}       (lokális scope)

  deriving Show


-- While parser
--------------------------------------------------------------------------------

{-
Parser a While nyelvhez. A szintaxist az Exp és Statement definíciónál látahtó
fenti kommentek összegzik, továbbá:

  - mindenhol lehet whitespace tokenek között
  - a Statement-eket egy Program-ban válassza el ';'
  - Az operátorok erőssége és assszociativitása a következő:
      infixr 2 ||
      infixr 3 &&
      infix  4 ==
      infix  4 <
      infixl 6 +
      infixl 7 *
  - "not" erősebben köt minden operátornál.
  - A kulcsszavak: not, and, while, do, if, end, true, false.
  - A változónevek legyenek betűk olyan nemüres sorozatai, amelyek *nem* kulcsszavak.
    Pl. "while" nem azonosító, viszont "whilefoo" már az!

Példa szintaktikilag helyes programra:

  x := 10;
  y := x * x + 10;
  while (x == 0) do
    x := x + 1;
    b := true && false || not true
  end;
  z := x
-}

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

keywords :: [String]
keywords = ["not", "and", "while", "do", "if", "end", "true", "false"]

pIdent :: Parser String
pIdent = do
  x <- some (satisfy isLetter) <* ws
  if elem x keywords
    then empty
    else pure x

pBoolLit :: Parser Bool
pBoolLit = (True  <$ string' "true")
       <|> (False <$ string' "false")

pIntLit :: Parser Int
pIntLit = read <$> (some (satisfy isDigit) <* ws)

pAtom :: Parser Exp
pAtom = (BoolLit <$> pBoolLit)
      <|> (IntLit <$> pIntLit)
      <|> (Var <$> pIdent)
      <|> (char' '(' *> pExp <* char' ')')

pNot :: Parser Exp
pNot =
      (Not <$> (string' "not" *> pAtom))
  <|> pAtom

pMul :: Parser Exp
pMul = foldl1 Mul <$> sepBy1 pNot (char' '*')

pAdd :: Parser Exp
pAdd = foldl1 Add <$> sepBy1 pMul (char' '+')

pEqOrLt :: Parser Exp
pEqOrLt =
  pAdd >>= \e ->
        (Eq e <$> (string' "==" *> pAdd))
    <|> (Lt e <$> (string' "<"  *> pAdd))
    <|> pure e

pAnd :: Parser Exp
pAnd = foldr1 And <$> sepBy1 pEqOrLt (string' "&&")

pOr :: Parser Exp
pOr = foldr1 Or <$> sepBy1 pAnd (string' "||")

pExp :: Parser Exp
pExp = pOr

pProgram :: Parser Program
pProgram = sepBy pStatement (char' ';')

pStatement :: Parser Statement
pStatement =
        (Assign <$> pIdent <*> (string' ":=" *> pExp))
    <|> (While <$> (string' "while" *> pExp)
               <*> (string' "do" *> pProgram <* string' "end"))
    <|> (If <$> (string' "if"   *> pExp)
            <*> (string' "then" *> pProgram)
            <*> (string' "else" *> pProgram <* string' "end"))
    <|> (Block <$> (char' '{' *> pProgram <* char' '}'))

pSrc :: Parser Program
pSrc = ws *> pProgram <* eof


-- Interpreter
------------------------------------------------------------

{-
Interpreter a While nyelvhez.

Kifejezések:
  - A logikai és artimetikai műveletek kiértékelése értelemszerű. Ha nem
    megfelelő típusú értéket kapunk argumentumokra, dobjunk "error"-al hibát.
  - Az == operátor működik, ha mindkét argumentum Bool, vagy ha mindkét argumentum
    Int, az eredmény mindig Bool.

Változó scope és értékadás kezelése:
  - Új scope-nak számít:
    - minden "while" kifejezés teste
    - minden "if" kifejezés két ága
    - minden új Block (a szintaxisban pl "x := 0; {y := x; x := x}"-nél
      a kapcsos zárójeles utasítássorozat új blokkban van).

  - ha egy új változónak értéket adunk, akkor felvesszük a környezet elejére
  - ha egy meglévő változónak értéket adunk, akkor update-eljük a változó értékét
  - amikor az interpreter végez egy scope kiértékeléséval, eldobja az összes,
    scope-ban újonnan felvett változót a környezetből.
-}

type Val  = Either Int Bool
type Env  = [(Name, Val)]
type Eval = State Env

binOp :: String
      -> Exp
      -> Exp
      -> Either (Int -> Int -> Int) (Bool -> Bool -> Bool)
      -> Eval Val
binOp opName e1 e2 f = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (f, v1, v2) of
    (Left f , Left n1 , Left n2 ) -> pure (Left  (f n1 n2))
    (Right f, Right b1, Right b2) -> pure (Right (f b1 b2))
    _                             -> error ("type error in " ++ opName ++ " argument")

evalExp :: Exp -> Eval Val
evalExp e = case e of
  Add e1 e2 -> binOp "+"  e1 e2 (Left (+))
  Mul e1 e2 -> binOp "*"  e1 e2 (Left (*))
  And e1 e2 -> binOp "&&" e1 e2 (Right (&&))
  Or  e1 e2 -> binOp "||" e1 e2 (Right (||))
  Eq  e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (Left  n1, Left  n2) -> pure (Right (n1 == n2))
      (Right b1, Right b2) -> pure (Right (b1 == b2))
      _                    -> error "type error in == arguments"
  Lt  e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (Left  n1, Left  n2) -> pure (Right (n1 < n2))
      _                    -> error "type error in < arguments"
  Var x -> do
    env <- get
    case lookup x env of
      Nothing -> error ("variable not in scope: " ++ x)
      Just v  -> pure v
  IntLit n -> pure (Left n)
  BoolLit b -> pure (Right b)
  Not e -> do
    v <- evalExp e
    case v of
      Right b -> pure (Right (not b))
      _       -> error "type error in \"not\" argument"

newScope :: Eval a -> Eval a
newScope ma = do
  env <- (get :: State Env Env)
  a <- ma
  modify (\env' -> drop (length env' - length env) env')
  pure a

updateEnv :: Name -> Val -> Env -> Env
updateEnv x v env =
  case go env of
    Nothing   -> (x, v):env
    Just env' -> env'
  where
    go :: Env -> Maybe Env
    go [] = Nothing
    go ((x', v'):env)
      | x == x'   = Just ((x, v):env)
      | otherwise = ((x', v'):) <$> go env

evalSt :: Statement -> Eval ()
evalSt s = case s of
  Assign x e -> do
    v <- evalExp e
    modify (updateEnv x v)
  While e p -> do
    v <- evalExp e
    case v of
      Right b -> if b then newScope (evalProg p) >> evalSt (While e p)
                      else pure ()
      _       -> error "type error: expected a Bool condition in \"while\" expression"
  If e p1 p2 -> do
    v <- evalExp e
    case v of
      Right b -> if b then newScope (evalProg p1)
                      else newScope (evalProg p2)
      _       -> error "type error: expected a Bool condition in \"if\" expression"
  Block p ->
    newScope (evalProg p)

evalProg :: Program -> Eval ()
evalProg = mapM_ evalSt


-- interpreter
--------------------------------------------------------------------------------

runProg :: String -> Env
runProg str = case runParser pSrc str of
  Nothing     -> error "parse error"
  Just (p, _) -> execState (evalProg p) []

p1 :: String
p1 = "x := 10; y := 20; {z := x + y}; x := x + 100"
```
