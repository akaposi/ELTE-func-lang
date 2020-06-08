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
data Tree a = Node (Either a (Tree a)) (Tree a) | TriLeaf a a a
  deriving (Eq, Ord, Show)
```

Egy lehetséges értéke:

```haskell
ex1 :: Tree Int
ex1 =
  Node
   (Left 1)
    (Node
      (Right (TriLeaf 2 3 4))
      (TriLeaf 5 6 7))
```

Írjunk `Functor` példányt a `Tree` típushoz! __(1 pont)__

Írjunk `Foldable` példányt a `Tree` típushoz! __(2 pont)__

Definiáljuk azt a függvényt, ami visszaadja a balról első
`a` értéket egy fából. Példa: `leftmost ex1 == 1`. __(1 pont)__

```haskell
leftmost :: Tree a -> a
```

Definiáljunk egy függvényt, ami visszadja balról az első `a` típusú értéket, ami
egy `TriLeaf` konstruktor második eleme, és hamis rá egy feltétel.  Ha nincs ilyen
érték, akkor `Nothing` az eredmény. __(2 pont)__

```haskell
findComplementInTriLeaf :: (a -> Bool) -> Tree a -> Maybe a
```

Példák:

```haskell
findComplementInTriLeaf (==3) ex1 == Just 6
findComplementInTriLeaf (/=3) ex1 == Just 3
findComplementInTriLeaf (<4) ex1 == Just 6
findComplementInTriLeaf (4<) ex1 == Just 3
findComplementInTriLeaf (\x -> x == 3 || x == 6) ex1 == Nothing
```

Írjunk `Traversable` példányt a `Tree` típushoz! __(1 pont)__

Definiáljunk egy függvényt, ami minden `a` értéket felcímkéz az attól balra levő
`TriLeaf` konstruktorok számával! Tipp: használjuk a `State` monádot. __(2 pont)__

```haskell
countTriLeafs :: Tree a -> Tree (a, Int)
```

Példák:

```haskell
countTriLeafs ex1 ==
  Node
  (Left (1,0))
    Node
    (Right (TriLeaf (2,0) (3,0) (4,0)))
    (TriLeaf (5,1) (6,1) (7,1))

countTriLeafs (Node (Right (TriLeaf 1 2 3)) (Node (Left 4) (TriLeaf 0 0 0))) ==
  Node (Right (TriLeaf (1,0) (2,0) (3,0))) (Node (Left (4,1)) (TriLeaf (0,1) (0,1) (0,1)))

countTriLeafs (Node (Right $ TriLeaf 1 1 1) (Node (Left 2) (TriLeaf 3 4 5))) == (Node (Right $ TriLeaf (1,0) (1,0) (1,0)) (Node (Left (2,1)) (TriLeaf (3,1) (4,1) (5,1))))
```

Cseréljük ki egy fában levő `a` értékeket egy adott lista elemeire periodikusan,
balról-jobbra bejárási sorrendben. A periodikusan azt jelenti, hogyha a lista
végére értunk, a következő elem helyére ismét a lista legelső elemét tegyük. Ha
a lista eredetileg is üres volt, akkor hagyjuk a fát helyben. Tipp: használjuk a
`State` monádot. __(3 pont)__

```haskell
periodicReplace :: [a] -> Tree a -> Tree a
```

Példák a működésre:

```haskell
periodicReplace [7,8,9] ex1 ==
  Node
   (Left 7)
    Node
      (Right (TriLeaf 8 9 7))
      (TriLeaf 8 9 7)

periodicReplace [] ex1 == ex1
periodicReplace [0,1] ex1 == periodicReplace [0,1,0,1] ex1
```

## `While` nyelv (8 pont)

Feladatunk, hogy kiegeszitsuk a `While` nyelvet egy globális, egész számokat
tartalmazó stack-kel! Ehhez a stack-hez a program tetszőleges pontjárol hozzá
lehet férni, és a szokásos stack műveletekkel lehet írni/olvasni (`peek`, `pop`,
`push`).

### Absztrakt szintaxis

Vegyük fel a `Peek :: Exp`, `Push :: Exp -> Statement` és `Pop :: Statement`
konstrukorokat! __(1 pont)__

### Parser

Egészítsük ki a kifejezések olvasását. Példa a stack műveleteket konkrét szintaxisára:

```haskell
push 5; push (1+2); x := peek; pop; y := peek; z := x + y
```
Tehát egyszerűen `peek`, `pop` és `push` kulcsszavakat vezetünk be. Ne felejtsük el kiegészíteni a kulcsszavak kezelését. __(2 pont)__

### Interpretáció

Első lépésként egészitsuk ki a jelenlegi program állapotot egy `[Int]` értékkel
is. Ezzel a listával fogjuk reprezentálni a globális stack aktuális állapotát.
Ehhez módosítani kell a program olyan részeit, ahol az állapotot használjuk. A
`runProg` függvény új típusa legyen `String -> (Env, [Int])`. Valamint ügyeljunk
arra, hogy a stack a program eléjen legyen ures! __(2 pont)__

Egészítsük ki az `evalExp` függvényt `peek` értelmezésével, valamint az `evalSt`
fuggvényt a `Pop` es `Push` értelmezésével! A működés a következő:

- `peek` kifejezéssel ki tudjuk olvasni a stack legfelső elemét, anélkül hogy
  módositanánk a stack-et. __(1 pont)__
- A `push` utasitással tudunk a stack tetejére egy egesz számot tenni. __(1 pont)__
- A `pop` utasitassal tudjuk eltávolitani a stack tetején lévo elemet. Ha a
  stack üres, akkor dobjunk hibat! __(1 pont)__


Példa a működésre:

```haskell
runProg "push 5; push (1+2); x := peek; pop; y := peek; z := x + y" == ([("z",Left 8),("y",Left 5),("x",Left 3)],[5])
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

data Tree a = Node (Either a (Tree a)) (Tree a) | TriLeaf a a a
  deriving (Eq, Ord, Show)

ex1 :: Tree Int
ex1 =
  Node
   (Left 1)
    (Node
      (Right (TriLeaf 2 3 4))
      (TriLeaf 5 6 7))

-- 1
-- instance Functor Tree where

-- 2
-- instance Foldable Tree where

-- 1
leftmost :: Tree a -> a
leftmost = undefined

-- 2
findComplementInTriLeaf :: (a -> Bool) -> Tree a -> Maybe a
findComplementInTriLeaf = undefined

-- 1
-- instance Traversable Tree where

-- 2
countTriLeafs :: Tree a -> Tree (a, Int)
countTriLeafs = undefined

-- 3
periodicReplace :: [a] -> Tree a -> Tree a
periodicReplace = undefined

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
