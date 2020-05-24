A feladatsor megoldására 2 óra áll rendelkezésre. A vizsgán tetszőleges segédeszköz használható. Ennek a feladatleírásnak
a végén megtalálható a kitöltendő Haskell fájl. Megjegyzés: automatikus teszteset van a "while" rész előtti feladatokhoz, illetve a `pDoubleLit` részfeladathoz, de a pontozás nem ez alapján történik, hanem a megoldás figyelembe vételével. Részpontszámok lehetségesek.

Ponthatárok:

  - __2__: 10-11
  - __3__: 12-14
  - __4__: 15-16
  - __5__: 17-20

Adott az alábbi fa típus:

```haskell
data Tree a = Leaf a | Node1 a (Tree a) | Node2 (Tree a) (Tree a)
  deriving (Eq, Ord, Show)
```

Egy lehetséges értéke:

```haskell
ex1 :: Tree Int
ex1 =
  Node1 0
    (Node1 2
       (Node2
          (Node1 10 (Leaf 20))
          (Leaf 30)))
```

Írjunk `Functor` példányt a `Tree` típushoz! __(1 pont)__

Írjunk `Foldable` példányt a `Tree` típushoz! __(2 pont)__

Definiáljuk azt a függvényt, ami visszaadja a legutolsó (leginkább jobboldali)
`a` értéket egy fából. Példa: `rightmost ex1 == 30`. __(1 pont)__

```haskell
rightmost :: Tree a -> a
```

Definiáljunk egy függvényt, ami visszadja balról az első `a` típusú értéket egy
fából, amire igaz egy feltétel. Ha nincs ilyen érték, akkor `Nothing` az
eredmény. Példa: `findElem (>10) ex1 == Just 20`. __(1 pont)__

```haskell
findElem :: (a -> Bool) -> Tree a -> Maybe a
```

Írjunk `Traversable` példányt a `Tree` típushoz! __(1 pont)__

Definiáljuk a függvényt, amely megszámozza egy `Tree` elemeit! A bejárás
sorrendje legyen balról-jobbra preorder, azaz először a `Node1`-ben található
`a` értéket járjuk be, utána pedig a részfát. Tipp: használjuk a `State`
monádot. __(2 pont)__

```haskell
numberElems :: Tree a -> Tree (a, Int)
```

Cseréljük ki egy fában levő `a` értékeket egy adott lista elemeire,
balról-jobbra preorder sorrendben. Tipp: használjuk a `State` monádot. __(3 pont)__

```haskell
replace :: [a] -> Tree a -> Tree a
```

Példák a működésre:

```haskell
replace [8, 9] ex1 == Node1 8 (Node1 9 (Node2 (Node1 10 (Leaf 20)) (Leaf 30)))
replace [] ex1 == ex1
replace [0..] ex1 == Node1 0 (Node1 1 (Node2 (Node1 2 (Leaf 3)) (Leaf 4)))

```



## `While` nyelv (9 pont)

A feladatunk, hogy kiegészítsük a `while` nyelvet lebegőpontos értékekkel,
amelyek reprezentációja a Haskell-beli `Double` típus lesz. Ehhez módosítanunk
kell a szintaxist, a parsert és az interpretert.

### Absztrakt szintaxis

Vegyünk fel egy `DoubleLit :: Double -> Expr` konstruktort. Vegyünk fel továbbá
egy `Div :: Expr -> Expr -> Expr` konstruktort az eddigi kifejezések mellé. Ez
fogja reprezentálni a lebegőpontos osztás (`/`) műveletet. __(1 pont)__

### Parser

A következő feladatunk, hogy konkrét szintaxist is társítunk az újonnan megadott
nyelvi elemekhez. Néhány példa a `DoubleLit` literálok konkrét szintaxisára:

```haskell
0.0
-120.44
100.70
```

Definiáljunk egy `pDoubleLit :: Parser Double` nevű parsert, ami a `Double`
literálokat ismeri fel. A literálok tartalmazzanak pontosan egy tizedes pontot,
mivel a pont nélküli szám literált szeretnénk `IntLit`-ként
értelmezni továbbra is. Opcionálisan lehet `-` egy literál előtt
közvetlenül, ekkor negálni kell az olvasott értéket. Tipp: használjuk a `read :: String -> Double` függvényt a
végeredmény kiszámolásához. Ne feledkezzünk a `ws` olvasásáról a literál
után. __(2 pont)__

Egészítsük ki a `pExp` parser-t úgy, hogy a `Double` literálokat és az osztás
műveletet is felismerje.  A `/` bináris operátor kössön erősebben a `*`-nál,
gyengébben a `not` alkalmazásnál, és asszociáljon balra. A literál olvasásnál
ügyeljünk, hogy a `pIntLit` csak azután hívódjon, hogy a `pDouble` sikertelen
volt. __(2 pont)__

### Interpretáció
Definiáljuk újra a `Val` típust úgy, hogy `Double` literálokat is
tartalmazhasson, azaz haszáljunk három konstruktoros típust a `Val`-hoz. Ehhez
esetleg módosítanunk kell más definíciókon is, amelyek `Val`-t használnak, pl.
át kell nevezni a korábbi `Left` és `Right` konstruktorokat a kódban.
__(2 pont)__

Egészítsük ki a kifejezéseket értelmező függvényt (`evalExp`), hogy a `Double`
típusú értékeket is kezelje! __(2 pont)__

- Értelmezzük a `DoubleLit`-et a megfelelő `Val` konstruktorral.

- Értelmezzük a `Div` műveletet a Haskell-beli `/` osztással. Mindkét argumentum
  `Double` kell hogy legyen, egyébként dobjunk hibát. __Tipp__: egészítsük ki a `binOp` függvényt úgy,
  hogy `Double -> Double -> Double` típusú függvényt is kaphasson! Ehhez használjunk
  az `Either` helyett egy másik típust a bementben, aminek három konstruktora van.

__Extra feladat (+1) pontért__: Egészítsük ki az `Add`, `Mul`, `Eq` és `Lt`
  értelmezését úgy, hogy ezek akkor is működjenek, ha mindkét argumentum
  `Double` típusú.  Például az összeadás működjön két `Double`-ra is. A megoldás
  tetszőleges, érdemes a `binOp` függvényt újragondolni, hogy ezt a "túlterhelést"
  is tudja kezelni.

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

data Tree a = Leaf a | Node1 a (Tree a) | Node2 (Tree a) (Tree a)
  deriving (Eq, Ord, Show)

ex1 :: Tree Int
ex1 =
  Node1 0
    (Node1 2
       (Node2
          (Node1 10 (Leaf 20))
          (Leaf 30)))

instance Functor Tree where
  fmap = undefined

instance Foldable Tree where
  -- elég az egyik
  foldr   = undefined
  foldMap = undefined

rightmost :: Tree a -> a
rightmost = undefined

findElem :: (a -> Bool) -> Tree a -> Maybe a
findElem = undefined

instance Traversable Tree where
  traverse = undefined

numberElems :: Tree a -> Tree (a, Int)
numberElems = undefined

replace :: [a] -> Tree a -> Tree a
replace = undefined


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
