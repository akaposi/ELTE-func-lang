{-# language DeriveFunctor #-}

-- Monád folyt: State, lista, stb
------------------------------------------------------------

import Control.Monad  -- ap

newtype State s a = State {runState :: s -> (a, s)}
  -- deriving (Functor)


instance Functor (State s) where
  fmap f (State g) = State $ \s -> case g s of (a, s) -> (f a, s)

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  State f >>= g = State $ \s -> case f s of
    (a, s') -> (runState (g a)) s'

-- lekérdezi az állapotot
get :: State s s
get = State $ \s -> (s, s)

-- átírja az állapotot
put :: s -> State s ()
put s = State $ \_ -> ((), s)

-- alkalmaz egy függvényt az állapotra
modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put (f s)


-- tipikus feladat: interpreter (imperatív)
--   (gyakran gyakorlatban: hiba monád + state monád kombinációja)
--     (később: monád transzformer)
------------------------------------------------------------

-- [x := 10; x := x + 10; y := x]

--- "environment", programkörnyezet
type Env = [(String, Int)]

-- kifejezések (értékadás jobb oldalán álló dolog)
data Exp = IntLit Int | Add Exp Exp | Mul Exp Exp | Var String
  deriving Show

-- példa: (x + 10) + y
exp1 = Add (Add (Var "x") (IntLit 10)) (Var "y")

-- kiértékelés
-- (nincs feltétlen State-re szükség)
evalExp :: Exp -> State Env Int
evalExp (IntLit n) = pure n
evalExp (Add e1 e2) = do
  n1 <- evalExp e1
  n2 <- evalExp e2
  pure (n1 + n2)
evalExp (Mul e1 e2) = do
  n1 <- evalExp e1
  n2 <- evalExp e2
  pure (n1 * n2)
evalExp (Var x) = do
  env <- get
  case lookup x env of
    Nothing -> error ("nincs a környeztben változó: " ++ x)
    Just n  -> pure n

-- runState (evalExp exp1) [("x", 10), ("y", 20)]
-- == (40,[("x",10),("y",20)])

evalState :: State s a -> s -> a
evalState sa s = fst (runState sa s)

execState :: State s a -> s -> s
execState sa s = snd (runState sa s)

type Program = [(String, Exp)]

prog1 = [("x", IntLit 10), ("y", Var "x")]
-- x := 10; y := x

-- különböző design lehetőségek:
--   új változót kezelni: a) hiba b) felvesszük a környezetbe

update :: String -> Int -> Env -> Env
update x n []             = [(x, n)]
update x n ((x', n'):env)
  | x == x'    = (x', n):env  -- új érték beírása
  | otherwise  = (x', n'):update x n env

evalProgram :: Program -> State Env ()
evalProgram [] = pure ()
evalProgram ((x, exp):prog) = do
  n <- evalExp exp
  modify (update x n)
  evalProgram prog

-- segédfüggvény
runProgram :: Program -> Env
runProgram prog = execState (evalProgram prog) []

-- while nyelv: változó deklarálás, while ciklus, Int, Bool
-- extra feladat: kifejezések, állítások, interpreter


-- lista monád
------------------------------------------------------------

-- kérdés: []
-- instance Functor [] where
--   fmap = map

-- instance Applicative [] where
--   pure = return
--   (<*>) = ap

-- Prelude-ben benne van
-- instance Monad [] where
--   return = ?
--   (>>=)  = ?

returnList :: a -> [a]
returnList a = [a]

-- Prelude-ben van egy nevesített függvény, ami ez
bindList :: [a] -> (a -> [b]) -> [b]
bindList as f = concatMap f as


-- példa:
list1 :: [Int]
list1 = do
  x <- [0..10]
  y <- [0..10]
  pure $ x + y

list1' :: [Int]
list1' =
  [0..10] >>= \x ->
  [0..10] >>= \y ->
  pure $ x + y

-- bind-olt érték bejárja az összes lehetséges listaértéket
list1'' :: [Int]
list1'' =
  concatMap (\x -> concatMap (\y -> [x + y]) [0..10]) [0..10]

-- listakifejezés:
list1''' :: [Int]
list1''' = [x + y | x <- [0..10], y <- [0..10]]

-- listakifejezés: szintaktikus cukorka a do-notációra

list2 :: [Int]
list2 = [x + y | x <- [0..10], y <- [0..10], x + y < 6]

guardList :: Bool -> [()]
guardList True  = [()]
guardList False = []

-- példa:
list2' :: [Int]
list2' = do
  x <- [0..10]
  y <- [0..10]
  _ <- if (x + y < 6) then [()] else []  -- elvágja a lehetőségeket
                                         -- ha []
  -- _ <- guardList (x + y < 6)
  pure $ x + y


-- fmap definiálása monád metódussal
fmap' :: Monad m => (a -> b) -> m a -> m b
fmap' f ma = do
  a <- ma
  pure (f a)

-- map függvény (fmap) újradefiniálható a concatMap függvénnyel
fmap'' :: (a -> b) -> [a] -> [b]
fmap'' f ma = do
  a <- ma
  pure (f a)

-- direkt
map' :: (a -> b) -> [a] -> [b]
map' f as = concatMap (\a -> [f a]) as

-- (azt mindenképpen szeretnénk, hogy az fmap' megegyezzen az fmap-el)

-- példák:

-- pitagoraszi hármasok (az összes, végtelen listában)
ptriple :: [(Int, Int, Int)]
ptriple = do
  z <- [1..]
  x <- [1..z]
  y <- [1..z]
  guardList (x*x + y*y == z*z)
  pure (x, y, z)

-- pl. take 20 ptriple

-- az összes lehetséges részlista

-- első verzió
sublists :: [a] -> [[a]]
sublists []     = pure []     -- [[]]
sublists (a:as) = do
  -- két lehetőség: vagy hozzávesszük a-t az outputhoz, vagy pedig
  -- elhagyjuk
  as'  <- sublists as   -- as' részlistája as-nek
  aas' <- [a:as', as']  -- bejárjuk a két lehetőséget
  pure aas'

  -- (tömörebb verzió: monádtörvény szerint a fenti kifejezés
  --  átírható  erre)
  -- as'  <- sublists as   -- as' részlistája as-nek
  -- [a:as', as']


-- absztrahált verzió: filterM
-- Control.Monad modulban

-- filter monadikus verziója
-- (emlékeztető: mapMaybe :: (a -> Maybe b) -> [a] - Maybe [b])
--               mapM     :: Monad m => (a -> m b) -> [a] -> m [b])

-- filter általánosítása tetszőleges monádban
filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' f [] = pure []
filterM' f (a:as) = do
  b  <- f a
  as <- filterM' f as
  if b then pure (a:as) else pure as

-- lista monád: több lehetséges értéket ad vissza listában
--  "nemdeterminisztikus"
sublists' :: [a] -> [[a]]
sublists' = filterM' (\_ -> [True, False])
   -- ha True a feltétel, akkor benn hagyjuk az elemet
   -- ha False, akkor elhagyjuk
   -- mivel a lista monádban használjuk a filter-t,
   -- mindkét eset mindig megjelenik a végeredményben


-- mi történik mapM esetén lista monádban?
-- pl: mapM (\x -> [x + 1, x + 2]) [0, 1, 2, 3]
-- (ha az "összes lehetséges X-et" szeretnénk felsorolni/megkeresni,
--  akkor érdemes gondolkodni lista monádon)

-- (kitekintés: lista monád: egyszerű verziója a
--  logikai programozásnak)

-- (prolog/datalog programozás: alapból *minden* egy nemdeterminisztikus
-- "monád"-ban van)


-- Helyettesítés monád
------------------------------------------------------------

-- Föl lehet fögni ezt úgy, mint a Maybe, Either és lista monádok
-- általánosítása, valamilyen leveles fa típusokra.

-- példa:

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Show)

instance Applicative Tree where
  pure = return
  (<*>) = ap

instance Monad Tree where
  -- return :: a -> Tree a
  return = Leaf

  -- (>>=) :: Tree a -> (a -> Tree b) -> Tree b
  -- meghívjuk a függvény, az új fát behelyettesítjük
  Leaf a   >>= f = f a
  Node l r >>= f = Node (l >>= f) (r >>= f)

-- példa:
 -- Node (Leaf 0) (Leaf 1) >>= \x -> Node (Leaf x) (Leaf x)
 -- == Node (Node (Leaf 0) (Leaf 0)) (Node (Leaf 1) (Leaf 1))

-- Tree fmap-et visszakapjuk a Tree (>>=)-al!


-- Maybe/Either monád: helyettesítés monád speciális esete

-- Maybe: leveles fa + 1 darab nulláris elágazás
-- data Maybe a = Leaf a | Null

-- Either: leveles fa + 1 darab annotált nulláris elágazás
-- data Either a b = Leaf b | Null a

-- lista monád: nem ennyire egyszerű az eset
-- ha hunyorítunk, akkor a lista monád is helyettesítés
-- lista: lineáris fa
-- concatMap: behelyetessít minden listaelem helyére egy új listát.

-- (kitekintés: tetszőleges f-re, ami Functor f, létre tudunk hozni
--  egy fa típust, amire van helyettesítés Monad instance)

-- standard név: Free monád
-- data Free f a = Leaf a | Branch (f (Free f a))
-- instance Functor f => Monad (Free f) where ...
-- szintén "generikus" programozásban használható
--   (monadikus interpreterek általános megfogalmazása)
