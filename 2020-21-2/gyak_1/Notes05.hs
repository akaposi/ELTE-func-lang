
-- BEAD : IO függvény, van benne rekurzió
--   holnap: első házi (4 pont), State monad feladat (határidő szorg időszak vége)

{-# language DeriveFunctor #-}

import Control.Monad
import Data.List (minimumBy)
import Data.Ord (comparing)
import Control.Arrow (first, second)

--------------------------------------------------------------------------------

-- nyomtassuk a legrövidebbet

f :: IO ()
f = do
  l1 <- getLine
  l2 <- getLine
  l3 <- getLine
  if (length l1 <= length l2) && (length l2 <= length l3) then do
    putStrLn l1
  else if length l2 <= length l3 then do
    putStrLn l2
  else do
    putStrLn l3

f' :: IO ()
f' = do
  l1 <- getLine
  l2 <- getLine
  l3 <- getLine
  if (length l1 <= length l2) && (length l2 <= length l3)
    then putStrLn l1
    else if length l2 <= length l3
      then putStrLn l2
      else putStrLn l3

f'' :: IO ()
f'' = do
  l1 <- getLine
  l2 <- getLine
  l3 <- getLine
  -- putStrLn $ snd $ minimum [(length l, l) | l <- [l1, l2, l3]]
  putStrLn $ snd $ minimum $ map (\l -> (length l, l)) [l1, l2, l3]

f''' :: IO ()
f''' = do
  ls <- replicateM 3 getLine
  putStrLn $ snd $ minimum $ map (\l -> (length l, l)) ls

-- data Ordering = LT | EQ | GT

f'''' :: IO ()
f'''' = do
  ls <- replicateM 3 getLine
  putStrLn $ minimumBy (\l1 l2 -> compare (length l1) (length l2)) ls

f''''' :: IO ()
f''''' = do
  ls <- replicateM 3 getLine
  putStrLn $ minimumBy (comparing length) ls

f2 :: IO ()
f2 = replicateM 3 getLine >>= putStrLn . minimumBy (comparing length)



-- State monád definíciója
--------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))
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
-- 100% praktikus összefoglaló State monádról

-- State s a              -- instance Monad (State s)
-- ma :: State s a        -- művelet, ami tud írni/olvasni egyetlen "s" típusú mutábilis változót

-- put    :: s -> State s ()         --   put s :: State s ()     az állapotot beállítja "s" értékre
-- get    :: State s s               --   viszatérési értékben lekérdezni a jelengi "s" állapotot
-- modify :: (s -> s) -> State s ()  --   (... ; x := f(x); ...) ~ (... >> modify f >> ...)

-- futtató függvények:

-- runState  :: State s a -> s -> (a, s)    -- runState : kap egy műveletet + kezdő állapotot, viszad: (visszatérési érték, végső állapot)
-- evalState :: State s a -> s -> a         -- csak értéket ad vissza
-- execState :: State s a -> s -> s         -- csak végső állapotot ad vissza


-- ki tudjuk fejezni:

-- f() :
--   var x = init;
--   x := x + 1
--   y := x
--   return (x, var)


modInt :: State Int ()
modInt = do
  modify (+10)
  modify (+10)
  replicateM_ 10 (modify (*2))

-- használat
res :: Int
res = execState modInt 0

modInt2 :: State Int Int
modInt2 = do
  n <- get
  if n == 0 then do
    modify (+20)
  else do
    modify (*10)
  pure 5

-- használat
res2 :: Int
res2 = evalState modInt2 10

res3 :: (Int, Int)
res3 = runState modInt2 10



data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Show)

-- Definiálj egy függvényt, ami kicsérli egy fa leveleiben tárolt értékeket
-- balról jobbra haladva egy megadott lista elemeire.
-- Használj State monádot!

-- pl: replaceLeaves [10, 20, 30] (Node (Leaf 2) (Leaf 3)) == Node (Leaf 10) (Leaf 20)
--     replacereplaceLeaves [5] (Leaf 10) == Leaf 5
--     replacereplaceLeaves [5] (Node (Leaf 0) (Node (Leaf 0) (Leaf 0))) ==
--        (Node (Leaf 5) (Node (Leaf 0) (Leaf 0)))

popDefault :: a -> State [a] a
popDefault deflt = do
  as <- get
  case as of
    []   -> pure deflt
    a:as -> do
      put as
      pure a

replaceLeaves :: [a] -> Tree a -> Tree a
replaceLeaves as t = evalState (go t) as where
  go :: Tree a -> State [a] (Tree a)
  go (Leaf a)   = Leaf <$> popDefault a
  go (Node l r) = do
    l' <- go l
    r' <- go r
    pure (Node l' r')
  -- go (Node l r) = Node <$> go l <*> go r    -- Applicative szintaxis

-- imperatív pszeudokód:
{-
def replaceLeaves(as : [a], t : Tree a): case t of
  Leaf a   -> return Leaf(as.pop());
  Node l r -> return Node (replaceLeaves(as, l), replaceLeaves(as, r));
-- kicsit "veszélyes" implementáció, mert mutálja az inputot

def replaceLeaves(as : [a], t : Tree a):
  var as' = as.copy()
  def go(t : Tree a): case t of
     Leaf a   -> return Leaf(as'.popDefault(a));
     Node l r -> return Node(go(l), go(r));
  return go(t)
-}

-- Mi van, ha 2 vagy több mutable var-t akarunk szimulálni?
-- Azt tudjuk mondani: legyen "s" egy tuple típus.

-- példa: két darab Int változó

modIntPair :: State (Int, Int) ()
modIntPair = do
  modify $ \(x, y) -> (y, x)         -- (x, y) = (y, x)    értékadás (csere)
  modify $ \(x, y) -> (x + 100, y)   -- x = x + 100; y = y


-- Control.Arrow (first, second)
modIntPair' :: State (Int, Int) ()
modIntPair' = do
  modify $ \(x, y) -> (y, x)         -- (x, y) = (y, x)    értékadás (csere)
  modify $ first (+10)
  modify $ second (*200)

  x <- fst <$> get                   --
  y <- snd <$> get
  (x, y) <- get

  if x == 0 then do
    modify $ first (+y)
  else
    pure ()



-- További feladatok
--------------------------------------------------------------------------------

{-

push :: Int -> State [Int] ()
push n = modify (n:)

pop :: State [Int] (Maybe Int)
pop = do
  ns <- get
  case ns of
    []   -> pure Nothing
    n:ns -> do
      put ns
      pure (Just n)


-- Írj egy függvényt, ami kizárólag push és pop felhasználásával map-eli az állapot listát.
mapPP :: (a -> a) -> State [a] ()
mapPP = undefined


-- Írj egy függvényt, ami egy Int listában minden elemet kicserél az addigi elemek maximumára.
-- Tegyük fel, hogy a lista nem-negatív számokat tartalmaz.
-- Használd a (State Int)-et. Legyen az állapot a jelenlegi maximális Int.
maxs :: [Int] -> [Int]
maxs as = undefined

--------------------------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Show)

-- Definiálj egy függvényt, ami kicsérli egy fa leveleiben tárolt értékeket
-- balról jobbra haladva egy megadott lista elemeire.
-- Használj State monádot!

-- pl: replaceLeaves [10, 20, 30] (Node (Leaf 2) (Leaf 3)) == Node (Leaf 10) (Leaf 20)
--     replacereplaceLeaves [5] (Leaf 10) == Leaf 5
--     replacereplaceLeaves [5] (Node (Leaf 0) (Node (Leaf 0) (Leaf 0))) ==
--        (Node (Leaf 5) (Node (Leaf 0) (Leaf 0)))

replaceLeaves :: [a] -> Tree a -> Tree a
replaceLeaves = undefined

-- Ugyanezt jobbról balra is implementáld! Azaz jobbról balra haladj
-- a fában, és úgy illeszd a lista elemeit a fába.
replaceLeaves' :: [a] -> Tree a -> Tree a
replaceLeaves' = undefined


-- Definiáld a függvényt, ami megfordítja a fa leveleiben tárolt értékek sorrendjét!
-- tipp: használd a replaceLeaves függvényt.
reverseElems :: Tree a -> Tree a
reverseElems = undefined


--------------------------------------------------------------------------------

-- Értelmezd a következő utasítások listáját. Minden utasítás
-- egy Int-et módosító művelet. Az "Add n" adjon n-et a jelenlegi
-- állapothoz, a "Subtract n" vonjon ki n-t, és a "Mul" értelemszerűen.
data Op = Add Int | Subtract Int | Mul Int

evalOps :: [Op] -> State Int ()
evalOps = undefined

-- Add meg ennek segítségével az állapotot módosító (Int -> Int) függvényt.
runOps :: [Op] -> Int -> Int
runOps = undefined


-- Írd át a következő függvényeket úgy, hogy *csak* a State
-- konstruktort használd, monád/funktor instance-t és get/put/modify-t
-- ne használj.
--------------------------------------------------------------------------------


modify' :: (s -> s) -> State s ()
modify' f = do
  s <- get
  put (f s)

  -- Művelet végrehajtása lokálisan: az állapot visszaáll a művelet után.
locally :: State s a -> State s a
locally ma = do
  s <- get        -- "elmentjük" az állapotot
  a <- ma         -- futtatjuk ma-t
  put s           -- visszaállítjuk
  pure a

-- Állapot módosítás n-szer
modifyNTimes :: Int -> (s -> s) -> State s ()
modifyNTimes 0 f = pure ()
modifyNTimes n f = modify f >> modifyNTimes (n - 1) f

--------------------------------------------------------------------------------
-}
