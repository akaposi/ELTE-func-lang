{-# language DeriveFunctor, InstanceSigs #-}


import Control.Monad

-- State monád
--------------------------------------------------------------------------------

-- (newtype: ugyanaz, mint a data, de csak 1 konstruktor + 1 mező lehetséges
--           nincs futásidejű költsége)

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

   -- bemenő állapot ->  (visszatérési érték, kimenő állapot)
   --     s          ->    (a, s)

instance Applicative (State s) where
  pure a = State (\s -> (a, s))
  (<*>) = ap

instance Monad (State s) where
  -- return :: a -> State s a
  -- return :: a -> s -> (a, s)         -- nincs hatás: nem változik s
  return = pure

  -- (>>=) :: State s a -> (a -> State s b) -> State s b
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


-- State monád használata
------------------------------------------------------------

-- t :: State s a      Monad (State s)
--                     - 1 darab mutábilis referencia, "s" típusa
--                     - "a" visszatérési érték típusa
--                     - mellékhatás: referencia írás/olvasás

-- put    : egy konkrét értéket írunk a ref-be
-- get    : olvassa a ref jelenlegi értékét
-- modify : egy függvényt alkalmaz a ref jelenlegi értékén

--         futattni a definiált műveleteket
--             művelet       kezdő érték   (visszatérési érték, végső ref érték)
-- runState  :: State s a   ->  s           -> (a, s)
-- evalState :: State s a   ->  s           -> a
-- execState :: State s a   ->  s           -> s

-- put :: s -> State s ()           -- beír egy értéket az állapotba
-- get :: State s s                 -- értékként visszaadja az állapotot
-- modify :: (s -> s) -> State s () -- függvényt alkalmaz az állapotra

p1 :: State Int Int
p1 = do
  put 10
  put 20         -- felülírom az értéket
  modify (+100)  -- függvényt alkalmazok
  n <- get
  return n

-- runState p1 0 == (120, 120)

p2 :: State Int ()
p2 = do
  modify (*10)
  return ()

-- runState p2  1 == ((), 10)
-- evalState p2 1 == ()
-- execState p2 1 == 10

-- Standard függvények State-el implementálva:
------------------------------------------------------------

-- sum :: [Int] -> Int
sumState :: [Int] -> Int
sumState ns = execState (go ns) 0 where
  go []     = return ()
  go (n:ns) = modify (+n) >> go ns

sumState' :: [Int] -> Int
sumState' ns = execState (mapM_ (\n -> modify (+n)) ns) 0

-- reverse :: [a] -> [a]
reverseState :: [a] -> [a]
reverseState as = execState (go as) [] where
  go [] = return ()
  go (a:as) = modify (a:) >> go as

reverseState' :: [a] -> [a]
reverseState' as = execState (mapM_ (\a -> modify (a:)) as) []


------------------------------------------------------------


-- Definiálj egy függvényt, ami a lista állapotot kiegészíti egy elemmel
push :: a -> State [a] ()
push = undefined


-- példák:
-- runState (push 10) [] == ((), [10])
-- runState (push 10 >> push 10 >> push 20) [] == ((), [20, 10, 10])

-- Ha az állapot lista nem üres, akkor a következő függvény leveszi az első
-- elemet és visszaadja Just értékként, egyébként Nothing-ot ad.
pop :: State [a] (Maybe a)
pop = undefined

-- példák:
-- runState pop []        == (Nothing, [])
-- runState pop [0, 1, 2] == (Just 0, [1, 2])


-- Írj egy függvényt, ami egy Int listában minden elemet kicserél az elemtől
-- balra levő elemek maximumára (beleértve az elemet). Legyen az állapot
-- Nothing, ha még egy értéket sem jártunk be, egyébként pedig Just-ban tárolva
-- az eddigi értékek maximuma.

maxs :: [Int] -> State (Maybe Int) [Int]
maxs = undefined

-- példák:
-- evalState (maxs [1, 2, 5, 2]) Nothing == [1, 2, 5, 5]
-- evalState (maxs [10, 5, 12, 3] Nothing) == [10, 10, 12, 12]

-- pop  :: State [a] (Maybe a)
-- push :: a -> State [a] ()

-- Írj egy függvényt, ami kizárólag push, pop és rekurzió felhasználásával
-- map-eli az állapot listát.
mapPushPop :: (a -> a) -> State [a] ()
mapPushPop = undefined

-- példák:
-- execState (mapPushPop (+10)) [0, 1, 2] == [10, 11, 12]


-- Definiálj egy függvényt, ami kicseréli egy fa leveleiben tárolt értékeket
-- balról jobbra haladva egy megadott lista elemeire. Használj State monádot!

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Show)

-- pl: replaceLeaves [10, 20, 30] (   Node (Leaf 2) (Leaf 3))
--                                 == Node (Leaf 10) (Leaf 20)
--     replacereplaceLeaves [5] (Leaf 10) == Leaf 5
--     replacereplaceLeaves [5]
--        (Node (Leaf 0) (Node (Leaf 0) (Leaf 0))) ==
--        (Node (Leaf 5) (Node (Leaf 0) (Leaf 0)))

replaceLeaves :: [a] -> Tree a -> Tree a
replaceLeaves = undefined

-- Definiáld a függvényt, ami megfordítja a fa leveleiben tárolt értékek
-- sorrendjét!  tipp: használhatod a replaceLeaves függvényt.
reverseElems :: Tree a -> Tree a
reverseElems = undefined


-- Írd át a következő függvényeket úgy, hogy *csak* a
-- (State :: (s -> (a, s)) -> State s a) konstruktort használd,
-- monád/funktor instance-t és get/put/modify-t ne használj.
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

-- Értelmezd a következő utasítások listáját. Minden utasítás
-- egy Int-et módosító művelet. Az "Add n" adjon n-et a jelenlegi
-- állapothoz, a "Subtract n" vonjon ki n-t, és a "Mul" értelemszerűen.
data Op = Add Int | Subtract Int | Mul Int

evalOps :: [Op] -> State Int ()
evalOps = undefined

-- Add meg ennek segítségével az állapotot módosító (Int -> Int) függvényt.
runOps :: [Op] -> Int -> Int
runOps = undefined
