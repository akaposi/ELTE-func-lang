{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}

module Gy06 where

import Control.Monad

-- Előző óráról maradtak
------------------------------------------

-- join
f3 :: Monad m => m (m a) -> m a
f3 mma = do
  ma <- mma
  a <- ma
  return a

f5 :: Monad m => (a -> m b) -> m a -> m b
f5 f ma = do
  a <- ma
  f a

-- fx :: Monad m => (a -> b) -> m a -> mb
f6 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
f6 = liftM2

f7 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
f7 f ma mb mc = do
  a <- ma
  b <- mb
  c <- mc
  return (f a b c)

f8 :: Monad m => (a -> b -> c -> d -> e -> f) -> m a -> m b -> m c -> m d -> m e -> m f
f8 = liftM5

f9 :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f9 f g a = do
  b <- f a
  g b



-- State monád definíció
--------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

-- Either a b = Left a | Right b
instance Applicative (State s) where -- Következő óra, töltelék kód
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
modify f = do
  n <- get
  put (f n)

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--------------------------------------------------------------------------------

-- típus (2 paraméteres):   State :: * -> * -> *
--  State s a  :   mellékhatás: egy darab "s" típus írható/olvasható referencia
--     instance Monad (State s)
--     "a": visszatérési érték típusa (mint minden monádban)

-- futtatás:
--------------------------------------------------------------------------------

--                       kezdő állapot      (érték, végső állapotot)
-- runState :: State s a ->      s          -> (a, s)

-- (kényelmi függvények)
-- evalState :: State s a ->      s          -> a
-- execState :: State s a ->      s          -> s


--------------------------------------------------------------------------------

fy :: State Int ()
fy = do
  put (10) -- Referencia értéke = 10
  n <- get -- n értéke = 10
  put (20)

triple :: State Int ()
triple = modify (*3)
-- triple = do
--   n <- get
--   put (n * 3)

addTwo :: State Int ()
addTwo = modify (+2)
-- addTwo = do
--   n <- get
--   put (n + 2)

-- TODO: Auxiliary function!

timesNinePlusSix :: State Int ()
timesNinePlusSix = do
  triple
  addTwo
  triple

-- Should be ((5 * 3) + 2) * 3 = 5 * 9 + 6 = 51
test :: Int
test = execState timesNinePlusSix 5

--


{- Stack -}
-- FILO

-- Add an element to the top of the stack by extending the state list!
push :: a -> State [a] ()
push a = modify (a:)
-- push a = do
--   as <- get
--   put (a:as)

-- runExample2 :: (a,s)
-- runExample2 = do
--   push 10
--   push 20


-- Examples:
-- runState (push 10) [] == ((), [10])
-- runState (push 10 >> push 10 >> push 20) [] == ((), [20, 10, 10])

-- Define a function that checks if a stack is empty!
isEmpty :: State [a] Bool
isEmpty = fmap null get
-- isEmpty = do
--   as <- get
--   -- if null as then return True else return False
--   return (null as)

-- Return the top element of the stack if it is not empty.
top :: State [a] (Maybe a)
top = do
  as <- get
  case as of
    [] -> return Nothing
    a : as' -> return (Just a)


-- Remove and return the top element of the stack if it is not empty.
pop :: State [a] (Maybe a)
pop = do
  as <- get
  case as of
    [] -> do return Nothing
    a : as' -> do
      put as'
      return (Just a)




-- runState stackTest [] == ('d', "ba")
stackTest :: State [Char] (Maybe Char)
stackTest = do
  push 'a'
  push 'b'
  push 'c'
  pop
  push 'd'
  pop


-- [1,4,3,2,3,15,4,6,...] - [1,4,4,4,4,15,15,15,15,15]
-- Írj egy függvényt, ami egy Int listában minden elemet kicserél az addigi
-- elemek maximumára.  Tegyük fel, hogy a lista nem-negatív számokat tartalmaz.
-- Használd a (State Int)-et. Legyen az állapot a jelenlegi maximális Int.
maxs :: [Int] -> [Int]
maxs ns = evalState (go ns) 0 where
  go :: [Int] -> State Int [Int]
  go [] = return []
  -- go (a:as) = do
  --   m <- get
  --   when m > a
  --     then do
  --       as' <- go as
  --       return (m : as')
  --     else do
  --       put a
  --       as' <- go as
  --       return (a : as')
  go (a:as) = do
    m <- get
    when (m < a) (put a)
    a' <- get
    as' <- go as
    return (a' : as')


    -- let rM = max a m
    -- put rM
    -- as' <- go as
    -- return (rM : as')



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


-- Írd át a következő függvényeket úgy, hogy *csak* a (State :: (s -> (a, s)) ->
-- State s a) konstruktort használd, monád/funktor instance-t és
-- get/put/modify-t ne használj.
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
-- modifyNTimes :: Int -> (s -> s) -> State s ()
-- modifyNTimes 0 f = pure ()
-- modifyNTimes n f = modify f >> modifyNTimes (n - 1) f


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
