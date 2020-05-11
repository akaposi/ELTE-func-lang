{-# language DeriveFunctor #-}

import Control.Monad (ap)

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

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--------------------------------------------------------------------------------


-- 1. feladat

-- Definiálj egy függvényt, ami kicsérli egy fa leveleiben tárolt értékeket
-- balról jobbra haladva egy megadott lista elemeire.
-- Használj State monádot!

-- pl: replaceLeaves [10, 20, 30] (Node (Leaf 2) (Leaf 3)) == Node (Leaf 10) (Leaf 20)
--     replacereplaceLeaves [5] (Leaf 10) == Leaf 5
--     replacereplaceLeaves [5] (Node (Leaf 0) (Node (Leaf 0) (Leaf 0))) ==
--        (Node (Leaf 5) (Node (Leaf 0) (Leaf 0)))

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Show)

replaceLeaves :: [a] -> Tree a -> Tree a
replaceLeaves = undefined


-- Ugyanezt jobbról balra is implementáld! Azaz jobbról balra haladj
-- a fában, és úgy illeszd a lista elemeit a fába.
replaceLeaves' :: [a] -> Tree a -> Tree a
replaceLeaves' = undefined



-- 2. feladat

-- Írd át a következő függvényeket úgy, hogy csak a State
-- konstruktort használd, monád/funktor instance-t és get/put/modify-t
-- ne használj.

-- állapot módosítás
modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put (f s)

-- állapot módosítás n-szer
modifyNTimes :: Int -> (s -> s) -> State s ()
modifyNTimes n f = go n where
  go 0 = pure ()
  go n = modify f >> go (n - 1)

-- Művelet végrehajtása lokálisan: az állapot visszaáll a művelet után.
locally :: State s a -> State s a
locally ma = do
  s <- get
  a <- ma
  put s
  pure a

-- hajtsunk végre egy műveletet n-szer, az összes köztes állapotot és
-- visszatérési értéket adjuk visza listában
iterateState :: Int -> State s a -> State s [(a, s)]
iterateState n ma = go n where
  go 0 = pure []
  go n = do
    a    <- ma
    s    <- get
    rest <- go (n - 1)
    pure ((a, s):rest)

-- monadikus kompozíció (>=>) specializálása State-re
composeState :: (a -> State s b) -> (b -> State s c) -> (a -> State s c)
composeState f g a = f a >>= \b -> g b

-- ap specializálása State-re
apState :: State s (a -> b) -> State s a -> State s b
apState mf ma = do
  f <- mf
  a <- ma
  pure (f a)

joinState :: State s (State s a) -> State s a
joinState mma = do
  ma <- mma
  ma
