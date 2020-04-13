
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

-- Megpróbálunk levenni egy elemet, ha nincs akkor egy default értéket
-- visszaadunk.
popWithDefault :: a -> State [a] a
popWithDefault a = do
  as <- get
  case as of
    []   -> pure a
    a:as -> put as >> pure a

replaceLeaves :: [a] -> Tree a -> Tree a
replaceLeaves as t = evalState (go t) as where
  go (Leaf a)   = Leaf <$> popWithDefault a
  go (Node l r) = do
    l <- go l
    r <- go r
    pure (Node l r)

-- Ugyanezt jobbról balra is implementáld! Azaz jobbról balra haladj
-- a fában, és úgy illeszd a lista elemeit a fába.
replaceLeaves' :: [a] -> Tree a -> Tree a
replaceLeaves' as t = evalState (go t) as where
  go (Leaf a)   = Leaf <$> popWithDefault a
  go (Node l r) = do
    r <- go r
    l <- go l
    pure (Node l r)



-- 2. feladat

-- Írd át a következő függvényeket úgy, hogy csak a State
-- konstruktort használd, monád/funktor instance-t és get/put/modify-t
-- ne használj.

-- állapot módosítás
modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

-- állapot módosítás n-szer
modifyNTimes :: Int -> (s -> s) -> State s ()
modifyNTimes n f = State $ \s -> go n s where
  go 0 s = ((), s)
  go n s = go (n - 1) (f s)

-- Művelet végrehajtása lokálisan: az állapot visszaáll a művelet után.
locally :: State s a -> State s a
locally (State f) = State $ \s -> case f s of (a, s') -> (a, s)

-- hajtsunk végre egy műveletet n-szer, az összes köztes állapotot és
-- visszatérési értéket adjuk visza listában
iterateState :: Int -> State s a -> State s [(a, s)]
iterateState n (State f) = State $ \s -> go n s where
  go 0 s = ([], s)
  go n s = case f s of
    (a, s') -> case go (n - 1) s of
      (rest, s'') -> ((a, s'):rest, s'')

-- monadikus kompozíció (>=>) specializálása State-re
composeState :: (a -> State s b) -> (b -> State s c) -> (a -> State s c)
composeState f g a = State $ \s -> case runState (f a) s of
  (b, s') -> runState (g b) s'

-- ap specializálása State-re
apState :: State s (a -> b) -> State s a -> State s b
apState (State f) (State g) = State $ \s -> case f s of
  (f, s') -> case g s' of
    (a, s'') -> (f a, s'')

joinState :: State s (State s a) -> State s a
joinState (State f) = State $ \s -> case f s of
  (g, s') -> runState g s'
