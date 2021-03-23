
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Control.Monad
import Data.Bits (xor, shiftR)
import Data.Word

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

-- 1. összegezz egy listát State használatával!
sum1 :: [Int] -> Int
sum1 ns = execState (go ns) 0 where
  go :: [Int] -> State Int ()
  go []     = pure ()
  go (n:ns) = modify (+n) >> go ns

sum2 :: [Int] -> Int
sum2 ns = execState (traverse (\n -> modify (+n)) ns) 0


-- 2. fordíts meg egy listát State használatával!
rev1 :: [a] -> [a]
rev1 as = execState (go as) [] where
  go :: [a] -> State [a] ()
  go []     = pure ()
  go (a:as) = modify (a:) >> go as

rev2 :: [a] -> [a]
rev2 as = execState (traverse (\a -> modify (a:)) as) []


-- 3. Cseréld ki az összes elemet egy listában a korábbi elemek összegére.
-- Használj State-et.
sums1 :: [Int] -> [Int]
sums1 ns = evalState (go ns) 0 where
  go :: [Int] -> State Int [Int]
  go []     = pure []
  go (n:ns) = do
    s <- get
    put (n + s)
    ns' <- go ns
    pure (s:ns')

sums2 :: [Int] -> [Int]
sums2 ns = evalState (traverse go ns) 0 where
  go :: Int -> State Int Int
  go n = do
    s <- get
    put (s + n)
    pure s


--------------------------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Functor, Foldable, Traversable)

-- 4. Cseréld ki az összes levelet a fában a levéltől balra levő levelek összegére.
-- Használj State-et.
treeSums1 :: Tree Int -> Tree Int
treeSums1 t = evalState (go t) 0 where
  go :: Tree Int -> State Int (Tree Int)
  go (Leaf n) = do
    s <- get
    put (n + s)
    pure $ Leaf s
  go (Node l r) = do
    l' <- go l
    r' <- go r
    pure $ Node l' r'

treeSums2 :: Tree Int -> Tree Int
treeSums2 t = evalState (go t) 0 where
  go :: Tree Int -> State Int (Tree Int)
  go (Leaf n) = do
    s <- get
    put (n + s)
    pure $ Leaf s
  go (Node l r) =
    Node <$> go l <*> go r

treeSums3 :: Tree Int -> Tree Int
treeSums3 t = evalState (traverse go t) 0 where
  go :: Int -> State Int Int
  go n = do {s <- get; put (s + n); pure s}


-- 5. Cseréld ki az összes elemet egy tetszőleges Traversable struktúrában a korábbi elemek
--    összegére.
genericSums :: (Traversable t, Num a) => t a -> t a
genericSums ta = evalState (traverse go ta) 0 where
  go :: Num a => a -> State a a
  go a = do {s <- get; put (s + a); pure s}


-- 6. Számozd be jobbról-balra bejárási sorrendbe 0-tól egy fa leveleit.
--    Tipp: a traverse függvény most nem használható, mivel az balról-jobbra működik.

label1 :: Tree a -> Tree (Int, a)
label1 t = evalState (go t) 0 where

  go :: Tree a -> State Int (Tree (Int, a))
  go (Leaf a) = do
    n <- get
    put (n + 1)
    pure $ Leaf (n, a)
  go (Node l r) = do
    r' <- go r
    l' <- go l
    pure $ Node l' r'

label2 :: Tree a -> Tree (Int, a)
label2 t = evalState (go t) 0 where

  go :: Tree a -> State Int (Tree (Int, a))
  go (Leaf a) = do
    n <- get
    put (n + 1)
    pure $ Leaf (n, a)
  go (Node l r) =
    -- trükk: először az "r"-t járjuk be, aztán az "l"-t, viszont
    -- a végeredményeket fordítva tesszük a Node ba (lásd: "flip"),
    -- tehát az alábbi definíció ugyanaz, mint a label1-ben
    flip Node <$> go r <*> go l
