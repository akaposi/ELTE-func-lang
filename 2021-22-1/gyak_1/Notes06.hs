{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Monad
import Control.Applicative

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

pop :: State [a] (Maybe a)
pop = do
  as <- get
  case as of
    []   -> return Nothing
    a:as -> do
      put as
      return (Just a)

-- State
--------------------------------------------------------------------------------

-- Írj egy függvényt, ami egy Int listában minden elemet kicserél az addigi
-- elemek maximumára.  Tegyük fel, hogy a lista nem-negatív számokat tartalmaz.
-- Használd a (State Int)-et. Legyen az állapot a jelenlegi maximális Int.
maxs :: [Int] -> [Int]
maxs = undefined

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Show)

-- Definiálj egy függvényt, ami kicseréli egy fa leveleiben tárolt értékeket
-- balról jobbra haladva egy megadott lista elemeire. Használj State monádot!

-- pl: replaceLeaves [10, 20, 30] (   Node (Leaf 2) (Leaf 3))
--                                 == Node (Leaf 10) (Leaf 20)
--     replacereplaceLeaves [5] (Leaf 10) == Leaf 5
--     replacereplaceLeaves [5]
--        (Node (Leaf 0) (Node (Leaf 0) (Leaf 0))) ==
--        (Node (Leaf 5) (Node (Leaf 0) (Leaf 0)))

-- evalState :: State s a -> s -> a
replaceLeaves :: [a] -> Tree a -> Tree a
replaceLeaves as t = evalState (go t) as where
  go :: Tree a -> State [a] (Tree a)
  go (Leaf a)   = do
    ma' <- pop
    case ma' of
      Nothing -> return (Leaf a)
      Just a' -> return (Leaf a')
  go (Node l r) = do
    l' <- go l
    r' <- go r
    return (Node l' r')


-- Definiáld a függvényt, ami megfordítja a fa leveleiben tárolt értékek
-- sorrendjét!  tipp: használhatod a replaceLeaves függvényt.
reverseElems :: Tree a -> Tree a
reverseElems = undefined

-- Cseréld ki az összes levelet a fában a levéltől balra levő levelek
-- összegére.  Használj State-et.
treeSums :: Tree Int -> Tree Int
treeSums = undefined

-- Számozd be jobbról-balra bejárási sorrendben 0-tól egy fa leveleit.
labelRightToLeft :: Tree a -> Tree (Int, a)
labelRightToLeft = undefined

-- Definiálj egy függvényt, ami jobbról balra bejárva map-eli egy fa elemeit
-- egy mellékhatásos függvénnyel.
traverseRightToLeft :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
traverseRightToLeft = undefined

-- Bónusz (nehéz): definiáld az előző függvényt tetszőleges Traversable-re.
traverseRightToLeft' :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverseRightToLeft' = undefined


-- definiáld újra a következő függvényeket "traverse" felhasználásával!
maxs' :: [Int] -> [Int]
maxs' = undefined

replaceLeaves' :: [a] -> Tree a -> Tree a
replaceLeaves' = undefined

reverseElems' :: Tree a -> Tree a
reverseElems' = undefined

treeSums' :: Tree Int -> Tree Int
treeSums' = undefined

-- definiáld újra a következő függvényt "traverseRightToLeft" felhasználásával
labelRightToLeft' :: Tree a -> Tree (Int, a)
labelRightToLeft' = undefined

--------------------------------------------------------------------------------

-- Írd át az összes következő függvényt úgy, hogy a (Monad m) helyett
-- (Applicative m) legyen használva. A függvények viselkedése ne változzon!
-- Tipp: Control.Applicative kombinátorait érdemes megnézni.

f1 :: Monad m => b -> m a -> m b
f1 b ma = do
  a <- ma
  return b

-- f1' :: Applicative m => b -> m a -> m b   -- ugyanígy a többi függvényre

f2 :: Monad m => m Bool -> m a -> m a -> m a
f2 mb ma ma' = do
  b <- mb
  if b then do {a <- ma; ma'; return a}
       else do {ma; ma'}

f3 :: Monad m => (a -> m b) -> [a] -> m [b]
f3 f []     = return []
f3 f (a:as) = do
  b  <- f a
  bs <- f3 f as
  return (b : bs)

f4 :: Monad m => (a -> m Bool) -> [a] -> m [a]
f4 f []     = return []
f4 f (a:as) = do
  b <- f a
  if b then do {as' <- f4 f as; return (a:as')}
       else f4 f as

f5 :: Monad m => m a -> m b -> m c -> m d -> m (a, c)
f5 ma mb mc md = do
  md
  c <- mc
  mb
  a <- ma
  return (a, c)

f6 :: Monad m => (a -> b) -> m a -> m b
f6 f ma = do
  a <- ma
  return (f a)

f7 :: Monad m => m (a -> b) -> m a -> m b
f7 mf ma = do
  f <- mf
  a <- ma
  return (f a)

f8 :: Monad m => m a -> m b -> m b
f8 ma mb = ma >> mb


-- Írd át a következő függvényeket úgy, hogy *csak* a State newtype konstruktort
-- használd, monád/funktor instance-t és get/put/modify-t ne használj.
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
modifyNTimes n f = replicateM_ n (modify f)

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
