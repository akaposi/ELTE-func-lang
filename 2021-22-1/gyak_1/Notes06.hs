{-# language InstanceSigs, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Monad
import Control.Applicative
import Data.Foldable  -- toList

-- köv feladat:
------------------------------------------------------------

-- State feladat (State + lista vagy fa)
--  (kb maxs bonyolultságú)

-- canvas feladat
------------------------------------------------------------

data Tree3 a = Leaf3 a | Node3 (Tree3 a) (Tree3 a) (Tree3 a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

canvas :: Tree3 (Maybe a) -> Maybe (Tree3 a)
canvas (Leaf3 ma)       = Leaf3 <$> ma    -- (<$>) = fmap
canvas (Node3 t1 t2 t3) = Node3 <$> canvas t1 <*> canvas t2 <*> canvas t3

-- canvas (Node3 t1 t2 t3) =
--   t1' <- canvas t1
--   t2' <- canvas t2
--   t3' <- canvas t3
--   pure (Node3 t1' t2' t3')

-- Applicative map-elés:
--   fmap általánosítása N-darab argumentumra
--    1:   f <$> ma
--    2:   f <$> ma <*> mb
--    3:   f <$> ma <*> mb <*> mc
--    ...

-- Applicative instance teszi lehetővé az N-es fmap-elést
--    (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- class Traversable t where
--    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- traverse általánosítja köv függvényt:
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
--   - nem csak listára, hanem bármilyen Traversable t-re
--   - mellékhatás Applicative, nem kell, hogy Monad legyen
--     (minden Monad Applicative, de nem fordítva)

canvas' :: Tree3 (Maybe a) -> Maybe (Tree3 a)
canvas' t = traverse id t

-- std:
-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
-- sequenceA :: Tree3 (Maybe a) -> Maybe (Tree3 a)
-- sequenceA = traverse id

-- (backwards kompatibilis Monad-os verzió:)
-- sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)

canvas'' :: Tree3 (Maybe a) -> Maybe (Tree3 a)
canvas'' = sequenceA

io :: IO ()
io = do
  sequenceA [putStrLn "foo", putStrLn "bar"]
  sequenceA (Node3 (Leaf3 (putStrLn "foo"))
                   (Leaf3 (putStrLn "foo"))
                   (Leaf3 (putStrLn "foo")))
  pure ()

-- házi feladat: megnézni, hogy mit csinál ez a függvény:
--   (sequence speciális esete)
-- sequenceA :: Tree [a] -> [Tree a]

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
maxs ns = evalState (go ns) 0 where
  go :: [Int] -> State Int [Int]
  go []     = pure []
  go (n:ns) = do
    m <- get
    put (max m n)
    ns' <- go ns
    pure (m:ns')

-- traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
maxs'' :: [Int] -> [Int]
maxs'' ns = evalState (traverse go ns) 0 where
  go :: Int -> State Int Int
  go n = do
    m <- get
    put (max m n)
    pure m

-- maxs''' :: Traversable t => t Int -> t Int
maxs''' ns = evalState (traverse go ns) 0 where
  go :: Int -> State Int Int
  go n = do
    m <- get
    put (max m n)
    pure m

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Functor, Foldable, Traversable)

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
reverseElems t = replaceLeaves (reverse elems) t where
  elems = foldr (:) [] t

  -- Data.Foldable.toList : Foldable t => t a -> [a]
  -- toList t = foldr (:) [] t

  -- treeToList :: Tree a -> [a]
  -- treeToList = (rekurzív def)

-- Cseréld ki az összes levelet a fában a levéltől balra levő levelek
-- összegére.  Használj State-et.
treeSums :: Tree Int -> Tree Int
treeSums = maxs'''

-- Definiálj egy függvényt, ami jobbról balra bejárva map-eli egy fa elemeit
-- egy mellékhatásos függvénnyel.
traverseRightToLeft :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
traverseRightToLeft = go where
  go :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  go f (Leaf a)   = Leaf <$> f a
  go f (Node l r) = (\l r -> Node r l) <$> go f r <*> go f l
    -- do
    -- l' <- go f l
    -- t' <- go r r
    -- pure (Node l' r)

-- Számozd be jobbról-balra bejárási sorrendben 0-tól egy fa leveleit.
labelRightToLeft :: Tree a -> Tree (Int, a)
labelRightToLeft t = evalState (traverseRightToLeft go t) 0 where
  go :: a -> State Int (Int, a)
  go a = do
    n <- get
    put (n + 1)
    pure (n, a)

-- Bónusz (nehéz): definiáld az előző függvényt tetszőleges Traversable-re.
traverseRightToLeft' ::
  (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverseRightToLeft' = undefined

  -- könnyű : két bejárással
  -- nehéz  : egy bejárással

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
