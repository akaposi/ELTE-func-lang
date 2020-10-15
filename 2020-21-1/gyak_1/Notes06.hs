
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

import Control.Monad

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


-- Foldable/Traversable példa
--------------------------------------------------------------------------------

-- sum, length, elem

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Show)

instance Foldable Tree where
  foldr f b (Leaf a)   = f a b                   -- foldr (:) [] (Leaf a) == (a : [])
  foldr f b (Node l r) = foldr f (foldr f b r) l
    -- foldr f b r  -- jobb fa foldr eredménye :: b

    -- foldr (:) [] (Leaf a) = (:) a [] = a:[] = [a]
    -- foldr (:) [] (Node (Leaf x) (Leaf y)) =
    --      foldr (:) (foldr (:) [] (Leaf y)) (Leaf x)
    --      foldr (:) [y] (Leaf x)
    --      foldr (:) [y] (Leaf x)
    --      (:) x [y]
    --      [x, y]

    -- toList :: Tree a -> [a]
    -- toList = foldr (:) []

    -- naiv (legrosszab idő: kvadratikus)
    -- toList (Leaf a)   = [a]
    -- toList (Node l r) = toList l ++ toList r

    -- (((xs ++ ys) ++ zs) ++ ...)   -- kvadratikus (bal lista ismételt lemásolása)
    -- xs ++ (ys ++ (zs ++ ...))     -- lineáris

    -- []     ++ ys = ys
    -- (x:xs) ++ ys = x : (ys ++ ys)

-- foldr f b []     = b
-- foldr f b (a:as) = f a (foldr f b as)

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f b [x, y, z] == f x (f y (f z b))
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- fában: az összes "a" típusú értéket egy függvénnyel kombinálni

-- Mit tudunk kezdeni Foldable Tree instance-al?
-- nézzük meg Data.Foldable modult
-- (sum, length, maximum, minimum, elem, stb.)

-- derive Foldable
-- {-# language DeriveFoldable #-}
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)
  deriving (Show, Functor, Foldable, Traversable)

--------------------------------------------------------------------------------

-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
-- traverse : mapA túlterhelése

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  -- alkalmazzuk f-et minden levélre, adjuk az eredményeket Tree-ben
  traverse f (Leaf a)   = Leaf <$> f a
  traverse f (Node l r) = Node <$> traverse f l <*> traverse f r

-- {-# language DeriveTraversable #-}

-- Számozzuk be balról jobbra egy fa leveleit!
label :: Tree a -> Tree (a, Int)
label t = evalState (traverse go t) 0 where            -- State-ben traverse: visszaad egy (State s (Tree ...))

  -- mellékhatásos függvény, amit minden Leaf-beli "a"-ra végrehajtok.
  go :: a -> State Int (a, Int)
  go a = do
    n <- get
    put (n + 1)
    pure (a, n)

data RTree a = RNode a [RTree a] deriving (Show, Functor, Foldable)
-- házi feladat: instance Foldable RTree

instance Traversable RTree where
  traverse f (RNode a ts) = RNode <$> f a <*> traverse (traverse f) ts
  -- fmap f (RNode a ts) = RNode (f a) (fmap (fmap f) ts)


-- State feladatok
--------------------------------------------------------------------------------

pop :: State [a] (Maybe a)
pop = do
  as <- get
  case as of
    []   -> pure Nothing
    a:as -> do
      put as
      pure (Just a)

-- Definiálj egy függvényt, ami kicsérli egy fa leveleiben tárolt értékeket
-- balról jobbra haladva egy megadott lista elemeire.
-- Használj State monádot!

-- pl: replaceLeaves [10, 20, 30] (Node (Leaf 2) (Leaf 3)) == Node (Leaf 10) (Leaf 20)
--     replacereplaceLeaves [5] (Leaf 10) == Leaf 5
--     replacereplaceLeaves [5] (Node (Leaf 0) (Node (Leaf 0) (Leaf 0))) ==
--        (Node (Leaf 5) (Node (Leaf 0) (Leaf 0)))

replaceLeaves :: [a] -> Tree a -> Tree a
replaceLeaves as t = evalState (go t) as where     -- go t megad egy State műveletet, evalState futtatja, "as" kezdő állapot
  go :: Tree a -> State [a] (Tree a)
  go (Leaf a) = do
    ma <- pop
    case ma of
      Nothing -> pure (Leaf a)   -- már elfogyott a lista
      Just a' -> pure (Leaf a')  -- kicseréljük a'-ra az értéket
  go (Node l r) = Node <$> go l <*> go r
    -- vagy:
    -- do l' <- go l
    --    r' <- go r'
    --    pure (Node l' r')

-- (segédfüggvény nélkül nem lehet)
-- replaceLeaves :: [a] -> Tree a -> Tree a
-- replaceLeaves (x:xs) (Leaf _)   = Leaf x
-- replaceLeaves []     (Leaf x)   = Leaf x
-- replaceLeaves xs     (Node l r) =
--   replaceLeaves xs l -- ad l'

-- state használata, de State monád nélkül
replaceLeaves'' :: [a] -> Tree a -> Tree a
replaceLeaves'' as t = fst (go as t) where

  go (x:xs) (Leaf _)   = (Leaf x, xs)
  go []     (Leaf x)   = (Leaf x, [])
  go xs     (Node l r) = case go xs l of
    (l', xs') -> case go xs' r of
      (r', xs'') -> (Node l' r', xs'')

replaceLeavesT :: [a] -> Tree a -> Tree a
replaceLeavesT as t = evalState (traverse go t) as where
  go :: a -> State [a] a
  go a = do
    ma <- pop
    case ma of
      Nothing -> pure a
      Just a' -> pure a'

-- függvény: megtükrözi a fát, és közben IO-ban kinyomtatja a "a" értéket
--    (nem írható traverse-el, mert nem csak az "a" értékeket dolgozzuk fel)

foo :: Show a => Tree a -> IO (Tree a)
foo (Leaf a) = do
  print a
  pure (Leaf a)
foo (Node l r) = do
  l' <- foo l
  r' <- foo r
  pure (Node r' l') -- csere miatt nem traverse!

-- Ugyanezt jobbról balra is implementáld! Azaz jobbról balra haladj
-- a fában, és úgy illeszd a lista elemeit a fába.
replaceLeaves' :: [a] -> Tree a -> Tree a
replaceLeaves' = undefined


-- Definiáld a függvényt, ami megfordítja a fa leveleiben tárolt értékek sorrendjét!
-- tipp: használd a replaceLeaves függvényt.
reverseElems :: Tree a -> Tree a
reverseElems = undefined


--------------------------------------------------------------------------------


-- Írj egy függvényt, ami egy Int listában minden elemet kicserél az addigi elemek maximumára.
-- Tegyük fel, hogy a lista nem-negatív számokat tartalmaz.
-- Használd a (State Int)-et. Legyen az állapot a jelenlegi maximális Int.
maxs :: [Int] -> [Int]
maxs as = undefined


-- Írj egy függvényt, ami kizárólag push és pop felhasználásával map-eli az állapot listát.
mapPP :: (a -> a) -> State [a] ()
mapPP = undefined


-- Értelmezd a következő utasítások listáját. Minden utasítás
-- egy Int-et módosító művelet. Az "Add n" adjon n-et a jelenlegi
-- állapothoz, a "Subtract n" vonjon ki n-t, és a "Mul" értelemszerűen.
data Op = Add Int | Subtract Int | Mul Int

evalOps :: [Op] -> State Int ()
evalOps = undefined

-- Add meg ennek segítségével az állapotot módosító (Int -> Int) függvényt.
runOps :: [Op] -> Int -> Int
runOps = undefined


--------------------------------------------------------------------------------
-- BEAD: State, viszont lesz rekurzív definíció/adattípus
--------------------------------------------------------------------------------
