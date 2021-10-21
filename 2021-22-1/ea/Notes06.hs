{-# language InstanceSigs #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- Applicative generikus példák
-- Foldable/Traversable
-- Lista monád
-- Parser monád
--------------------------------------------------------------------------------

import Control.Monad

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return :: a -> State s a
  return a = State (\s -> (a, s))

  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) (State f) g = State $ \s -> case f s of
    (a, s) -> runState (g a) s

put :: s -> State s ()
put s = State $ \_ -> ((), s)

get :: State s s
get = State $ \s -> (s, s)

evalState :: State s a -> s -> a
evalState sta s = fst (runState sta s)

execState :: State s a -> s -> s
execState sta s = snd (runState sta s)

{-
class Functor f => Applicative f where
  pure  :: a -> f a                   -- ugyanaz mint "return"
                                       -- használjuk pure-t return helyett
                                       -- (csak historikus, hogy külön van a kettő)
  (<*>) :: f (a -> b) -> f a -> f b   -- "ap"
-}

-- Tetszőleges aritású fmap:
-- (a -> b) -> f a -> f b
-- (a -> b -> c) -> f a -> f b -> f c
-- ...

-- Applicative: program futása előtt ismert minden mellékhatás, ami létre jön
-- Monad      : futás közben derül, ki, hogy mik a mellékhatások
--   (Applicative: jobban elemzhető, optimalizálható)

-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f []     = pure []
mapA f (a:as) = (:) <$> f a <*> mapA f as  -- "bináris fmap"

-- mapM f (a:as) = do
--   b  <- f a
--   bs <- mapM f as
--   return (b:bs)

-- minden esetben amikor mapM használható, mapA is (viszont nem fordítva)
--     Applicative super class-ja Monad-nak

-- újradefiniálás:
-- mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM' = mapA


-- filter :: (a -> Bool) -> [a] -> [a]

-- filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- filterM f []     = pure []
-- filterM f (a:as) = do
--   b   <- f a
--   as' <- filterM f as
--   if b then pure (a:as')   -- benn hagyjuk a-t
--        else pure as'       -- kihagyjuk a-t

filterA :: Applicative f => (a -> f Bool) -> [a] -> f [a]
filterA f []     = pure []
filterA f (a:as) =
  (\b as' -> if b then a:as' else as) <$> f a <*> filterA f as

-- zipWithM, zipWithA


-- Foldable, Traversable osztályok
------------------------------------------------------------

-- Foldable f    :  f a -ból tudunk egy [a]-t kapni (sum, product, minimum, maximum)
-- Traversable f :  f a -t Applicative-an tudjuk map-elni a struktúrát

-- standard
-- class Foldable f where
--   foldr   :: (a -> b -> b) -> b -> f a -> b
--   foldMap :: Monoid m => (a -> m) -> f a -> m
  -- vagy foldr-t, vagy foldMap-et kell (minimum definiálni)
  -- egy instance-ban

-- instance Foldable [] where
--   foldr = foldr

-- foldMap foldr-ből
-- foldMap' :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
-- foldMap' f fa = foldr (\a m -> f a <> m) mempty fa

-- extra házi: foldr-t foldMap-ből (mi az a Monoid, aminek a (<>) és mempty
--   metódusával foldr-t tudunk csinálni?)

-- sum     :: (Foldable t, Num a) => t a -> a
-- minimum :: (Foldable t, Ord a) => t a -> a
-- maximum :: (Foldable t, Ord a) => t a -> a
-- length  :: Foldable t => t a -> Int
-- null    :: Foldable t => t a -> Bool      -- üres-e

-- lásd továbbá: import Data.Foldable
--    > :browse Data.Foldable
--    > :bro    Data.Foldable
--  (kilistázza az importált dolgokat)

--------------------------------------------------------------------------------

-- standard:
-- class Functor => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- standard import:
-- (historikus okokból)
-- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)

-- instance-ok
--------------------------------------------------------------------------------

-- Maybe

-- instance Foldable Maybe where
--   -- vagy üres vagy 1 elemű
--   foldr f b Nothing  = b
--   foldr f b (Just a) = f a b          -- foldr f b (a:[]) == f a b

-- sum (Just 10) == 10
-- sum Nothing   == 0
-- toList (Just 10) == [10]
-- length Nothing == 0

-- instance Traversable Maybe where
--   -- üres vagy 1 elemű lista           -- Applicative f, f :: a -> f b
--   traverse f Nothing  = pure Nothing
--   traverse f (Just a) = Just <$> f a
      -- (<$>) : fmap mint infix operátor

-- traverse print (Just 10)   : 10-et nyomtat, végeredmény == Just ()

-- eldobja a végeredményt:
-- traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a ->

--------------------------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f (Leaf a)   = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Foldable Tree where
  -- tipp: foldMap-et írjuk meg
  foldMap f (Leaf a)   = f a
  foldMap f (Node l r) = foldMap f l <> foldMap f r

  -- foldr f b (Leaf a)   = f a b         -- 1-elemű lista
  -- foldr f b (Node l r) = foldr f (foldr f b r) l

instance Traversable Tree where
  traverse f (Leaf a)   = Leaf <$> f a
  traverse f (Node l r) = Node <$> traverse f l <*> traverse f r

-- Traversable definíció:
--   vesszük fmap definíciót
--   minden konstruktor alkalmazás, legyen N-áris Applicative map
--   (traverse = fmap + Applicative mellékhatás)

--------------------------------------------------------------------------------

{-
label :: Tree a -> Tree (a, Int)
label t = evalState (traverse go t) 0 where
  -- traverse _ t :: State Int (Tree (a, Int))
  --          _   :: a -> State Int (a, Int)
  go :: a -> State Int (a, Int)
  go a = do
    n <- get
    put (n + 1)
    pure (a, n)
-}

--------------------------------------------------------------------------------

-- {-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a) (Tree2 a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

--   "ingyen" kapunk egy csomó függvényt innen:
--   (felhasználható vizsgán, gyak feladatban)
-- Data.Foldable
-- Data.Traversable
-- Control.Applicative
-- Control.Monad

-- label általánosítása tetszőleges Traversable-re
label :: Traversable t => t a -> t (a, Int)
label t = evalState (traverse go t) 0 where
  go :: a -> State Int (a, Int)
  go a = do
    n <- get
    put (n + 1)
    pure (a, n)


-- lista monád
--------------------------------------------------------------------------------

-- instance Functor []

-- mi a következő?


-- (standard)
-- instance Applicative []

-- instance Monad [] where
--   return :: a -> [a]
--   return a = [a]
--
--   (>>=) :: [a] -> (a -> [b]) -> [b]
--   (>>=) as f = concatMap f as
--     map f as :: [[b]]
--     concat (map f as) :: [b]
--     concatMap ( flatMap )

-- használat:
l1 :: [(Int, Int)]
l1 = do
  x <- [0..10]
  y <- [0..x]
  pure (x, y)

-- korábbi listakifejezésssel:
l1' :: [(Int, Int)]
l1' = [(x, y) | x <- [0..10], y <- [0..x]]

-- listakifejezés: szintaktikus cukor lista Monad instance-ra

l2 :: [Int]      -- l2 = [x + 20 | x <- [0..10]]
l2 = do
  x <- [0..10]
  pure (x + 20)

-- Monad instance definíciója szerint kifejtve:
-- l2 = concatMap (\x -> [x + 20]) [0..10]
-- l2 = fmap (\x -> x + 20) [0..10]

-- Monad + Applicative törvényekből
-- do x <- mx         ==   fmap (+20) mx
--    pure (x + 20)

-- Mik az Applicative törvények?
--   lásd : Control.Applicative dokumentáció

-- listakifejezések: lehet bennük let, szűrőkifejezés is
-- let

l3 :: [Int]
l3 = [ x + y | x <- [0..10], let y = x*x]

-- l3 :: [Int]
-- l3 = do
--   x <- [0..10]
--   let y = x*x
--   pure (x + y)

-- szűrés: szükség van a következő függvényre:

-- standard: guard, Control.Monad-ból jön
guard' :: Bool -> [()]
guard' b = if b then [()] else []

l4 :: [Int]
l4 = [x | x <- [0..10], even x]

l4' :: [Int]
l4' = do
  x <- [0..10]
  guard' $ even x          -- hogyan szűr?
  pure x

l4'' :: [Int]
l4'' = do
  x <- [0..10]
  if even x then pure ()    -- concatMap f [a] == f a    vagy: pure a >>= f == f a
            else []         -- concatMap f []  == []
  pure x

  -- lista monádban: üres listát bind-olunk: üres listát kapunk
l5 :: [Int]
l5 = do
  x <- []
  pure $ x * x    -- l5 == []

  -- 1-elemű listát bind-olunk: ugyanaz, mint egy függvényhívás (bind elhagyható)

  -- guard: Bool feltételtől függően választ a 0/1 elemű lista között
  --        (hatása: szűrés)

-- list Applicative:
--    fmap                   : sima map függvény
--    f <$> xs <*> ys        : összes lehetséges kombinációt bejárja
--    f <$> xs <*> ys <*> zs : összes lehetséges kombinációt bejárja

-- (+) <$> [0, 1, 2] <*> [4, 5, 6] == [4,5,6,5,6,7,6,7,8]

-- lehet-e más Applicative [] instance?
--    (zip-es instance:)
--      (+) <$> [0, 1, 2] <*> [4, 5, 6] == zipWith (+) [0, 1, 2] [4, 5, 6]
--     (szintén legális, viszont erre már nincs Monad instance)

-- külön veszünk egy newtype wrap-et listákon, arra megadjuk a zip-es instance-ot
-- newtype ZipList a = ZipList [a]
-- instance Applicative ZipList where ...
