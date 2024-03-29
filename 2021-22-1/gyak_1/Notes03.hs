
{-# language InstanceSigs #-}

-- pragma: ghc options a fájl tetején
-- {-# language <nyelvi opció 1> <nyelvi opció 2> ... #-}
-- InstanceSig

-- Ajánlott option: figyelmeztet, ha nem fedjük le az összes esetet
-- mintaillesztésnél
{-# options_ghc -Wincomplete-patterns #-}

-- Következő gyak:
--   - Felvételt csinálok
--   - Monad (lásd előadás)
--   - Óra elei feladat témája: egyszerűbb data-ra Functor instance

------------------------------------------------------------

import Prelude hiding (Either(..), Functor(..), Semigroup(..), Monoid(..))

-- óra eleji feladat megoldása:

maybeSum :: [Maybe Int] -> Int
maybeSum []            = 0
maybeSum (Nothing:mns) = maybeSum mns
maybeSum (Just n:mns)  = n + maybeSum mns

maybeSum2 :: [Maybe Int] -> Int
maybeSum2 []       = 0
maybeSum2 (mn:mns) = case mn of
  Nothing -> maybeSum2 mns
  Just n  -> maybeSum2 mns + n

-- maybeSum2 (mn:mns) =
--   (case mn of Nothing -> 0; Just n -> n) + maybeSum2 mns

-- std függvény: maybe
-- maybeSum2 (mn:mns) = maybe 0 id mn + maybeSum2 mns


--------------------------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)

class Eq' a where
  eq :: a -> a -> Bool

class Eq' a => Ord' a where
  lte :: a -> a -> Bool      -- (<=)

class Show' a where
  show' :: a -> String       --

instance Eq' a => Eq' (Tree a) where
  eq :: Tree a -> Tree a -> Bool
  eq (Leaf a)   (Leaf a')    = eq a a'     -- Eq' a constraint-ből
  eq (Node l r) (Node l' r') = eq l l' && eq r r'
  eq _          _            = False

-- "lexikografikus": Leaf _ <= Node _ _
instance Ord' a => Ord' (Tree a) where
  lte (Leaf _) (Node _ _)     = True
  lte (Leaf a) (Leaf a')      = lte a a'
  lte (Node l r) (Node l' r') = lte l l' && lte r r'
  lte _          _            = False


-- -- emlékezzünk: (++) lista összefűzés, String összefűzés
-- instance Show' a => Show' (Tree a) where
--   show' (Leaf a) = "Leaf " ++ show' a
--   -- ...

-- Eq, Ord, Show


-- Semigroup, Monoid, Functor
------------------------------------------------------------

infixr 6 <>
class Semigroup a where
  (<>) :: a -> a -> a   -- asszociatív bináris művelet
                        -- x <> (y <> z) = (x <> y) <> z

class Semigroup a => Monoid a where
  mempty :: a           -- Semigroup + egységelem
                        -- mempty <> x = x
                        -- x <> mempty = x

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- Feladat: írd meg a következő instance-okat! (+feleljen meg az osztály törvényeknek!)
-- Tipp: ugorjuk át a nehezebb feladatokat.

instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (<>) :: (a, b) -> (a, b) -> (a, b)
  (a, b) <> (a', b') = (a <> a', b <> b')

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty :: (a, b)
  mempty = (mempty, mempty)
  -- (x, y) <> (mempty, mempty) = (x <> mempty, y <> mempty) = (x, y)  OK

-- tipp: használjuk a _-t arra, hogy megkapjuk a metódusok típusát
instance Semigroup b => Semigroup (a -> b) where

  -- használjuk Semigroup b instance-t
  (<>) :: (a -> b) -> (a -> b) -> a -> b
  (<>) f g a = f a <> g a

instance Monoid b => Monoid (a -> b) where
  mempty :: a -> b
  mempty a = mempty

--------------------------------------------------------------------------------

-- map :: (a -> b) -> [a] -> [b]

-- általánosítás:
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- instance Functor [] where
--    fmap :: (a -> b) -> [] a -> [] b
--    fmap = map

--    fmap :: (a -> b) -> [a] -> [b]    -- szintaktikus cukorka lista típusra!
--    fmap = map

-- min 1 paraméter
--  utolsú típusparaméter fölött map-elünk (Functor instance-al)

data    Foo1 a      = Foo1 Int a a a deriving Show
data    Foo2 a      = Foo2 Bool a Bool
data    Foo3 a      = Foo3 a a a a a
data    Tree1 a     = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show
data    Tree2 a     = Node2 a [Tree2 a] deriving Show
data    Pair a b    = Pair a b
data    Either a b  = Left a | Right b
data    Tree3 i a   = Leaf3 a | Node3 (i -> Tree3 i a)  -- i-szeres elágazás
newtype Id a        = Id a
newtype Const a b   = Const a
newtype Fun a b     = Fun (a -> b)

instance Functor Foo1 where
  fmap :: (a -> b) -> Foo1 a -> Foo1 b
  fmap f (Foo1 n x y z) = Foo1 n (f x) (f y) (f z)

-- fmap (++"foo") $ Foo1 10 "a" "b" "c"
--   == Foo1 10 "afoo" "bfoo" "cfoo"

instance Functor Foo2 where
  fmap = undefined

instance Functor Foo3 where
  fmap = undefined

instance Functor Tree1 where
  fmap = undefined

-- data Pair a b = Pair a b        (,)
-- Pair-t parciálisan alkalmazzuk 1 darab típusra
instance Functor (Pair c) where
  fmap :: (a -> b) -> Pair c a -> Pair c b
  fmap f (Pair c a) = Pair c (f a)

instance Functor Tree2 where
  fmap = undefined

--      Tree4 Bool a         Node3 (Bool -> Tree Bool a)
--                           Node3 (Tree Bool a, Tree Bool a)
-- data Tree3 i a = Leaf3 a | Node3 (i -> Tree3 i a)  -- i-szeres elágazás

instance Functor (Tree3 i) where
  fmap :: (a -> b) -> Tree3 i a -> Tree3 i b
  fmap f (Leaf3 a) = Leaf3 (f a)
  fmap f (Node3 g) = Node3 (\i -> fmap f (g i))
      -- i-edik map-elt fa: i-edik eredeti fa map-elése

instance Functor (Either a) where
  fmap = undefined

instance Functor Id where
  fmap = undefined

instance Functor (Const a) where
  fmap = undefined

instance Functor (Fun a) where
  fmap = undefined
