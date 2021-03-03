
{-# language InstanceSigs #-}


-- Semigroup, Monoid, Functor
------------------------------------------------------------

import Prelude hiding (Either(..), Functor(..), Semigroup(..), Monoid(..))

infixr 6 <>
class Semigroup a where
  (<>) :: a -> a -> a   -- asszociatív

class Semigroup a => Monoid a where
  mempty :: a
  -- mempty egységeleme a (<>) műveletnek

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- Feladat: írd meg a következő instance-okat! (+feleljen meg az osztály törvényeknek!)

instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (<>) :: (a, b) -> (a, b) -> (a, b)
  (<>) (a, b) (a', b') = (a <> a', b <> b')  -- párok kombinálása = mezők kombinálása

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty :: (a, b)
  mempty = (mempty, mempty)                      -- párok egységeleme, egységelemek párja

-- házi feladat
instance Semigroup b => Semigroup (a -> b) where   -- függvény kimenete kombinálható --> függvény kombinálása
  (<>) = undefined

instance Monoid b => Monoid (a -> b) where   -- függvény kimenetének van egységelem --> mi az egységelem függvény
  mempty = undefined

-- ajánlás: nehezet ugorjuk át.

data    Foo1 a      = Foo1 Int a a a
data    Foo2 a      = Foo2 Bool a Bool

data    Foo3 a      = Foo3 a a a a a
data    Tree1 a     = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show
data    Tree2 a     = Node2 a [Tree2 a] deriving Show
data    Pair a b    = Pair a b
data    Either' a b = Left' a | Right' b
data    Tree3 i a   = Leaf3 a | Node3 (i -> Tree3 i a)  -- i-szeres elágazás
newtype Id a        = Id a
newtype Const a b   = Const a
newtype Fun a b     = Fun (a -> b)

instance Functor Foo1 where
  fmap :: (a -> b) -> Foo1 a -> Foo1 b
  fmap f (Foo1 n a1 a2 a3) = Foo1 n (f a1) (f a2) (f a3)

instance Functor Foo2 where
  fmap f (Foo2 b1 a b2) = Foo2 b1 (f a) b2

instance Functor Foo3 where
  fmap f (Foo3 a1 a2 a3 a4 a5) = Foo3 (f a1) (f a2) (f a3) (f a4) (f a5)

instance Functor Tree1 where
  fmap f (Leaf1 a)   = Leaf1 (f a)
  fmap f (Node1 l r) = Node1 (fmap f l) (fmap f r)

-- Pair   :: * -> * -> *   (2-paraméteres)
-- Pair a :: * -> *        (1-paraméteres típus konstruktor)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Functor Tree2 where
  fmap f (Node2 a ts) = Node2 (f a) (map (fmap f) ts)

instance Functor (Tree3 i) where
  fmap f (Leaf3 a) = Leaf3 (f a)
  fmap f (Node3 g) = Node3 (\i -> fmap f (g i))

     -- g :: i -> Tree i a        -- fák i-vel indexelt sorozata
     -- _ :: i -> Tree i b
     -- \i -> fmap f (g i)        -- minden részfát rekurzívan map-elünk

-- legyen i = Bool
-- data Tree3 a = Leaf3 a | Node3 (Bool -> Tree3 a)
-- data Tree3 a = Leaf3 a | Node3 (Tree3 a) (Tree3 a)

-- legyen i = Int
-- data Tree3 a = Leaf3 a | Node3 (Int -> Tree3 a)     -- 2^64 részfa minden Node3 alatt


instance Functor (Either' a) where
  fmap = undefined

instance Functor Id where
  fmap = undefined

instance Functor (Const a) where
  fmap = undefined

instance Functor (Fun a) where
  fmap = undefined


-- bónusz feladat: definiáld a foldl függvényt listára úgy, hogy csak foldr-t és lambda kifejezést
-- használhatsz (rekurziót sem!).
