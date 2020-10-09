
-- Semigroup, Monoid, Functor
------------------------------------------------------------

import Prelude hiding (Either(..), Functor(..), Semigroup(..), Monoid(..))

infixr 6 <>
class Semigroup a where
  (<>) :: a -> a -> a

class Semigroup a => Monoid a where
  mempty :: a

class Functor f where
  fmap :: (a -> b) -> f a -> f b


-- Feladat: írd meg a következő instance-okat! (+megfelel az osztály törvényeknek!)

instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []

-- instance Eq a => Eq (Maybe a) where
--   Just x == Just y = (x == y)
--   Nothing == Nothing = True
--   _ == _ = False

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (x, y) <> (x', y') = (x <> x', y <> y')

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)

instance Semigroup b => Semigroup (a -> b) where
  f <> g = \a -> f a <> g a

instance Monoid b => Monoid (a -> b) where
  mempty = \_ -> mempty


-- Feladat: írj Functor instance-t az összes alábbi típushoz!
-- Általánosan fmap : függvényt alkalmazzuk az utolsó típusparaméter összes előfordulásán
--                    az adatstruktúrában.

data    Foo1 a      = Foo1 Int a a a
data    Foo2 a      = Foo2 Bool a Bool
data    Foo3 a      = Foo3 a a a a a
data    Tree1 a     = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show
data    Tree2 a     = Node2 a [Tree2 a] deriving Show
data    Pair a b    = Pair a b
data    Either' a b = Left' a | Right' b
data    Tree3 i a   = Leaf3 a | Node3 (i -> Tree3 i a)
newtype Id a        = Id a
newtype Const a b   = Const a
newtype Fun a b     = Fun (a -> b)

instance Functor Foo1 where
  -- fmap :: (a -> b) -> Foo1 a -> Foo1 b
  fmap f (Foo1 n x y z) = Foo1 n (f x) (f y) (f z)

instance Functor Tree1 where
  -- fmap :: (a -> b) -> Tree1 a -> Tree1 b
  fmap f (Leaf1 a)   = Leaf1 (f a)
  fmap f (Node1 l r) = Node1 (fmap f l) (fmap f r)

instance Functor (Pair a) where    -- extra (utolsó) paraméter fölött map-elünk
  -- fmap :: (b -> c) -> Pair a b -> Pair a c
  fmap f (Pair a b) = Pair a (f b) -- (második mezőnek van b típusa)

-- példa :
t1 :: Tree1 Int
t1 = Node1 (Leaf1 100) (Leaf1 200)

t2 :: Tree1 Bool
t2 = fmap even t1  -- Node1 (Leaf1 True) (Leaf1 True)

instance Functor Foo2 where
  fmap f (Foo2 b a b') = Foo2 b (f a) b'

instance Functor Foo3 where
  fmap f (Foo3 a1 a2 a3 a4 a5) = Foo3 (f a1) (f a2) (f a3) (f a4) (f a5)

leaf2 :: Tree2 Int
leaf2 = Node2 1000 []     -- levél konstruktor

tree2 :: Tree2 Int
tree2 = Node2 100 [leaf2, leaf2]

tree3 :: Tree2 Int
tree3 = Node2 100 [tree2, tree2, tree2]

instance Functor Tree2 where
  fmap f (Node2 a ts) = Node2 (f a) (map (fmap f) ts)

tree4 :: Tree3 Int Int
tree4 = Node3 (\i -> Leaf3 i)

-- házi feladat lerajzolni (vizualizálni mint fa)
tree5 :: Tree3 Int Int
tree5 = Node3 (\i -> Node3 (\j -> Leaf3 (i + j)))

instance Functor (Tree3 i) where
  fmap f (Node3 g) = Node3 (\i -> fmap f (g i))

instance Functor (Either' a) where
  fmap f (Left' a)  = Left' a
  fmap f (Right' b) = Right' (f b)

instance Functor Id where
  fmap f (Id a) = Id (f a)

instance Functor (Const a) where
  fmap f (Const a) = Const a

instance Functor (Fun a) where
  fmap f (Fun g) = Fun (f . g)
