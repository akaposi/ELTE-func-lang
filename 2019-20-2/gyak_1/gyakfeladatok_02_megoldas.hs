
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


-- Feladat: írd meg a következő instance-okat!

instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (x, y) <> (x', y') = (x <> x, y <> y')

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)

instance Semigroup b => Semigroup (a -> b) where
  (f <> g) x = f x <> g x

instance Monoid b => Monoid (a -> b) where
  mempty x = mempty

-- Feladat: írj Functor instance-t az összes alábbi típushoz!

data    Foo1 a      = Foo1 Int a a a
data    Foo2 a      = Foo2 Bool a Bool
data    Foo3 a      = Foo3 a a a a a
data    Tree1 a     = Leaf1 a | Node1 (Tree1 a) (Tree1 a)
data    Tree2 a     = Node2 a [Tree2 a]
data    Pair a b    = Pair a b
data    Either' a b = Left' a | Right' b
data    Tree3 i a   = Leaf3 a | Node3 (i -> Tree3 i a)
newtype Id a        = Id a
newtype Const a b   = Const a
newtype Fun a b     = Fun (a -> b)

instance Functor Foo1 where
  fmap f (Foo1 n x y z) = Foo1 n (f x) (f y) (f z)

instance Functor Foo2 where
  fmap f (Foo2 b a b') = Foo2 b (f a) b'

instance Functor Foo3 where
  fmap f (Foo3 a b c d e) = Foo3 (f a) (f b) (f c) (f d) (f e)

instance Functor Tree1 where
  fmap f (Leaf1 a) = Leaf1 (f a)
  fmap f (Node1 l r) = Node1 (fmap f l) (fmap f r)

instance Functor Tree2 where
  fmap f (Node2 a ts) = Node2 (f a) (map (fmap f) ts)

instance Functor (Tree3 i) where
  fmap f (Leaf3 a) = Leaf3 (f a)
  fmap f (Node3 g) = Node3 (fmap f . g)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Functor (Either' a) where
  fmap f (Left' a) = Left' a
  fmap f (Right' b) = Right' (f b)

instance Functor Id where
  fmap f (Id a) = Id (f a)

instance Functor (Const a) where
  fmap f (Const a) = Const a

instance Functor (Fun a) where
  fmap f (Fun g) = Fun (f . g)
