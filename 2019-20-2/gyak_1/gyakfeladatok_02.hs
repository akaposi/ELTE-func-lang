
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
  (<>) = undefined

instance Monoid [a] where
  mempty = undefined

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (<>) = undefined

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = undefined

instance Semigroup b => Semigroup (a -> b) where
  (<>) = undefined

instance Monoid b => Monoid (a -> b) where
  mempty = undefined


-- Feladat: írj Functor instance-t az összes alábbi típushoz!

data    Tree1 a     = Leaf1 a | Node1 (Tree1 a) (Tree1 a)
data    Tree2 a     = Node2 a [Tree2 a]
data    Pair a b    = Pair a b
data    Either' a b = Left' a | Right' b
data    Tree3 i a   = Leaf3 a | Node3 (i -> Tree3 i a)
newtype Id a        = Id a
newtype Const a b   = Const a
newtype Fun a b     = Fun (a -> b)

instance Functor Tree1 where
  fmap = undefined

instance Functor Tree2 where
  fmap = undefined

instance Functor (Tree3 i) where
  fmap = undefined

instance Functor (Pair a) where
  fmap = undefined

instance Functor (Either' a) where
  fmap = undefined

instance Functor Id where
  fmap = undefined

instance Functor (Const a) where
  fmap = undefined

instance Functor (Fun a) where
  fmap = undefined
