module Notes05 where
import Prelude hiding (Functor(..))

-- Functor exercises from https://github.com/AndrasKovacs/ELTE-func-lang/blob/master/2019-20-2/gyak_1/gyakfeladatok_02.hs

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap f []     = []
  fmap f (x:xs) = f x : fmap f xs
 

data    Foo1 a      = Foo1 Int a a a
data    Foo1' b a   = Foo1' b a a a

instance Functor Foo1 where
  fmap f (Foo1 a b c d) = Foo1 a (f b) (f c) (f d)
instance Functor (Foo1' b) where
  fmap f (Foo1' a b c d) = Foo1' a (f b) (f c) (f d)


data    Foo2 a      = Foo2 Bool a Bool
data    Foo2' b a   = Foo2' b a b

instance Functor Foo2 where
  fmap f (Foo2 a b c) = Foo2 a (f b) c
instance Functor (Foo2' b) where
  fmap f (Foo2' a b c) = Foo2' a (f b) c


data    Foo3 a      = Foo3 a a a a a

instance Functor Foo3 where
  fmap f (Foo3 a b c d e) = Foo3 (f a) (f b) (f c) (f d) (f e)
 
data    Pair a b    = Pair a b
instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

data    Either' a b = Left' a | Right' b
instance Functor (Either' a) where
  fmap f (Left' a) = Left' a
  fmap f (Right' b) = Right' (f b)

newtype Id a        = Id a
instance Functor Id where
  fmap f (Id a) = Id (f a)

newtype Const a b   = Const a
instance Functor (Const a) where
  fmap f (Const a) = Const a


data    Tree1 a     = Leaf1 a | Node1 (Tree1 a) (Tree1 a)

instance Functor Tree1 where
  fmap f (Leaf1 a)   = Leaf1 (f a)
  fmap f (Node1 l r) = Node1 (fmap f l) (fmap f r)
 

data    Tree2 a     = Node2 a [Tree2 a]

instance Functor Tree2 where
  fmap f (Node2 a xs) = Node2 (f a) (fmap (fmap f) xs)
