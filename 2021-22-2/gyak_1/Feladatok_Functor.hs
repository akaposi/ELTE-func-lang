
{-# language InstanceSigs #-}

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
  fmap f (Foo1 n x y z) = Foo1 n (f x) (f y) (f z)

instance Functor Tree1 where
  fmap f (Leaf1 a)   = Leaf1 (f a)
  fmap f (Node1 l r) = Node1 (fmap f l) (fmap f r)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Functor Foo2 where
  fmap f (Foo2 b a b') = Foo2 b (f a) b'

instance Functor Foo3 where
  fmap f (Foo3 a1 a2 a3 a4 a5) = Foo3 (f a1) (f a2) (f a3) (f a4) (f a5)

instance Functor Tree2 where
  fmap f (Node2 a ts) = Node2 (f a) (map (fmap f) ts)

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

-- Bónusz feladatok
--------------------------------------------------------------------------------

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip fab = (fmap fst fab, fmap snd fab)

apply :: Functor f => f (a -> b) -> a -> f b
apply fab a = fmap ($ a) fab

first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first f (a, c) = fmap (\b -> (b, c)) (f a)

second :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
second f (c, a) = fmap (\b -> (c, b)) (f a)

data Sum f g a = Inl (f a) | Inr (g a) deriving Show
data Product f g a = Product (f a) (g a) deriving Show
newtype Compose f g a = Compose (f (g a)) deriving Show

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap f (Inl fa) = Inl (fmap f fa)
  fmap f (Inr ga) = Inr (fmap f ga)

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap f (Product fa ga) = Product (fmap f fa) (fmap f ga)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose (fmap (fmap f) fga)


-- bónusz bónusz: mire használható ez a függvény? Tipp: a megoldáshoz
-- rekurzió szükséges.
löb :: Functor f => f (f a -> a) -> f a
löb ffaa = go where
  go = fmap ($ go) ffaa

-- bónusz bónusz 2:
newtype Fix f = Fix (f (Fix f))

fold :: Functor f => (f a -> a) -> Fix f -> a
fold g = go where
  go (Fix ff) = g (fmap go ff)
