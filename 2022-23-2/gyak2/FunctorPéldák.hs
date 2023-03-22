
{-# language StandaloneDeriving, UndecidableInstances #-}


data List a = Nil | Cons a (List a) deriving (Show)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a)   = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

data Twice a = Twice a a deriving (Eq, Show)

instance Functor Twice where
  fmap :: (a -> b) -> Twice a -> Twice b
  fmap f (Twice x y) = Twice (f x) (f y)

data Triple a = Triple a a a deriving (Eq, Show)

instance Functor Triple where
  fmap :: (a -> b) -> Triple a -> Triple b
  fmap f (Triple x y z) = Triple (f x) (f y) (f z)

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair c) where
  fmap :: (a -> b) -> Pair c a -> Pair c b
  fmap f (Pair c a) = Pair c (f a)

data Foo1 a = Foo1 Int a a a deriving Show

instance Functor Foo1 where
  fmap f (Foo1 n x y z) = Foo1 n (f x) (f y) (f z)

data Foo2 a = Foo2 Bool a Bool deriving Show

instance Functor Foo2 where
  fmap f (Foo2 b a b') = Foo2 b (f a) b'

data RoseTree a = RTNode a [RoseTree a] deriving Show

instance Functor RoseTree where
  fmap f (RTNode a ts) = RTNode (f a) (map (fmap f) ts)

newtype Id a = Id a deriving Show

instance Functor Id where
  fmap f (Id a) = Id (f a)

newtype Const a b = Const a deriving Show

instance Functor (Const a) where
  fmap f (Const a) = Const a

newtype Fun a b = Fun (a -> b)

instance Functor (Fun a) where
  fmap f (Fun g) = Fun (f . g)

-- Bónusz feladatok
--------------------------------------------------------------------------------

-- Add meg a következő definíciókat típushelyesen,
-- végtelen loop és kivételek nélkül!

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip fab = (fmap fst fab, fmap snd fab)

apply :: Functor f => f (a -> b) -> a -> f b
apply fab a = fmap ($ a) fab

first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first f (a, c) = fmap (\x -> (x, c)) (f a)

second :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
second f (c, a) = fmap (\x -> (c, x)) (f a)

data Sum f g a = Inl (f a) | Inr (g a) deriving Show

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap f (Inl fa) = Inl (fmap f fa)
  fmap f (Inr ga) = Inr (fmap f ga)

data Product f g a = Product (f a) (g a) deriving Show

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap f (Product fa ga) = Product (fmap f fa) (fmap f ga)

newtype Compose f g a = Compose (f (g a)) deriving Show

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose (fmap (fmap f) fga)

newtype Exp x f a = Exp (x -> f a)

instance Functor f => Functor (Exp x f) where
  fmap f (Exp g) = Exp (fmap (fmap f) g)

-- Mire használható ez a függvény? Tipp: a megoldáshoz rekurzió szükséges.
löb :: Functor f => f (f a -> a) -> f a
löb ffa = let fa = fmap ($ fa) ffa in fa

-- példa a használatra: listák, amelyek minden eleme hivatkozhat bármelyik másik elemre
-- hasonló egy Excel tábla sorának kiértékeléséhez
mylist1 = löb [\xs -> 100, \xs -> 200, \xs -> xs !! 0 + xs !! 1] -- [100, 200, 300]
mylist2 = löb [\xs -> xs !! 1 * xs !! 2, \_ -> 10, \_ -> 20]     -- [200, 10, 20]

newtype Fix f = Fix {unFix :: f (Fix f)}

-- A "StandaloneDeriving" opciót használjuk itt, mert
-- a standard "deriving Show" már nem működik.
deriving instance Show (f (Fix f)) => Show (Fix f)

fold :: Functor f => (f a -> a) -> Fix f -> a
fold f ff = let go (Fix ff) = f (fmap go ff) in go ff

data Free f a = Pure a | Free (f (Free f a))

deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)

instance Functor f => Functor (Free f) where
  fmap f (Pure a)   = Pure (f a)
  fmap f (Free ffa) = Free (fmap (fmap f) ffa)

newtype Cont r a = Cont {unCont :: (a -> r) -> r}

instance Functor (Cont r) where
  fmap f (Cont g) = Cont $ \k -> g (k . f)
