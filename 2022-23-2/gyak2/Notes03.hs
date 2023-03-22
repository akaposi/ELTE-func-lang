{-# language StandaloneDeriving, UndecidableInstances #-}

-- Osztályok, algebrai típusok
--------------------------------------------------------------------------------

data Color = Red | Green | Blue
data List a = Nil | Cons a (List a)
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- írd meg a következő instance-okat!

instance Eq Color where
  (==) :: Color -> Color -> Bool
  (==) = undefined

instance Ord Color where
  (<=) :: Color -> Color -> Bool
  (<=)  = undefined

instance Show Color where
  show :: Color -> String
  show  = undefined

instance Eq a => Eq (List a) where
  (==) :: List a -> List a -> Bool
  (==) = undefined

instance Show a => Show (List a) where
  show :: List a -> String
  show = undefined

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) = undefined

instance Show a => Show (Tree a) where
  show :: Tree a -> String
  show = undefined



-- Functor
--------------------------------------------------------------------------------

-- írd meg a következő instance-okat!

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap = undefined

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap = undefined

data Twice a = Twice a a deriving (Eq, Show)

instance Functor Twice where
  fmap :: (a -> b) -> Twice a -> Twice b
  fmap = undefined

data Triple a = Triple a a a deriving (Eq, Show)

instance Functor Triple where
  fmap :: (a -> b) -> Triple a -> Triple b
  fmap = undefined

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair c) where
  fmap :: (a -> b) -> Pair c a -> Pair c b
  fmap = undefined


-- Bónusz feladatok
--------------------------------------------------------------------------------

-- Add meg a következő definíciókat típushelyesen,
-- végtelen loop és kivételek nélkül!

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = undefined

apply :: Functor f => f (a -> b) -> a -> f b
apply = undefined

first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first = undefined

second :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
second = undefined

data Sum f g a = Inl (f a) | Inr (g a) deriving Show

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap = undefined

data Product f g a = Product (f a) (g a) deriving Show

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap = undefined

newtype Compose f g a = Compose (f (g a)) deriving Show

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap = undefined

newtype Exp x f a = Exp (x -> f a)

instance Functor f => Functor (Exp x f) where
  fmap = undefined

-- Mire használható ez a függvény? Tipp: a megoldáshoz rekurzió szükséges.
löb :: Functor f => f (f a -> a) -> f a
löb = undefined

newtype Fix f = Fix (f (Fix f))

-- A "StandaloneDeriving" opciót használjuk itt, mert
-- a standard "deriving Show" már nem működik.
deriving instance Show (f (Fix f)) => Show (Fix f)

fold :: Functor f => (f a -> a) -> Fix f -> a
fold = undefined

data Free f a = Pure a | Free (f (Free f a))

deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)

instance Functor f => Functor (Free f) where
  fmap = undefined

newtype Cont r a = Cont ((a -> r) -> r)

instance Functor (Cont r) where
  fmap = undefined
