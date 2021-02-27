{-# options_ghc -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeApplications #-}

module Notes03 where
  
-- More functor instances:

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- Already defined in Prelude:
--   instance Functor [] 
--   instance Functor (Either a)
--   instance Functor ((,) a)
--   instance Functor Maybe

data Pair a b = Pair a b
              deriving (Show)
instance Functor (Pair a) where
  fmap = undefined

data Either' a b = Left' a
                 | Right' b
                 deriving (Show)
instance Functor (Either' a) where
  fmap = undefined

data BinTree a = BLeaf a
               | BNode (BinTree a) (BinTree a)
               deriving (Show, Eq)

instance Functor BinTree where
  fmap = undefined


data K a b = Const a
           deriving (Show)

instance Functor (K a) where
  fmap = undefined


data T1 a = Leaf1 a
          | Node1 [T1 a]
          deriving (Show)

instance Functor T1 where
  fmap = undefined


data Fun a b = MkFun (a -> b)

instance Functor (Fun a) where
  fmap = undefined


data T2 a = Leaf2 a
          | Node2 (Int -> a)

instance Functor T2 where
  fmap = undefined


-- Semigroup/Monoid:

-- class Semigroup m where
--   (<>) :: m -> m -> m
-- class Monoid m where
--   mempty :: m

--   Laws:     (x <> y) <> z == x <> (y <> z)           (associativity)
--             (mempty <> x) == x == (x <> mempty)      (left and right identity)

newtype List a = MkList [a] deriving (Show, Eq, Ord)
instance Semigroup (List a) where
  (<>) = undefined
instance Monoid (List a) where
  mempty = undefined

concatList :: [[a]] -> [a]
concatList = undefined

concatBinTree :: BinTree [a] -> [a]
concatBinTree = undefined

mconcatList :: Monoid m => [m] -> m
mconcatList = undefined

mconcatBinTree :: Monoid m => BinTree m -> m
mconcatBinTree = undefined

-- We can define concatList using 
concatList' :: [[a]] -> [a]
concatList' = mconcatList

-- Use mconcatBinTree to define:
flattenBinTree :: BinTree a -> [a]
flattenBinTree = undefined

-- Define a new monoid instance and use mconcatBinTree to define:
maxTree :: BinTree Int -> Int
maxTree = undefined

-- Bonus functor instances:
newtype Compose f g a = MkCompose (f (g a))
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap = undefined

newtype Product f g a = MkProduct (f a, g a)
instance (Functor f, Functor g) => Functor (Product f g) where
  fmap = undefined

data Sum f g a = SumLeft (f a)
               | SumRight (g a)
instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap = undefined