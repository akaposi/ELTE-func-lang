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

-- mapPair :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
-- mapPair f g (a, c) = (f a, g c)

data Pair a b = Pair a b
              deriving (Show)

instance Functor (Pair a) where
  fmap :: (c -> d) -> Pair a c -> Pair a d
  fmap f (Pair x y) = Pair x (f y)

data Either' a b = Left' a
                 | Right' b
                 deriving (Show)
instance Functor (Either' a) where
  fmap :: (c -> d) -> Either' a c -> Either' a d
  fmap f (Left' x)  = Left' x
  fmap f (Right' y) = Right' (f y)


data BinTree a = BLeaf a
               | BNode (BinTree a) (BinTree a)
               deriving (Show, Eq)
instance Functor BinTree where
  fmap f (BLeaf x)   = BLeaf (f x)
  fmap f (BNode l r) = BNode (fmap f l) (fmap f r)


data K a b = Const a
           deriving (Show)
instance Functor (K a) where
  fmap _ (Const x) = Const x


data T1 a = Leaf1 a
          | Node1 [T1 a]
          deriving (Show)
instance Functor T1 where
  fmap :: (a -> b) -> T1 a -> T1 b
  fmap f (Leaf1 x)  = Leaf1 (f x)
  fmap f (Node1 xs) = Node1 (fmap @[] (fmap @T1 f) xs)
   -- xs             :: [T1 a]
   -- ?0             :: [T1 b]
   -- ?0 = map ?1 xs :: [T1 b] if ?1 :: T1 a -> T1 b
   -- ?1 = fmap f

data T3 a = Leaf3 a
          | Node3 (T1 [T3 a])
          deriving (Show)
instance Functor T3 where
  fmap :: (a -> b) -> T3 a -> T3 b
  fmap f (Leaf3 x)  = Leaf3 (f x)
  fmap f (Node3 xs) = Node3 (fmap @T1 (fmap @[] (fmap @T3 f)) xs)

data Fun a b = MkFun (a -> b)

instance Functor (Fun c) where
  fmap f (MkFun g) = MkFun (f . g)

data T2 a = Leaf2 a
          | Node2 (Int -> a)  -- (Int -> a) ~ Fun Int a

instance Functor T2 where
  fmap f (Leaf2 x)  = Leaf2 (f x)
  fmap f (Node2 g) = Node2 (fmap f g)
  -- fmap f (Node2 g) = Node2 (f . g)



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