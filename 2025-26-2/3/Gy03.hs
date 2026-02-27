{-# LANGUAGE InstanceSigs, QuantifiedConstraints, StandaloneDeriving #-}
{- HLINT ignore "Use newtype instead of data" -}
module Gy03 where

import Prelude hiding (Maybe(..), Either(..))

-- Folding: Simulation of recursion on a list
-- foldr :: (a ->  b  ->  b ) -> b -> [a] -> b
-- (:)   :: (a -> [a] -> [a])
--
--                        [] :: [a]
-- Folding combines all expressions of type 'a' in a list.

{-
Example:
foldr (-) 54 [10, 11]
= foldr (-) 54 (10 : (11 : []))
= 10 - (11 - 54)
= 10 - (-43)
= 53
-}

-- Let's try this with another type:

data Single a = Single a deriving (Eq, Show)
data Tuple a = Tuple a a deriving (Eq, Show)
data Quintuple a = Quintuple a a a a a deriving (Eq, Show)
data List a = Nil | Cons a (List a) deriving (Eq, Show)
data Maybe a = Just a | Nothing deriving (Eq, Show)

-- The general idea is:
-- every element of a should be part of the fold, once
-- the elements should be folded in the same order as they appear in the structure

foldrSingle :: (a -> b -> b) -> b -> Single a -> b
foldrSingle f b (Single a) = f a b

-- Think of f as Cons and b as Nil

foldrTuple :: (a -> b -> b) -> b -> Tuple a -> b
foldrTuple f b (Tuple a a') = f a (f a' b)

foldrQuintuple :: (a -> b -> b) -> b -> Quintuple a -> b
foldrQuintuple f b (Quintuple x y z u v) = f x (f y (f z (f u (f v b))))

foldrList :: (a -> b -> b) -> b -> List a -> b
foldrList f b Nil = b
foldrList f b (Cons x xs) = f x (foldrList f b xs)

foldrMaybe :: (a -> b -> b) -> b -> Maybe a -> b
foldrMaybe f b Nothing = b
foldrMaybe f b (Just a) = f a b

-- Similar to mappability, folding can also be generalized with the help of the Foldable type class.
{-
:i Foldable
type Foldable :: (* -> *) -> Constraint
class Foldable t where
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap' :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
  {-# MINIMAL foldMap | foldr #-}
    -- Defined in ‘Data.Foldable’
-}
-- In the type class, all other operations are performed using foldr (or later foldMap).
-- For example:
-- sum = foldr (+) 0
-- product = foldr (*) 1
-- toList = foldr (:) []

instance Foldable Single where
  foldr :: (a -> b -> b) -> b -> Single a -> b
  foldr = foldrSingle

  foldMap :: Monoid m => (a -> m) -> Single a -> m
  foldMap f (Single a) = f a 

instance Foldable Tuple where
  foldr :: (a -> b -> b) -> b -> Tuple a -> b
  foldr = foldrTuple

  foldMap :: Monoid m => (a -> m) -> Tuple a -> m
  foldMap f (Tuple a a') = (f a) <> (f a')

instance Foldable Quintuple where
  foldr :: (a -> b -> b) -> b -> Quintuple a -> b
  foldr = foldrQuintuple

instance Foldable List where
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr = foldrList

  foldMap :: Monoid m => (a -> m) -> List a -> m
  foldMap f Nil = mempty
  foldMap f (Cons x xs) = (f x) <> (foldMap f xs)

instance Foldable Maybe where
  foldr :: (a -> b -> b) -> b -> Maybe a -> b
  foldr = foldrMaybe

data NonEmpty a = Last a | NECons a (NonEmpty a) deriving (Eq, Show)
data NonEmpty2 a = NECons2 a (List a) deriving (Eq, Show)
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)
data Either e a = Left e | Right a deriving (Eq, Show)
data BiTuple e a = BiTuple e a deriving (Eq, Show)
data TriEither e1 e2 a = LeftT e1 | MiddleT e2 | RightT a deriving (Eq, Show)
data BiList a b = ACons a (BiList a b) | BCons b (BiList a b) | ABNill deriving (Eq, Show)

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f b (Last a) = f a b
  foldr f b (NECons a ne) = f a (foldr f b ne)

instance Foldable NonEmpty2 where
  foldr :: (a -> b -> b) -> b -> NonEmpty2 a -> b
  foldr f b (NECons2 a l) = f a (foldr f b l)

-- Inorder 
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f b (Leaf a) = f a b
  foldr f b (Node t1 a t2) = foldr f (f a (foldr f b t2)) t1
  -- foldr f b t1
  -- foldr f b t2
  -- Note: To see the order in which foldr traverses the tree, you can try
  -- foldr (:) [] (Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5))
  -- in ghci, which turns the given tree into a list. Then you can experiment with
  -- different definitions of foldr above to see how it changes the order of the list.

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf a) = f a
  foldMap f (Node t1 a t2) = 
    (foldMap f t1) <> (f a) <> (foldMap f t2)
  -- foldMap f t1
  -- foldMap f t2

instance Foldable (Either fixed) where
  foldr :: (a -> b -> b) -> b -> Either fixed a -> b
  foldr f b (Left _) = b
  foldr f b (Right a) = f a b

  foldMap :: Monoid m => (a -> m) -> Either fixed a -> m
  foldMap f (Left _) = mempty 
  foldMap f (Right a) = f a

instance Foldable (BiTuple fixed) where
  foldr = undefined

instance Foldable (TriEither fixed1 fixed2) where
  foldr = undefined

instance Foldable (BiList fixed) where
  foldr = undefined

-- Higher-kinded Constraints

data Apply f a = MkApply (f a) deriving (Eq, Show)
data Compose f g a = MkCompose (f (g a)) deriving (Eq, Show)
data Fix f a = MkFix (f (Fix f a))
data Sum f a b = FLeft (f a) | FRight (f b) deriving (Eq, Show)
data Prod f a b = FProd (f a) (f b) deriving (Eq, Show)

instance Foldable f => Foldable (Apply f) where
  foldr :: Foldable f => (a -> b -> b) -> b -> Apply f a -> b
  foldr f b (MkApply fa) = foldr f b fa
  -- Foldable f means foldr :: (a -> b -> b) -> b -> f a -> b

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldr :: (Foldable f, Foldable g) => (a -> b -> b) -> b -> Compose f g a -> b
  foldr f b (MkCompose fga) = foldr (\ga b -> foldr f b ga) b fga
  -- Foldable f means foldrf :: (a -> b -> b) -> b -> f a -> b
  -- Foldable g means foldrg :: (a -> b -> b) -> b -> g a -> b
  -- fga : f (g a)
  -- foldrf (?0 :: g a -> b -> b) b fga
  -- ?0 = \ga b -> foldrg (f :: a -> b -> b) b ga

instance Foldable f => Foldable (Fix f) where
  foldr :: Foldable f => (a -> b -> b) -> b -> Fix f a -> b
  foldr f b (MkFix ffixfa) = foldr (\fixfa b -> foldr f b fixfa) b ffixfa
  -- ffixfa : f (Fix f a)
  -- Foldable f means foldr :: (a -> b -> b) -> b -> f a -> b
  -- \fixfa b -> foldr f b fixfa : Fix f a -> b -> b

instance Foldable f => Foldable (Sum f fixed) where
  foldr = undefined

instance Foldable f => Foldable (Prod f fixed) where
  foldr = undefined

data FList f a = FNil | FCons (f a) (f (FList f a))
-- Examples:
-- FCons [1, 2, 3] [FCons [4, 5, 6] []] :: FList List Int
-- FCons (Just True) (Just (FCons (Just False) (Just FNil))) :: FList Maybe Bool

instance Foldable f => Foldable (FList f) where
  foldr :: Foldable f => (a -> b -> b) -> b -> FList f a -> b
  foldr f b FNil = b
  foldr f b (FCons fa ffa) = undefined -- foldr _ _ fa
  -- Foldable f means foldr :: (a -> b -> b) -> b -> f a -> b
  -- fa :: f a
  -- ffa :: f (FList f a)
  -- foldr (?0 : FList f a -> b -> b) ? ffa
  -- foldr ? ? fa


{-

Semigroup: A set H that has an associative operation <>.
a <> (b <> c) = (a <> b) <> c
In Haskell, this is represented by the Semigroup type class.

Write a Semigroup instance for the following types!

-}

instance Semigroup Bool where
  (<>) :: Bool -> Bool -> Bool
  (<>) = (&&) -- (||)

instance Semigroup Int where
  (<>) :: Int -> Int -> Int
  (<>) = (+) -- (*)

data Endo a = MkEndo (a -> a)

instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (MkEndo f) <> (MkEndo g) = MkEndo (f . g) 

{-
Multiple operations can be selected for a set to form a semigroup
E.g.:
(ℝ, +)         is a semigroup
(ℝ, *)         is a semigroup
(A, \x y -> x) is a semigroup
(A, \x y -> y) is a semigroup
-}

{-

Monoid: A semigroup that has a unit element called mempty, satisfying

mempty <> x = x <> mempty = x

E.g.:
(ℝ, +, 0)        is a monoid
(ℝ, *, 1)        is a monoid
(List A, ++, []) is a monoid

In Haskell, this is the Monoid type class
Let's write the Monoid instance for the types chosen above
-}

instance Monoid Bool where
  mempty :: Bool
  mempty = True
  -- mempty && True = True
  -- mempty && False = False
  -- True && mempty = True
  -- False && mempty = False

instance Monoid Int where
  mempty :: Int
  mempty = 0

instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = MkEndo (\x -> x)


-- Alternative to the foldr operation: foldMap
-- foldMap :: Monoid m => (a -> m) -> f a -> m
-- Uses the <> operation for combination, e.g.:
-- foldMap f [a,b,c ...] = f a <> f b <> f c <> ... <> mempty

-- Write the foldMap operation for the types above!
-- It is worth noting that
-- foldMap f xs = foldr (\x a -> f x <> a) mempty xs
-- foldr f b xs = let (MkEndo g) = foldMap (\x -> MkEndo (\a -> f x a)) xs in g b


-- Practice:

data Tree2 a = Leaf2 | Node2 (Tree2 a) a (Tree2 a) deriving (Eq, Show)
data RoseTree a = RoseLeaf a | RoseNode [RoseTree a] deriving (Eq, Show)
data Tree3 a = Leaf3 a | Node3 (Tree3 a) (Tree3 a) deriving (Eq, Show)
data SkipList a = Skip (SkipList a) | SCons a (SkipList a) | SNill deriving (Eq, Show)
data CrazyType a = C1 a a | C2 a Int | C3 (CrazyType a) deriving (Eq, Show)
data Either3 a b c = Left3 a | Middle3 b | Right3 c deriving (Eq, Show)
data Triplet a b c = Triplet a b c deriving (Eq, Show)
data SplitTree a b = SplitTree (Tree a) a b (Tree b) deriving (Eq, Show)
data TriCompose f g h a = TriCompose (f (g (h a))) deriving (Eq, Show)
data Free f a = Pure a | Free (f (Free f a))

-- Don't mind me
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (Fix f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (Fix f a)
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (FList f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (FList f a)
