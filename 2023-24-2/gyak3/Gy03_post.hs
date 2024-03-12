{-# LANGUAGE InstanceSigs, QuantifiedConstraints, StandaloneDeriving #-}
module Gy03 where

import Prelude hiding (Maybe(..), Either(..))

-- Hajtogatás: Listára való rekurzió szimulációja
-- foldr :: (a ->  b  ->  b ) ->  b -> [a] -> b
--   (:) :: (a -> [a] -> [a])
-- foldr (+) 0 [1,2,3,4]
-- (+) 4 0 -> 4
-- foldr (+) 4 [1,2,3]
-- (+) 3 4 -> 7
-- foldr (+) 7 [1,2]
-- (+) 2 7 -> 9
-- foldr (+) 9 [1]
-- (+) 1 9 -> 10
-- foldr (+) 10 []
-- 10
-- foldr (:) [] [1,2,3]
-- (:) 3 []
--                         [] :: [a]
-- A hajtogatás összekombinálja az összes 'a' típusú kifejezést egy listában.
-- Próbáljuk ezt meg más típussal is eljátszani:
bl = False : bl


data Single a = Single a deriving (Eq, Show)
data Tuple a = Tuple a a deriving (Eq, Show)
data Quintuple a = Quintuple a a a a a deriving (Eq, Show)
data List a = Nil | Cons a (List a) deriving (Eq, Show)
data Maybe a = Just a | Nothing deriving (Eq, Show)
data NonEmpty a = Last a | NECons a (NonEmpty a) deriving (Eq, Show)
data NonEmpty2 a = NECons2 a (List a) deriving (Eq, Show)
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)
data Either e a = Left e | Right a deriving (Eq, Show)
data BiTuple e a = BiTuple e a deriving (Eq, Show)
data TriEither e1 e2 a = LeftT e1 | MiddleT e2 | RightT a deriving (Eq, Show)
data BiList a b = ACons a (BiList a b) | BCons b (BiList a b) | ABNill deriving (Eq, Show)
data Apply f a = MkApply (f a) deriving (Eq, Show)
data Fix f a = MkFix (f (Fix f a))
data Compose f g a = MkCompose (f (g a)) deriving (Eq, Show)
data Sum f a b = FLeft (f a) | FRight (f b) deriving (Eq, Show)
data Prod f a b = FProd (f a) (f b) deriving (Eq, Show)
data FList f a = FNil | FCons (f a) (f (FList f a))

foldrSingle :: (a -> b -> b) -> b -> Single a -> b
foldrSingle f b (Single a) = f a b

foldrTuple :: (a -> b -> b) -> b -> Tuple a -> b
foldrTuple f b (Tuple a a') = f a (f a' b)

foldrQuintuple :: (a -> b -> b) -> b -> Quintuple a -> b
foldrQuintuple = undefined

foldrList :: (a -> b -> b) -> b -> List a -> b
foldrList f b Nil = b
foldrList f b (Cons a as) = f a (foldr f b as)

foldrMaybe :: (a -> b -> b) -> b -> Maybe a -> b
foldrMaybe = undefined

-- Hasonlóan a mappolhatósághoz, a hajtogatás is általánosítható a Foldable típusosztály segítéségvel
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
-- A típusosztályban a többi művelet mind a foldr (vagy később a foldMap) segítségével
-- Például:
-- sum = foldr (+) 0
-- product = foldr (*) 1
-- toList = foldr (:) []

instance Foldable Single where
  foldr :: (a -> b -> b) -> b -> Single a -> b
  foldr = foldrSingle

instance Foldable Tuple where
  foldr :: (a -> b -> b) -> b -> Tuple a -> b
  foldr = foldrTuple

instance Foldable Quintuple where
  foldr :: (a -> b -> b) -> b -> Quintuple a -> b
  foldr = foldrQuintuple

instance Foldable List where
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr = foldrList

instance Foldable Maybe where
  foldr :: (a -> b -> b) -> b -> Maybe a -> b
  foldr = foldrMaybe

instance Foldable NonEmpty where

instance Foldable NonEmpty2 where

instance Foldable Tree where
  foldr f b (Leaf a) = f a b
  foldr f b (Node l a r) = foldr f (f a (foldr f b r)) l 

instance Foldable (Either fixed) where
  foldr f b (Left _) = b
  foldr f b (Right a) = f a b

-- data BiTuple e a = BiTuple e a deriving (Eq, Show)
-- data TriEither e1 e2 a = LeftT e1 | MiddleT e2 | RightT a deriving (Eq, Show)
-- data BiList a b = ACons a (BiList a b) | BCons b (BiList a b) | ABNill deriving (Eq, Show)


instance Foldable (BiTuple fixed) where
  foldr f b (BiTuple _ a) = f a b

instance Foldable (TriEither fixed1 fixed2) where
  -- foldr f b (LeftT _) = b
  -- foldr f b (MiddleT _) = b
  foldr f b (RightT a) = f a b
  foldr f b _ = b

instance Foldable (BiList fixed) where
  foldr f b (ACons _ ls) = foldr f b ls
  foldr f b (BCons a ls) = f a $ foldr f b ls
  foldr f b ABNill = b

-- data Apply f a = MkApply (f a) deriving (Eq, Show)
-- data Fix f a = MkFix (f (Fix f a))
-- data Compose f g a = MkCompose (f (g a)) deriving (Eq, Show)
-- data Sum f a b = FLeft (f a) | FRight (f b) deriving (Eq, Show)
-- data Prod f a b = FProd (f a) (f b) deriving (Eq, Show)
-- data FList f a = FNil | FCons (f a) (f (FList f a))

-- Magasabbrendű megkötések
instance Foldable f => Foldable (Apply f) where
  foldr f b (MkApply foldable_a) = foldr f b foldable_a 

instance Foldable f => Foldable (Fix f) where
  foldr f b (MkFix ffix) = foldr (flip (foldr f)) b ffix

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldr f b (MkCompose fga) = foldr (flip (foldr f)) b fga
  -- [[1,2,3], [1,2,3], [1,2,3]]

instance Foldable f => Foldable (Sum f fixed) where

instance Foldable f => Foldable (Prod f fixed) where

instance Foldable f => Foldable (FList f) where


{-

Félcsoport: Olyan H halmaz, amely rendelkezik egy <> asszociatív művelettel
Ez Haskellben a Semigroup típusosztály
-- (a <> b) <> c == a <> (b <> c)
Írjunk Semigroup instance-ot az alábbi típusokra!

-}

instance Semigroup Bool where
  (<>) :: Bool -> Bool -> Bool
  (<>) b b' = b && b'
  -- (<>) _ _ = True / False

instance Semigroup Int where
  (<>) :: Int -> Int -> Int
  (<>) a b = a * b

data Endo a = MkEndo (a -> a)

instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (<>) = undefined

{-
Egy halmazhoz több művelet is választható, hogy félcsoportot alkossanak
Pl.:
(ℝ, +)         egy félcsoport
(ℝ, *)         egy félcsoport
(A, \x y -> x) egy félcsoport
(A, \x y -> y) egy félcsoport
-}

{-

Egységelemes félcsoport: Egy olyan félcsoport, amelynek van egy mempty egységeleme, azaz

mempty <> x = x <> mempty = x

Pl.:
(ℝ, +, 0)        egy egységelemes félcsoport
(ℝ, *, 1)        egy egységelemes félcsoport
(List A, ++, []) egy egységelemes félcsoport

Ez Haskellben a Monoid típusosztály
Írjunk a fent választott félcsoportnak az egységelemét
-}

instance Monoid Bool where
  mempty :: Bool
  mempty = undefined

instance Monoid Int where
  mempty :: Int
  mempty = undefined

instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = undefined


-- A foldr művelet alternatívája: foldMap
-- foldMap :: Monoid m => (a -> m) -> f a -> m
-- A <> műveletet használja kombinálásra, pl.:
-- foldMap f [a,b,c ...] = f a <> f b <> f c <> ... <> mempty

-- Írd meg a fenti típusokra a foldMap műveletet is!
-- Érdemes belátni, hogy
-- foldMap f xs = foldr (\x a -> f x <> a) mempty xs
-- foldr f b xs = let (MkEndo g) = foldMap (\x -> MkEndo (\a -> f x a)) xs in g b


-- Gyakorlás:
-- Írj ezekre Foldable instance-ot

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

-- Mágia, ignore me
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (Fix f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (Fix f a)
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (FList f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (FList f a)
