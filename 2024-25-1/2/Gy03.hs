{-# LANGUAGE InstanceSigs, QuantifiedConstraints, StandaloneDeriving #-}
module Gy03 where

import Prelude hiding (Maybe(..), Either(..))

-- Hajtogatás: Listára való rekurzió szimulációja
-- foldr :: (a ->  b  ->  b ) ->  b -> [a] -> b
--   (:) :: (a -> [a] -> [a])
--
--                         [] :: [a]
-- A hajtogatás összekombinálja az összes 'a' típusú kifejezést egy listában.
-- Próbáljuk ezt meg más típussal is eljátszani:

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
foldrSingle f base (Single a) = f a base

foldrTuple :: (a -> b -> b) -> b -> Tuple a -> b
foldrTuple (+) base (Tuple a b) = b + (a + base) -- Veri kőrzd

foldrQuintuple :: (a -> b -> b) -> b -> Quintuple a -> b
foldrQuintuple f base (Quintuple a b c d e) = f e (f d (f c (f b (f a base))))

foldrList :: (a -> b -> b) -> b -> List a -> b
foldrList _ base Nil = base
foldrList f base (Cons a as) = f a (foldrList f base as)

foldrMaybe :: (a -> b -> b) -> b -> Maybe a -> b
foldrMaybe _ base Nothing = base
foldrMaybe f base (Just a) = f a base

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
  foldMap :: Monoid m => (a -> m) -> Single a -> m
  foldMap f (Single a) = f a

instance Foldable Tuple where
  foldr :: (a -> b -> b) -> b -> Tuple a -> b
  foldr = foldrTuple
  foldMap :: Monoid m => (a -> m) -> Tuple a -> m
  foldMap f (Tuple a b) = f a <> f b

instance Foldable Quintuple where
  foldr :: (a -> b -> b) -> b -> Quintuple a -> b
  foldr = foldrQuintuple
  foldMap :: Monoid m => (a -> m) -> Quintuple a -> m
  foldMap f (Quintuple a b c d e) = f a <> f b <> f c <> f d <> f e

instance Foldable List where
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr = foldrList
  foldMap :: Monoid m => (a -> m) -> List a -> m
  foldMap _ Nil         = mempty
  foldMap f (Cons a as) = f a <> foldMap f as

instance Foldable Maybe where
  foldr :: (a -> b -> b) -> b -> Maybe a -> b
  foldr = foldrMaybe
  foldMap :: Monoid m => (a -> m) -> Maybe a -> m
  foldMap _ Nothing = mempty
  foldMap f (Just a) = f a

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f base (Last a)      = f a base
  foldr f base (NECons a as) = f a (foldr f base as)

instance Foldable NonEmpty2 where
  foldr :: (a -> b -> b) -> b -> NonEmpty2 a -> b
  foldr f base (NECons2 el as) = f el (foldr f base as)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f base (Leaf a)     = f a base
  foldr f base (Node l m r) = f m (foldr f (foldr f base l) r)
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf a)     = f a
  foldMap f (Node l m r) = f m <> foldMap f r <> foldMap f l

instance Foldable (Either fixed) where
  foldr :: (a -> b -> b) -> b -> Either fixed a -> b
  foldr _f base (Left _fixed) = base
  foldr f base (Right a) = f a base
  foldMap :: Monoid m => (a -> m) -> Either fixed a -> m
  foldMap f (Right a) = f a
  foldMap _ _         = mempty

instance Foldable (BiTuple fixed) where

instance Foldable (TriEither fixed1 fixed2) where
  foldr :: (a -> b -> b) -> b -> TriEither fixed1 fixed2 a -> b
  foldr f base (RightT a)        = f a base
  {-
  foldr f base (LeftT _fixed1)   = base
  foldr f base (MiddleT _fixed2) = base
  -}
  foldr _ base _   = base
  foldMap :: Monoid m => (a -> m) -> TriEither fixed1 fixed2 a -> m
  foldMap  f (RightT a) = f a
  foldMap _ _ = mempty

instance Foldable (BiList fixed) where

-- Magasabbrendű megkötések
instance Foldable f => Foldable (Apply f) where
  foldr :: Foldable f => (a -> b -> b) -> b -> Apply f a -> b
  foldr f base (MkApply fa) = foldr f base fa
  foldMap :: (Foldable f, Monoid m) => (a -> m) -> Apply f a -> m
  foldMap f (MkApply fa) = foldMap f fa

instance Foldable f => Foldable (Fix f) where
  foldr :: Foldable f => (a -> b -> b) -> b -> Fix f a -> b
  foldr f base (MkFix faf) = foldr (\a b -> foldr f b a) base faf
  foldMap :: (Foldable f, Monoid m) => (a -> m) -> Fix f a -> m
  foldMap f (MkFix faf) = foldMap (foldMap f) faf

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldr :: (Foldable f, Foldable g) => (a -> b -> b) -> b -> Compose f g a -> b
  foldr f base (MkCompose fga) = foldr (\ga b -> foldr f b ga) base fga
  foldMap f (MkCompose fga) = foldMap (foldMap f) fga

instance Foldable f => Foldable (Sum f fixed) where

instance Foldable f => Foldable (Prod f fixed) where

instance Foldable f => Foldable (FList f) where
  foldr :: Foldable f => (a -> b -> b) -> b -> FList f a -> b
  foldr f base FNil           = base
  foldr f base (FCons fb fbs) = foldr f (foldr (\l b -> foldr f b l) base fbs) fb
  foldMap :: (Foldable f, Monoid m) => (a -> m) -> FList f a -> m
  foldMap _ FNil           = mempty
  foldMap f (FCons fb fbs) = foldMap f fb <> foldMap (foldMap f) fbs


{-

Félcsoport: Olyan H halmaz, amely rendelkezik egy <> asszociatív művelettel
Ez Haskellben a Semigroup típusosztály

Írjunk Semigroup instance-ot az alábbi típusokra!

-}

instance Semigroup Bool where
  (<>) :: Bool -> Bool -> Bool
  (<>) = (&&)

instance Semigroup Int where
  (<>) :: Int -> Int -> Int
  (<>) = (*)

-- >>> import Data.Monoid
-- >>> :t Sum True
-- Sum True :: Sum Bool

data Endo a = MkEndo (a -> a)

instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (<>) (MkEndo f) (MkEndo g) = MkEndo (f . g)

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
  mempty = True

instance Monoid Int where
  mempty :: Int
  mempty = 1

instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = MkEndo id


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
