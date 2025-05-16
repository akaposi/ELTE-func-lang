{-# LANGUAGE InstanceSigs, QuantifiedConstraints, StandaloneDeriving #-}
module Gy03 where

import Prelude hiding (Maybe(..), Either(..))

-- Fold példa:
-- foldr (+) 0 [1,2,3] == 1+2+3 + 0 == 6
-- foldr (+) 0 Cons 1 $ Cons 2 $ Cons 3 $ Nil
--          == plus 1 $ plus 2 $ plus 2 $   0 
--          ==      1 +      2 +      3 +   0
--
-- where plus = (+)

-- Hajtogatás: Listára való rekurzió szimulációja
-- foldr :: (a ->  b  ->  b ) ->  b -> [a] -> b
--   (:) :: (a -> [a] -> [a])
--
--                         [] :: [a]
-- A hajtogatás összekombinálja az összes 'a' típusú kifejezést egy listában.
-- Próbáljuk ezt meg más típussal is eljátszani:


-- Típusok amik voltak mult órán
data Single a = Single a deriving (Eq, Show)
data Tuple a = Tuple a a deriving (Eq, Show)
data Quintuple a = Quintuple a a a a a deriving (Eq, Show)
data List a = Nil | Cons a (List a) deriving (Eq, Show)
data Maybe a = Just a | Nothing deriving (Eq, Show)

foldrSingle :: (a -> b -> b) -> b -> Single a -> b
foldrSingle f b (Single a) = f a b

-- foldr folds from the right
foldrTuple :: (a -> b -> b) -> b -> Tuple a -> b
foldrTuple f b (Tuple a a') = f a (f a' b)


foldrQuintuple :: (a -> b -> b) -> b -> Quintuple a -> b
foldrQuintuple f b (Quintuple x xx xxx xxxx xxxxx) = f x $ f xx $ f xxx $ f xxxx b

foldrList :: (a -> b -> b) -> b -> List a -> b
foldrList f b Nil = b
foldrList f b (Cons a xs) = f a (foldrList f b xs)   -- f a b típus helyes, viszont nem hajtogattuk az egész listát
--                              \----------------/
--                                      : b

foldlList :: (b -> a -> b) -> b -> List a -> b
foldlList f b Nil = b
foldlList f b (Cons a xs) = f (foldlList f b xs) a

{-
foldrList (+) 0 (Cons 1 $ Cons 2 Nil) 
== (+) 1 (foldrList (+) 0 (Cons 2 Nil)) 
== (+) 1 ((+) 2 (foldrList (+) 0 Nil))
== (+) 1 ((+) 2 (0))

  +
 / \
1  +
  / \
  2 0

    +
   / \
   + 1
  / \
  2 0

-}

foldrMaybe :: (a -> b -> b) -> b -> Maybe a -> b
foldrMaybe f b Nothing = b       -- itt f-et nem tudjuk használni, csak b-t
foldrMaybe f b (Just a) = f a b

-- Példa
maybeFolding = foldr (+) 0 (Just 2) -- == 2
maybeFolding' = foldr (+) 0 Nothing -- == 0

-- foldl, foldMap == foldr . map

-- Hasonlóan a mappolhatósághoz, a hajtogatás is általánosítható a Foldableue típusosztály segítéségvel
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

data NonEmpty a = Last a | NECons a (NonEmpty a) deriving (Eq, Show)

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f b (Last a)      = f a b
  foldr f b (NECons a ns) = f a (foldr f b ns)

data NonEmpty2 a = NECons2 a (List a) deriving (Eq, Show)

instance Foldable NonEmpty2 where
  foldr :: (a -> b -> b) -> b -> NonEmpty2 a -> b
  foldr f b (NECons2 a ns) = f a (foldr f b ns)

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f b (Leaf a) = f a b
  foldr f b (Node l a r) = foldr f (f a (foldr f b r)) l

data Either e a = Left e | Right a deriving (Eq, Show)
instance Foldable (Either fixed) where
  -- Csak a jobboldali értéket lehet hajtogatni
  foldr :: (a -> b -> b) -> b -> Either fixed a -> b
  foldr f b (Left fixed) = b
  foldr f b (Right a) = f a b

data BiTuple e a = BiTuple e a deriving (Eq, Show)
instance Foldable (BiTuple fixed) where
  foldr :: (a -> b -> b) -> b -> BiTuple fixed a -> b
  foldr f b (BiTuple fx a) = (f a b)

data TriEither e1 e2 a = LeftT e1 | MiddleT e2 | RightT a deriving (Eq, Show)
instance Foldable (TriEither fixed1 fixed2) where
  foldr :: (a -> b -> b) -> b -> TriEither fixed1 fixed2 a -> b
  foldr f b (LeftT fx)   = b
  foldr f b (MiddleT fx) = b
  foldr f b (RightT a)   = f a b

data BiList a b = ACons a (BiList a b) | BCons b (BiList a b) | ABNill deriving (Eq, Show)
instance Foldable (BiList fixed) where
  foldr :: (a -> b -> b) -> b -> BiList fixed a -> b
  foldr f b (ACons fx ls) = (foldr f b ls)
  foldr f b (BCons a ls)  = f a (foldr f b ls)
  foldr f b (ABNill)      = b

data Apply f a = MkApply (f a) deriving (Eq, Show)
-- Magasabbrendű megkötések
instance Foldable f => Foldable (Apply f) where
  foldr :: Foldable f => (a -> b -> b) -> b -> Apply f a -> b
  foldr f b (MkApply fa) = (foldr f b fa)

data Fix f a = MkFix (f (Fix f a))
instance Foldable f => Foldable (Fix f) where
  foldr :: Foldable f => (a -> b -> b) -> b -> Fix f a -> b
  foldr f b (MkFix fFixfa)= foldr (\fixfa b' -> (foldr f b' fixfa)) b fFixfa
--                                 flip (foldr f)

data Compose f g a = MkCompose (f (g a)) deriving (Eq, Show)
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldr :: (Foldable f, Foldable g) => (a -> b -> b) -> b -> Compose f g a -> b
  foldr f b (MkCompose fga) = foldr (\ga b' -> foldr f b' ga) b fga

data Sum f a b = FLeft (f a) | FRight (f b) deriving (Eq, Show)
instance Foldable f => Foldable (Sum f fixed) where
  foldr :: Foldable f => (a -> b -> b) -> b -> Sum f fixed a -> b
  foldr f b (FLeft fx) = b
  foldr f b (FRight fa) = foldr f b fa


data Prod f a b = FProd (f a) (f b) deriving (Eq, Show)
instance Foldable f => Foldable (Prod f fixed) where
  foldr :: Foldable f => (a -> b -> b) -> b -> Prod f fixed a -> b
  foldr f b (FProd fx fa) = foldr f b fa

data FList f a = FNil | FCons (f a) (f (FList f a))
instance Foldable f => Foldable (FList f) where
  foldr :: Foldable f => (a -> b -> b) -> b -> FList f a -> b
  foldr f b FNil = b
  foldr f b (FCons fa fFlistfa) = foldr f (foldr (flip (foldr f)) b fFlistfa) fa


{-

Félcsoport: Olyan H halmaz, amely rendelkezik egy <> asszociatív művelettel
Ez Haskellben a Semigroup típusosztály

Írjunk Semigroup instance-ot az alábbi típusokra!

-}

instance Semigroup Bool where
  (<>) :: Bool -> Bool -> Bool
  (<>) = (||)

instance Semigroup Int where
  (<>) :: Int -> Int -> Int
  (<>) = (+)

data Endo a = MkEndo (a -> a)

instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (MkEndo f) <> (MkEndo g) = MkEndo (g . f)

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
  mempty = 0
{-
f . id = f -> \x -> f (id x) = f -> \x -> f x = f [QED]
id . f = f -> \x -> id (f x) = f -> \x -> f x = f [QED]
-}
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
instance Foldable (Tree2) where
  foldr :: (a -> b -> b) -> b -> Tree2 a -> b
  foldr f b Leaf2 = b
  foldr f b (Node2 l a r) = foldr f (f a (foldr f b r)) l

data RoseTree a = RoseLeaf a | RoseNode [RoseTree a] deriving (Eq, Show)
instance Foldable (RoseTree) where
  foldr :: (a -> b -> b) -> b -> RoseTree a -> b
  foldr f b (RoseLeaf a) = f a b
  foldr f b (RoseNode rs) = foldr (flip (foldr f)) b rs

data Tree3 a = Leaf3 a | Node3 (Tree3 a) (Tree3 a) deriving (Eq, Show)
instance Foldable (Tree3) where
  foldr :: (a -> b -> b) -> b -> Tree3 a -> b
  foldr f b (Leaf3 a) = f a b
  foldr f b (Node3 l r) = foldr f (foldr f b r) l

data SkipList a = Skip (SkipList a) | SCons a (SkipList a) | SNill deriving (Eq, Show)
instance Foldable (SkipList) where
  foldr :: (a -> b -> b) -> b -> SkipList a -> b
  foldr f b (Skip ss) = foldr f b ss
  foldr f b (SCons a ss) = f a (foldr f b ss)
  foldr f b (SNill) = b

data CrazyType a = C1 a a | C2 a Int | C3 (CrazyType a) deriving (Eq, Show)
instance Foldable (CrazyType) where
  foldr :: (a -> b -> b) -> b -> CrazyType a -> b
  foldr f b (C1 a a') = f a (f a' b) 
  foldr f b (C2 a i) = f a b
  foldr f b (C3 ct)  = foldr f b ct


data Either3 a b c = Left3 a | Middle3 b | Right3 c deriving (Eq, Show)
instance Foldable (Either3 fixed fixed') where
  foldr :: (a -> b -> b) -> b -> Either3 fixed fixed' a -> b
  foldr f b (Left3 fx) = b 
  foldr f b (Middle3 fx) = b
  foldr f b (Right3 a) = f a b

data Triplet a b c = Triplet a b c deriving (Eq, Show)
instance Foldable (Triplet fixed fixed') where
  foldr :: (a -> b -> b) -> b -> Triplet fixed fixed' a -> b
  foldr f b (Triplet fx fx' a) = f a b

data SplitTree a b = SplitTree (Tree a) a b (Tree b) deriving (Eq, Show)
instance Foldable (SplitTree fixed) where
  foldr :: (a -> b -> b) -> b -> SplitTree fixed a -> b
  foldr f b (SplitTree fxs fx a r) = f a (foldr f b r) 

data TriCompose f g h a = TriCompose (f (g (h a))) deriving (Eq, Show)
instance (Foldable f, Foldable g, Foldable h) => Foldable (TriCompose f g h) where
  foldr :: (Foldable f, Foldable g, Foldable h) => (a -> b -> b) -> b -> TriCompose f g h a -> b
  foldr f b (TriCompose fgha) = foldr (\gha b' -> foldr (\ha b'' -> foldr f b'' ha) b' gha) b fgha


data Free f a = Pure a | Free (f (Free f a))
instance Foldable f => Foldable (Free f) where
  foldr :: Foldable f => (a -> b -> b) -> b -> Free f a -> b
  foldr f b (Pure a) = f a b
  foldr f b (Free fFreefa) = foldr (\freefa b' -> (foldr f b' freefa)) b fFreefa

-- Mágia, ignore me
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (Fix f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (Fix f a)
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (FList f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (FList f a)
