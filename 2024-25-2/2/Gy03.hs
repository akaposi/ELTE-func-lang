{-# LANGUAGE InstanceSigs, QuantifiedConstraints, StandaloneDeriving #-}
module Gy03_pre where

import Prelude hiding (Maybe(..), Either(..))

-- Írjuk meg azt a függvényt ami összeszorozza a [Int] elemeit

multiply :: [Int] -> Int
multiply []     = 1
multiply (x:xs) = x * multiply xs


multiply' :: [Int] -> Int
multiply' xs = foldr (*) 1 xs


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
foldrSingle f b (Single a) = f a b

foldrTuple :: (a -> b -> b) -> b -> Tuple a -> b
foldrTuple f b (Tuple a a') = f a (f a' b)

{-

f a (f a' b)

     f
    / \
  a   f
     / \
    a'  b
-}

foldrQuintuple :: (a -> b -> b) -> b -> Quintuple a -> b
foldrQuintuple = undefined

foldrList :: (a -> b -> b) -> b -> List a -> b
foldrList f b Nil         = b
foldrList f b (Cons x xs) = f x (foldrList f b xs)

{-
Vezessük le:
foldrList (+) 0 (Cons 1 $ Cons 2 Nil) = 
(+) 1 (foldrList (+) 0 (Cons 2 Nil))  =
(+) 1 ((+) 2 (foldrList (+) 0 Nil))   =
(+) 1 ((+) 2 (0))   =

     (+)
    /  \
   1   (+)
      /  \ 
     2    0
-}

foldlList :: (b -> a -> b) -> b -> List a -> b
foldlList f b Nil         = b
foldlList f b (Cons x xs) = foldlList f (f b x) xs
 

{-
Vezessük le ezt is:
foldlList (+) 0 (Cons 1 $ Cons 2 Nil) =
foldlList (+) ((+) 0 1) (Cons 2 Nil)  =
foldlList (+) ((+) ((+) 0 1) 2) (Nil)  =
((+) ((+) 0 1) 2)  =

    (+)
   /   \
  (+)   2
 /   \
0    1

-}

-- Teszt : 
-- foldr (^) 1 [2,1,3] == 2 == 
-- 
-- foldl (^) 1 [2,1,3] == 1 == 
-- 

foldrMaybe :: (a -> b -> b) -> b -> Maybe a -> b
foldrMaybe f b Nothing = b
foldrMaybe f b (Just a) = f a b


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

-- foldMap f [a,b,c,d...] = f a <> f b <> f c <> f d <> ... <> mempty


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

{-
data NonEmpty a = Last a | NECons a (NonEmpty a) deriving (Eq, Show)
data NonEmpty2 a = NECons2 a (List a) deriving (Eq, Show)
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)
-}

instance Foldable NonEmpty where
  foldr f b (Last a) = f a b
  foldr f b (NECons a ne) = f a (foldr f b ne)
  


instance Foldable NonEmpty2 where
  foldr f b (NECons2 a ne) = f a (foldr f b ne)
--                               \-----------/
--                                     ^- itt listákra foldr-elünk
--  ne :: List a

instance Foldable Tree where
  foldr f b (Leaf a) = f a b
  foldr f b (Node l a r) = foldr f (f a (foldr f b r)) l

--                        V
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

instance Foldable (Either fixed) where

instance Foldable (BiTuple fixed) where

instance Foldable (TriEither fixed1 fixed2) where

instance Foldable (BiList fixed) where

-- Magasabbrendű megkötések

--data Apply f a = MkApply (f a) deriving (Eq, Show)
instance Foldable f => Foldable (Apply f) where
  foldr f b (MkApply fa) = (foldr f b fa)

-- data Fix f a = MkFix (f (Fix f a))
instance Foldable f => Foldable (Fix f) where
  foldr f b (MkFix fFixfa) = foldr (\fixfa b' -> foldr f b' fixfa) b fFixfa
--                                   flip (foldr f)

-- Eddig jutottunk

instance (Foldable f, Foldable g) => Foldable (Compose f g) where

instance Foldable f => Foldable (Sum f fixed) where

instance Foldable f => Foldable (Prod f fixed) where

instance Foldable f => Foldable (FList f) where



data T a
    = C1
    | C2 Int a
    | C3 a a
    | C4 a (T a)
    deriving Functor

-- Írjunk Foldable instance-t a következő T típusra

instance Foldable T where
    foldr :: (a -> b -> b) -> b -> T a -> b
    foldr f b C1 = b
    foldr f b (C2 int a) = f a b
    foldr f b (C3 a1 a2) = f a1 (f a2 b)
    foldr f b (C4 a ta) = f a (foldr f b ta)


{-

Félcsoport: Olyan H halmaz, amely rendelkezik egy <> asszociatív művelettel
Ez Haskellben a Semigroup típusosztály

https://en.wikipedia.org/wiki/Magma_(algebra)#/media/File:Magma_to_group4.svg

Írjunk Semigroup instance-ot az alábbi típusokra!

Asszociativ: a <> (b <> c) = (a <> b) <> c

-}



instance Semigroup Bool where
  (<>) :: Bool -> Bool -> Bool
  (<>) = (&&) -- (||)

instance Semigroup Int where
  (<>) :: Int -> Int -> Int
  (<>) = (+) -- (*)


data Endo a = MkEndo (a -> a)

instance Semigroup (Endo a) where
  -- (a -> a) -> (a -> a) -> a -> a
  (<>) :: Endo a -> Endo a -> Endo a
  MkEndo f <> MkEndo g = MkEndo (\a -> f (g a))


{-
Egy halmazhoz több művelet is választható, hogy félcsoportot alkossanak
Pl.:
(ℝ, +)         egy félcsoport
(ℝ, *)         egy félcsoport
(A, λx y -> x) egy félcsoport
(A, λx y -> y) egy félcsoport
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
data RoseTree a = RoseLeaf a | RoseNode [RoseTree a] deriving (Eq, Show)
data Tree3 a = Leaf3 a | Node3 (Tree3 a) (Tree3 a) deriving (Eq, Show)
data SkipList a = Skip (SkipList a) | SCons a (SkipList a) | SNill deriving (Eq, Show)
-- data CrazyType a = C1 a a | C2 a Int | C3 (CrazyType a) deriving (Eq, Show)
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
