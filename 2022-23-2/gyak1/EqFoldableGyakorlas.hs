{-# OPTIONS_GHC -Wmissing-methods -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module EqFoldableGyakorlas where

data Single a = Single a deriving Show
data SAndI a = SAndI a Int deriving Show
data Tuple a = Tuple a a deriving Show
data BiList a = Nil | Cons a a (BiList a) deriving Show
data RoseTree a = RoseLeaf a | RoseNode [RoseTree a] deriving Show
data SkipList a = Skip (SkipList a) | SCons a (SkipList a) | SNill deriving Show

-- Mi ennek a kindja?
-- :k DualList
data DualList a b = ConsA a (DualList a b) | ConsB b (DualList a b) | DNill deriving Show

-- Mi ennek a kindja? (nehéz)
-- :k Wrap
data Wrap f a = Wrap (f a)

-- Írj a fenti típusokra Eq instance-ot!

instance Eq a => Eq (Single a) where

instance Eq a => Eq (SAndI a) where

instance Eq a => Eq (Tuple a) where

instance Eq a => Eq (BiList a) where

instance Eq a => Eq (RoseTree a) where

instance Eq a => Eq (SkipList a) where

instance (Eq a, Eq b) => Eq (DualList a b) where

-- Wrap-ot kihagyjuk

-- Írj a fenti típusokra Foldable instance-ot!

instance Foldable Single where

instance Foldable SAndI where

instance Foldable Tuple where

instance Foldable BiList where

instance Foldable RoseTree where

instance Foldable SkipList where

instance Foldable (DualList q) where -- A q-n nem hajtogatunk végig!

instance Foldable f => Foldable (Wrap f) where

-- Magasabbrendű Polimorfizmus
-- Definiálj egy tetszőleges adattípust amelynek az alábbi kindja van:

data A
--  A :: * -> *

data B
-- B :: * -> * -> *

data C
-- C :: (* -> *) -> *

-- Foldable függvények

-- Definiálj egy függvényt ami valami hajtogatható tároló legkisebb elemét visszaadja!
smallest :: (Ord a, Foldable f) => f a -> {- ha nincs eleme adja ezt vissza -} a -> a
smallest = undefined

-- Definiálj egy függvényt ami valami hajtogatható tároló első elemét visszaadja.
-- Ha a tárolóba nincs elem adjuk vissza a második paramétert
safeHead :: Foldable f => f a -> a -> a
safeHead = undefined

-- Definiálj egy függvény ami valami hajtogatható tároló összes elemét összegyűjti!
fToList :: Foldable f => f a -> [a]
fToList = undefined

-- Definiáld a composeAll függvényt az első óráról hajtogatással!
composeAll :: Foldable f => f (a -> a) -> (a -> a)
composeAll = undefined

-- Definiáld a foldMap-ot a foldr segítségével!
foldMapViaFoldr :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
foldMapViaFoldr = undefined
