{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

import Prelude hiding (Eq(..), Show(..), Ord(..), Semigroup(..), Monoid(..))


-- Definiáld a következő függvényeket!
------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)

-- Számold meg a leveleket.
numLeaves :: Tree a -> Int
numLeaves = undefined

-- Alkalmazz egy függvényt az összes tárolt értékre
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = undefined

-- Összegezd a fában tárolt értékeket.
-- A "Num" osztály metódusa található a "(+)" művelet.
sumTree :: Num a => Tree a -> a
sumTree = undefined

-- Add vissza az elemek listáját.
-- Figyelem: a legegyszerűbb definíció nem hatékony (kvadratikus)!
-- Próbálj a fa méretében lineáris komplexitású definíciót adni.
treeToList :: Tree a -> [a]
treeToList = undefined

-- Definiálj egy "NonEmptyList a" típust ADT-ként, aminek az értékei nemüres
-- listák.
data NonEmptyList a = TODO -- add meg a konstruktorokat!

-- konvertálás listává
toList :: NonEmptyList a -> [a]
toList = undefined

-- Konvertálj listát nemüres listává! Ha a bemenet üres, legyen Nothing az eredmény.
fromList :: [a] -> Maybe (NonEmptyList a)
fromList = undefined


-- Osztályok
------------------------------------------------------------

data Color = Red | Green | Blue

class Eq a where
  (==) :: a -> a -> Bool
  infix 4 ==

class Eq a => Ord a where
  (<) :: a -> a -> Bool
  infix 4 <

class Show a where
  show :: a -> String

-- írd meg a következő instance-okat
instance Eq Color where
  (==) = undefined

instance Ord Color where
  (<) = undefined

instance Show Color where
  show = undefined

instance Eq a => Eq (Maybe a) where
  (==) = undefined

instance Ord a => Ord (Maybe a) where
  (<) = undefined

instance Show a => Show (Maybe a) where
  show = undefined

instance Eq a => Eq [a] where
  (==) = undefined

instance Ord a => Ord [a] where
  (<) = undefined

instance Show a => Show [a] where
  show = undefined

instance Eq a => Eq (Tree a) where
  (==) = undefined

instance Ord a => Ord (Tree a) where
  (<) = undefined

instance Show a => Show (Tree a) where
  -- bónusz: adj meg hatékony definíciót!
  show = undefined


-- Írd meg a következő instance-okat úgy, hogy megfeleljenek
-- az osztály törvényeknek!
------------------------------------------------------------

class Semigroup a where
  (<>) :: a -> a -> a   -- asszociatív művelet
  infixr 6 <>

class Semigroup a => Monoid a where
  mempty :: a  -- egységeleme <>-nek

instance Semigroup [a] where
  (<>) :: [a] -> [a] -> [a]
  (<>) = undefined

instance Monoid [a] where
  mempty :: [a]
  mempty = undefined

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (<>) :: (a, b) -> (a, b) -> (a, b)
  (<>) = undefined

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty :: (a, b)
  mempty = undefined

instance Semigroup b => Semigroup (a -> b) where
  (<>) :: (a -> b) -> (a -> b) -> (a -> b)
  (<>) = undefined

instance Monoid b => Monoid (a -> b) where
  mempty :: a -> b
  mempty = undefined
