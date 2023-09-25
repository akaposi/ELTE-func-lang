{-# options_ghc -Wincomplete-patterns -Wno-tabs #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor #-}

import Prelude(undefined, id, (++), map, const, (.), flip, ($), otherwise)
import Data.Either
import Data.Maybe
import Data.Functor
import Data.Bool
import GHC.Show
import GHC.Num
import GHC.Classes

infixr 6 <>

data HTree a = HLeaf a | HNode (HTree a) (HTree a) deriving (Eq, Ord, Functor)
data STree a = SLeaf | SNode (STree a) a (STree a) deriving (Eq, Ord, Functor)

e1 :: HTree Integer
e1 = HNode (HNode (HLeaf 1) (HLeaf 5)) (HNode (HNode (HLeaf 3) (HLeaf 2)) (HLeaf 4))

instance Show a => Show (HTree a) where
	show (HLeaf x) = show x
	show (HNode a b) = hshow a ++ " <> " ++ hshow b where
		hshow (HNode a b) = "(" ++ show (HNode a b) ++ ")"
		hshow (HLeaf x) = show (HLeaf x)

instance Show a => Show (STree a) where
	show (SLeaf) = "Leaf"
	show (SNode a b c) = hshow a ++ " " ++ show b ++ " " ++ hshow c where
		hshow (SNode a b c) = "(" ++ show (SNode a b c) ++ ")";
		hshow (SLeaf) = ""

-- Semigroup & Monoid
--------------------------------------------------------------------------------

class Semigroup t where
	(<>) :: t -> t -> t

-- Asszociativitás (a <> b) <> c == a <> (b <> c)

class Semigroup t => Monoid t where
	mempty :: t

-- Nulla tulajdonság: mempty <> a == a && a <> mempty == a

newtype Addition a = Addition {fromAddition :: a}
newtype Multiplication a = Multiplication {fromMultiplication :: a}

instance Num a => Semigroup (Addition a) where
	(<>) = undefined

instance Num a => Monoid (Addition a) where
	mempty = undefined

instance Num a => Semigroup (Multiplication a) where
	(<>) = undefined

instance Num a => Monoid (Multiplication a) where
	mempty = undefined

instance Semigroup [a] where
	(<>) = undefined

instance Monoid [a] where
	mempty = undefined

instance Semigroup a => Semigroup (Maybe a) where
	(<>) = undefined

instance Semigroup a => Monoid (Maybe a) where
	mempty = undefined

instance Semigroup (HTree a) where
	-- Technikailag nem asszociatív, de hasznos!
	(<>) = undefined

-- Foldable
--------------------------------------------------------------------------------

class Foldable t where
	foldMap :: Monoid b => (a -> b) -> t a -> b
	foldMap f = foldr ((<>) . f) mempty

	fold :: Monoid a => t a -> a
	fold = foldMap id

	foldr :: (a -> b -> b) -> b -> t a -> b
	foldl :: (b -> a -> b) -> b -> t a -> b

-- foldr jobb asszociatív
-- foldr f z [x1,x2,x3,x4,x5] = x1 `f` (x2 `f` (x3 `f` (x4 `f` (x5 `f` z))))

-- foldl bal asszociatív
-- foldl f z [x1,x2,x3,x4,x5] = ((((z `f` x1) `f` x2) `f` x3) `f` x4) `f` x5

-- A definíció szerint a fold-nak jobb asszociatívan kell alkalmaznia a <> operátort.
-- De <> úgy is definíció szerint asszociatív.

-- Implementáljuk a foldMap-et fold és fmap segítségével
functorFoldMap :: (Functor f,Foldable f,Monoid b) => (a -> b) -> f a -> b
functorFoldMap = undefined

instance Foldable [] where
	fold :: Monoid a => [a] -> a
	fold = undefined

	foldMap :: Monoid b => (a -> b) -> [a] -> b
	foldMap = undefined

	foldr :: (a -> b -> b) -> b -> [a] -> b
	foldr = undefined

	foldl :: (b -> a -> b) -> b -> [a] -> b
	foldl = undefined

toList :: Foldable t => t a -> [a]
toList = undefined

instance Foldable HTree where
	fold :: Monoid a => HTree a -> a
	fold = undefined
	
	foldMap :: Monoid b => (a -> b) -> HTree a -> b
	foldMap = undefined

	foldr :: (a -> b -> b) -> b -> HTree a -> b
	foldr = undefined

	foldl :: (b -> a -> b) -> b -> HTree a -> b
	foldl = undefined

toHTree :: Foldable t => t a -> Maybe (HTree a)
toHTree = undefined

instance Foldable STree where
	fold :: Monoid a => STree a -> a
	fold = undefined

	foldMap :: Monoid b => (a -> b) -> STree a -> b
	foldMap = undefined

	foldr :: (a -> b -> b) -> b -> STree a -> b
	foldr = undefined

	foldl :: (b -> a -> b) -> b -> STree a -> b
	foldl = undefined

instance Foldable Maybe where
	fold = undefined
	foldMap = undefined
	foldr = undefined
	foldl = undefined

-- Foldable függvények
--------------------------------------------------------------------------------

-- Vizsgáljuk meg hogy üres-e.
null :: (Foldable t) => t a -> Bool
null = undefined

-- Számoljuk meg az elemeket.
length :: (Foldable t) => t a -> Integer
length = undefined

-- Tartalmazza-e az elemet?
elem :: (Foldable t,Eq a) => a -> t a -> Bool
elem = undefined

minimum :: (Foldable t,Ord a) => t a -> Maybe a
minimum = undefined

maximum :: (Foldable t,Ord a) => t a -> Maybe a
maximum = undefined

-- Használjuk a foldr vagy foldl függvényt!
sum :: (Foldable t,Num a) => t a -> a
sum = undefined

product :: (Foldable t,Num a) => t a -> a
product = undefined

-- Használjuk a fenti Addition és Multiplication típusokat!
sum' :: (Foldable t,Num a) => t a -> a
sum' = undefined

product' :: (Foldable t,Num a) => t a -> a
product' = undefined

-- Szorzat és összeg
-- Példa: sumAndProduct [2,2,3] = (7,12)
sumAndProduct :: (Foldable t,Num a) => t a -> (a,a)
sumAndProduct = undefined

-- Bónusz: Szorzatok összege
-- Példa: sumOfProducts [[2,5],[3,7]] == 31
sumOfProducts :: (Foldable t1,Foldable t2,Num a) => t1 (t2 a) -> a
sumOfProducts = undefined

-- További feladatok
--------------------------------------------------------------------------------

data SparseList a = Nil | Skip (SparseList a) | Cons a (SparseList a)
data Sum f g a = Inl (f a) | Inr (g a) deriving Show
data Product f g a = Product (f a) (g a) deriving Show
newtype Compose f g a = Compose (f (g a)) deriving Show
data RoseTree a = RoseNode a [RoseTree a] deriving Show
data Pit a = PNil | PLeft a (Pit a) | PRight (Pit a) a

instance Functor SparseList where
	-- Ez ismerős...
	fmap = undefined

instance (Functor f,Functor g) => Functor (Sum f g) where
	fmap = undefined

instance (Functor f,Functor g) => Functor (Product f g) where
	fmap = undefined

instance (Functor f,Functor g) => Functor (Compose f g) where
	fmap = undefined

instance Functor RoseTree where
	-- Ez is ismerős...
	fmap = undefined

instance Functor Pit where
	fmap = undefined

instance (Semigroup (f a),Semigroup (g a)) => Semigroup (Product f g a) where
	(<>) = undefined

instance (Monoid (f a),Monoid (g a)) => Monoid (Product f g a) where
	mempty = undefined

instance Foldable SparseList where
	fold = undefined
	foldMap = undefined
	foldr = undefined
	foldl = undefined

instance (Foldable f,Foldable g) => Foldable (Sum f g) where
	fold = undefined
	foldMap = undefined
	foldr = undefined
	foldl = undefined

instance (Foldable f,Foldable g) => Foldable (Product f g) where
	fold = undefined
	foldMap = undefined
	foldr = undefined
	foldl = undefined

instance (Foldable f,Foldable g) => Foldable (Compose f g) where
	fold = undefined
	foldMap = undefined
	foldr = undefined
	foldl = undefined

instance Foldable RoseTree where
	-- Preorder bejárás.
	fold = undefined
	foldMap = undefined
	foldr = undefined
	foldl = undefined

instance Foldable Pit where
	fold = undefined
	foldMap = undefined
	foldr = undefined
	foldl = undefined