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
	(<>) (Addition a) (Addition b) = Addition $ a + b

instance Num a => Monoid (Addition a) where
	mempty = Addition 0

instance Num a => Semigroup (Multiplication a) where
	(<>) (Multiplication a) (Multiplication b) = Multiplication (a * b)

instance Num a => Monoid (Multiplication a) where
	mempty = Multiplication 1

instance Semigroup [a] where
	(<>) = (++)

instance Monoid [a] where
	mempty = []

instance Semigroup a => Semigroup (Maybe a) where
	(<>) (Just a) (Just b) = Just $ a <> b
	(<>) Nothing Nothing = Nothing
	(<>) (Just a) Nothing = Just a
	(<>) Nothing (Just b) = Just b

instance Semigroup a => Monoid (Maybe a) where
	mempty = Nothing

instance Semigroup (HTree a) where
	-- Technikailag nem asszociatív, de hasznos!
	(<>) = HNode

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
functorFoldMap f = fold . fmap f

instance Foldable [] where
	fold :: Monoid a => [a] -> a
	fold = foldr (<>) mempty

	foldMap :: Monoid b => (a -> b) -> [a] -> b
	foldMap = functorFoldMap

	foldr :: (a -> b -> b) -> b -> [a] -> b
	foldr f z [] = z
	foldr f z (x:xs) = f x $ foldr f z xs

	foldl :: (b -> a -> b) -> b -> [a] -> b
	foldl f a [] = a
	foldl f a (x:xs) = foldl f (f a x) xs

toList :: Foldable t => t a -> [a]
toList = foldr (:) []

instance Foldable HTree where
	fold :: Monoid a => HTree a -> a
	fold (HLeaf a) = a
	fold (HNode a b) = fold a <> fold b

	foldMap :: Monoid b => (a -> b) -> HTree a -> b
	foldMap = functorFoldMap

	foldr :: (a -> b -> b) -> b -> HTree a -> b
	foldr f z (HLeaf a) = f a z
	foldr f z (HNode a b) = foldr f (foldr f z b) a

	foldl :: (b -> a -> b) -> b -> HTree a -> b
	foldl f z (HLeaf a) = f z a
	foldl f z (HNode a b) = foldl f (foldl f z a) b

toHTree :: Foldable t => t a -> Maybe (HTree a)
toHTree = foldMap (Just . HLeaf)

instance Foldable STree where
	fold :: Monoid a => STree a -> a
	fold SLeaf = mempty
	fold (SNode a x b) = fold a <> x <> fold b

	foldMap :: Monoid b => (a -> b) -> STree a -> b
	foldMap = functorFoldMap

	foldr :: (a -> b -> b) -> b -> STree a -> b
	foldr f z SLeaf = z
	foldr f z (SNode a x b) = foldr f (f x (foldr f z b)) a

	foldl :: (b -> a -> b) -> b -> STree a -> b
	foldl f z SLeaf = z
	foldl f z (SNode a x b) = foldl f (f (foldl f z a) x) b

instance Foldable Maybe where
	fold Nothing = mempty
	fold (Just a) = a
	foldMap = functorFoldMap
	foldr f z Nothing = z
	foldr f z (Just a) = f a z
	foldl f z Nothing = z
	foldl f z (Just a) = f z a

-- Foldable függvények
--------------------------------------------------------------------------------

-- Vizsgáljuk meg hogy üres-e.
null :: (Foldable t) => t a -> Bool
null = foldr (\x y -> False) True

-- Számoljuk meg az elemeket.
length :: (Foldable t) => t a -> Integer
length = foldr (\x y -> y + 1) 0

-- Tartalmazza-e az elemet?
elem :: (Foldable t,Eq a) => a -> t a -> Bool
elem x = foldr (\y z -> x == y || z) False

min' :: (Ord a) => Maybe a -> Maybe a -> Maybe a
min' Nothing a = a
min' a Nothing = a
min' (Just a) (Just b) = Just $ min a b

minimum :: (Foldable t,Ord a) => t a -> Maybe a
minimum = foldr (\x y -> min' (Just x) y) Nothing

maximum :: (Foldable t,Ord a) => t a -> Maybe a
maximum = foldr (\x y -> max (Just x) y) Nothing

-- Használjuk a foldr vagy foldl függvényt!
sum :: (Foldable t,Num a) => t a -> a
sum = foldr (+) 0

product :: (Foldable t,Num a) => t a -> a
product = foldr (*) 1

-- Használjuk a fenti Addition és Multiplication típusokat!
sum' :: (Foldable t,Num a) => t a -> a
sum' = fromAddition . foldMap Addition

product' :: (Foldable t,Num a) => t a -> a
product' = fromMultiplication . foldMap Multiplication

-- Szorzat és összeg
-- Példa: sumAndProduct [2,2,3] = (7,12)
sumAndProduct :: (Foldable t,Num a) => t a -> (a,a)
sumAndProduct x = (sum x,product x)

-- Bónusz: Szorzatok összege
-- Példa: sumOfProducts [[2,5],[3,7]] == 31
sumOfProducts :: (Foldable t1,Foldable t2,Num a) => t1 (t2 a) -> a
sumOfProducts = fromAddition . foldMap
	(Addition . fromMultiplication . foldMap Multiplication)

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
	fmap f Nil = Nil
	fmap f (Skip x) = Skip $ fmap f x
	fmap f (Cons a x) = Cons (f a) (fmap f x)

instance (Functor f,Functor g) => Functor (Sum f g) where
	fmap f (Inl a) = Inl (fmap f a)
	fmap f (Inr a) = Inr (fmap f a)

instance (Functor f,Functor g) => Functor (Product f g) where
	fmap f (Product a b) = Product (fmap f a) (fmap f b)

instance (Functor f,Functor g) => Functor (Compose f g) where
	fmap f (Compose a) = Compose (fmap (fmap f) a)

instance Functor RoseTree where
	-- Ez is ismerős...
	fmap f (RoseNode a b) = RoseNode (f a) (fmap (fmap f) b)

instance Functor Pit where
	fmap f PNil = PNil
	fmap f (PLeft a b) = PLeft (f a) (fmap f b)
	fmap f (PRight a b) = PRight (fmap f a) (f b)

instance (Semigroup (f a),Semigroup (g a)) => Semigroup (Product f g a) where
	(<>) (Product a b) (Product c d) = Product (a <> c) (b <> d)

instance (Monoid (f a),Monoid (g a)) => Monoid (Product f g a) where
	mempty = Product mempty mempty

instance Foldable SparseList where
	fold Nil = mempty
	fold (Skip s) = fold s
	fold (Cons a b) = a <> fold b

	foldMap = functorFoldMap

	foldr f z Nil = z
	foldr f z (Skip s) = foldr f z s
	foldr f z (Cons a b) = f a (foldr f z b)

	foldl f z Nil = z
	foldl f z (Skip s) = foldl f z s
	foldl f z (Cons a b) = foldl f (f z a) b

instance (Foldable f,Foldable g) => Foldable (Sum f g) where
	fold (Inl a) = fold a
	fold (Inr a) = fold a

	foldMap f (Inl a) = foldMap f a
	foldMap f (Inr a) = foldMap f a

	foldr f z (Inl a) = foldr f z a
	foldr f z (Inr a) = foldr f z a

	foldl f z (Inl a) = foldl f z a
	foldl f z (Inr a) = foldl f z a

instance (Foldable f,Foldable g) => Foldable (Product f g) where
	fold (Product a b) = fold a <> fold b

	foldMap f (Product a b) = foldMap f a <> foldMap f b

	foldr f z (Product a b) = foldr f (foldr f z b) a

	foldl f z (Product a b) = foldl f (foldl f z a) b

instance (Foldable f,Foldable g) => Foldable (Compose f g) where
	fold (Compose a) = foldMap fold a

	foldMap f (Compose a) = foldMap (foldMap f) a

	foldr f z (Compose a) = foldr (\x acc -> foldr f acc x) z a

	foldl f z (Compose a) = foldl (\acc x -> foldl f acc x) z a

instance Foldable RoseTree where
	-- Preorder bejárás.
	fold (RoseNode a b) = a <> foldMap (fold) b

	foldMap f (RoseNode a b) = f a <> foldMap (foldMap f) b

	foldr f z (RoseNode a b) = f a $ foldr (\x acc -> foldr f acc x) z b

	foldl f z (RoseNode a b) = foldl (\acc x -> foldl f acc x) (f z a) b

instance Foldable Pit where

	fold PNil = mempty
	fold (PLeft a b) = a <> fold b
	fold (PRight a b) = fold a <> b

	foldMap = functorFoldMap

	foldr f z PNil = z
	foldr f z (PLeft a b) = f a (foldr f z b)
	foldr f z (PRight a b) = foldr f (f b z) a

	foldl f z PNil = z
	foldl f z (PLeft a b) = foldl f (f z a) b
	foldl f z (PRight a b) = f (foldl f z a) b
