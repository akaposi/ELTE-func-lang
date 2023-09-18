{-# options_ghc -Wincomplete-patterns -Wno-tabs #-}
{-# LANGUAGE InstanceSigs #-}
module Gy02 where

import Prelude hiding (Eq,Ord,Functor,(==),(/=),(>=),(<=),(>),(<),max,min,fmap,(<$>))
import qualified Prelude

infixl 1 <$>
infix 4 ==, /=, >=, <=, >, <

-- Típus osztályok
--------------------------------------------------------------------------------

class Eq a where
	(==) :: a -> a -> Bool
	(/=) :: a -> a -> Bool

	(/=) x y = not (x == y)
	{-# MINIMAL (==) #-}

class Eq a => Ord a where
	(<=) :: a -> a -> Bool
	(>=) :: a -> a -> Bool
	(<) :: a -> a -> Bool
	(>) :: a -> a -> Bool
	
	(>=) x y = y <= x
	(<) x y = not (x >= y)
	(>) x y = not (x <= y)
	{-# MINIMAL (<=) #-}

-- Hasznos szintaxis: if _ then _ else _

max :: Ord a => a -> a -> a
max = undefined

min :: Ord a => a -> a -> a
min = undefined

-- Emlékeztető: Típus osztály használata
data Color = Red | Green | Blue

instance Eq Color where
	(==) Red Red = True
	(==) Green Green = True
	(==) Blue Blue = True
	(==) _ _ = True

-- A továbbiakhoz definiálok néhány instance-t.
instance Eq Integer where
	(==) = (Prelude.==)

instance Ord Integer where
	(<=) = (Prelude.<=)

instance Eq Char where
	(==) = (Prelude.==)

instance Ord Char where
	(<=) = (Prelude.<=)

instance Eq a => Eq [a] where
	(==) [] [] = True
	(==) (x:xs) (y:ys) = (x == y) && (xs == ys)
	(==) _ _ = False

instance Ord a => Ord [a] where
	(<=) [] _ = True
	(<=) (x:xs) (y:ys) = case x == y of
		True -> xs <= ys
		False -> x <= y
	(<=) (x:xs) [] = False


-- Fás feladatok
--------------------------------------------------------------------------------

-- Huffman Tree: A levél csúcsok tárolnak értékeket.
data HTree a = HLeaf a | HNode (HTree a) (HTree a)

-- Tetszőleges
t1 :: HTree Integer
t1 = undefined

-- Legyen legalább 3 HLeaf benne!
t2 :: HTree Integer
t2 = undefined

-- Számold meg a leveleket.
numLeaves :: HTree a -> Integer
numLeaves = undefined

-- Fa magassága
-- Példa: depthHTree (HLeaf 5) == 1
-- Példa: depthHTree (HNode (HLeaf 1) (HLeaf 2)) == 2
depthHTree :: HTree a -> Integer
depthHTree = undefined

-- Összegezd a fában tárolt értékeket.
-- Segítség, GHCI-ben: ":i Num"
sumHTree :: Num a => HTree a -> a
sumHTree = undefined

e1 :: HTree Integer
e1 = HNode (HNode (HLeaf 5) (HLeaf 3)) (HNode (HLeaf 6) (HLeaf 4))

-- Ord: Keresd meg a legnagyobb értéket a fában.
maxInHTree :: Ord a => HTree a -> a
maxInHTree = undefined

-- Search Tree: A belső csúcsok tárolnak értékeket.
data STree a = SLeaf | SNode (STree a) a (STree a)

-- Tetszőleges
t3 :: STree Integer
t3 = undefined

-- Legyen legalább 1 SNode benne!
t4 :: STree Integer
t4 = undefined

-- Összegezd a fában tárolt értékeket.
sumSTree :: Num a => STree a -> a
sumSTree = undefined

instance Show a => Show (HTree a) where
	show = undefined

instance Eq a => Eq (HTree a) where
	(==) = undefined

instance Ord a => Ord (HTree a) where
	(<=) = undefined

instance Show a => Show (STree a) where
	show = undefined

instance Eq a => Eq (STree a) where
	(==) = undefined

instance Ord a => Ord (STree a) where
	(<=) = undefined

-- Alkalmazz egy függvényt az összes tárolt értékre
-- Példa: mapHTree (+5) (HLeaf 5) == (HLeaf 10)
mapHTree :: (a -> b) -> HTree a -> HTree b
mapHTree = undefined

mapSTree :: (a -> b) -> STree a -> STree b
mapSTree = undefined

-- Functor
--------------------------------------------------------------------------------

-- "Szerkezetet" megőrző morfizmus
class Functor f where
	fmap :: (a -> b) -> f a -> f b

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

instance Functor [] where
	fmap :: (a -> b) -> [a] -> [b]
	fmap = undefined

instance Functor Maybe where
	fmap :: (a -> b) -> Maybe a -> Maybe b
	fmap = undefined

instance Functor HTree where
	fmap :: (a -> b) -> HTree a -> HTree b
	fmap = undefined

instance Functor STree where
	fmap :: (a -> b) -> STree a -> STree b
	fmap = undefined

-- További adattípusok
--------------------------------------------------------------------------------

data Triplet a = T a a a deriving Show
data Quad a    = Q a a a a deriving Show
data Tagged a  = Tag String a deriving Show
data Void a    = Void deriving Show
data Try a     = Success a | Fail String

e2 :: Triplet Integer
e2 = T 5 6 7

e3 :: Quad Bool
e3 = Q True False True False

e4 :: Tagged Color
e4 = Tag "Sky" Blue

e5 :: Void Integer
e5 = Void

e6 :: Try Integer
e6 = Success 5

e7 :: Try Integer
e7 = Fail "Number not found"

-- Mi a típusa az fmap függvényeknek?

instance Functor Triplet where
	fmap = undefined

instance Functor Quad where
	fmap = undefined

instance Functor Tagged where
	fmap = undefined

instance Functor Void where
	fmap = undefined

-- Bónusz feladatok
--------------------------------------------------------------------------------

data IFun a     = IFun (Integer -> a)
data Fun a b    = Fun (a -> b)
data RoseTree a = RoseNode a [RoseTree a] deriving Show
data TreeI i a  = LeafI a | NodeI (i -> TreeI i a)
data Const a b  = Const a

e8 :: IFun String
e8 = IFun show

e9 :: Fun Color Bool
e9 = Fun (Green ==)

instance Functor IFun where
	fmap = undefined

instance Functor (Fun a) where
	fmap = undefined

instance Functor RoseTree where
	fmap = undefined

instance Functor (TreeI i) where
	fmap = undefined

instance Functor (Const i) where
	fmap = undefined
