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
max x y = if x <= y then y else x

min :: Ord a => a -> a -> a
min x y = if x <= y then x else y

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
t1 = HLeaf 3

-- Legyen legalább 3 HLeaf benne!
t2 :: HTree Integer
t2 = HNode (HNode (HLeaf 3) (HLeaf 5)) (HLeaf 13)

-- Számold meg a leveleket.
numLeaves :: HTree a -> Integer
numLeaves (HLeaf _) = 1
numLeaves (HNode a b) = numLeaves a + numLeaves b

-- Fa magassága
-- Példa: depthHTree (HLeaf 5) == 1
-- Példa: depthHTree (HNode (HLeaf 1) (HLeaf 2)) == 2
depthHTree :: HTree a -> Integer
depthHTree (HNode a b) = 1 + max (depthHTree a) (depthHTree b)
depthHTree (HLeaf _) = 1

-- Összegezd a fában tárolt értékeket.
-- Segítség, GHCI-ben: ":i Num"
sumHTree :: Num a => HTree a -> a
sumHTree (HLeaf k) = k
sumHTree (HNode a b) = sumHTree a + sumHTree b

e1 :: HTree Integer
e1 = HNode (HNode (HLeaf 5) (HLeaf 3)) (HNode (HLeaf 6) (HLeaf 4))

-- Ord: Keresd meg a legnagyobb értéket a fában.
maxInHTree :: Ord a => HTree a -> a
maxInHTree (HLeaf k) = k
maxInHTree (HNode a b) = max (maxInHTree a) (maxInHTree b)

-- Search Tree: A belső csúcsok tárolnak értékeket.
data STree a = SLeaf | SNode (STree a) a (STree a)

-- Tetszőleges
t3 :: STree Integer
t3 = SLeaf

-- Legyen legalább 1 SNode benne!
t4 :: STree Integer
t4 = SNode SLeaf 42 SLeaf

-- Összegezd a fában tárolt értékeket.
sumSTree :: Num a => STree a -> a
sumSTree (SLeaf) = 0
sumSTree (SNode a x b) = x + sumSTree a + sumSTree b

instance Show a => Show (HTree a) where
	show (HLeaf a) = "HLeaf " ++ show a
	show (HNode a b) = "HNode (" ++ show a ++ "," ++ show b ++ ")"

instance Eq a => Eq (HTree a) where
	(==) (HLeaf a) (HLeaf b) = a == b
	(==) (HNode a b) (HNode c d) = a == c && b == d
	(==) _ _ = False

instance Ord a => Ord (HTree a) where
	(<=) (HLeaf a) (HLeaf b) = a <= b
	(<=) (HLeaf _) (HNode _ _) = True
	(<=) (HNode _ _) (HLeaf _) = False
	(<=) (HNode a b) (HNode c d) = case a == c of
		False -> a <= c
		True -> b <= d

instance Show a => Show (STree a) where
	show SLeaf = "SLeaf"
	show (SNode a b c) = "SNode (" ++ show a ++ ") " ++ show b ++ " (" ++ show c ++ ")"

instance Eq a => Eq (STree a) where
	(==) SLeaf SLeaf = True
	(==) (SNode a b c) (SNode d e f) = a == d && b == e && c == f
	(==) _ _ = False

instance Ord a => Ord (STree a) where
	(<=) SLeaf SLeaf = True
	(<=) SLeaf (SNode _ _ _) = True
	(<=) (SNode _ _ _) SLeaf = False
	(<=) (SNode a b c) (SNode d e f) = case a == d of
		False -> a <= d
		True -> case b == e of
			False -> b <= e
			True -> case c == f of
				False -> c <= f
				True -> True

-- Alkalmazz egy függvényt az összes tárolt értékre
-- Példa: mapHTree (+5) (HLeaf 5) == (HLeaf 10)
mapHTree :: (a -> b) -> HTree a -> HTree b
mapHTree f (HLeaf x) = HLeaf (f x)
mapHTree f (HNode a b) = HNode (mapHTree f a) (mapHTree f b)

mapSTree :: (a -> b) -> STree a -> STree b
mapSTree _ (SLeaf) = SLeaf
mapSTree f (SNode a x b) = SNode (mapSTree f a) (f x) (mapSTree f b)

-- Functor
--------------------------------------------------------------------------------

-- "Szerkezetet" megőrző morfizmus
class Functor f where
	fmap :: (a -> b) -> f a -> f b

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

instance Functor [] where
	fmap :: (a -> b) -> [a] -> [b]
	fmap f [] = []
	fmap f (x:xs) = f x:fmap f xs

instance Functor Maybe where
	fmap :: (a -> b) -> Maybe a -> Maybe b
	fmap f Nothing = Nothing
	fmap f (Just a) = Just (f a)

instance Functor HTree where
	fmap :: (a -> b) -> HTree a -> HTree b
	fmap = mapHTree

instance Functor STree where
	fmap :: (a -> b) -> STree a -> STree b
	fmap = mapSTree

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
	fmap f (T a b c) = T (f a) (f b) (f c)

instance Functor Quad where
	fmap f (Q a b c d) = Q (f a) (f b) (f c) (f d)

instance Functor Tagged where
	fmap f (Tag s a) = Tag s (f a)

instance Functor Void where
	fmap _ _ = Void

instance Functor Try where
	fmap f (Success x) = Success (f x)
	fmap f (Fail s) = Fail s

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

e10 :: RoseTree Integer
e10 = RoseNode 50 [RoseNode 10 [RoseNode 5 [],RoseNode 2 []],RoseNode 5 []]

instance Functor IFun where
	fmap f (IFun h) = IFun (f . h)

instance Functor (Fun a) where
	fmap f (Fun h) = Fun (f . h)

instance Functor RoseTree where
	fmap f (RoseNode a b) = RoseNode (f a) (fmap f <$> b)

instance Functor (TreeI i) where
	fmap f (LeafI a) = LeafI (f a)
	fmap f (NodeI h) = NodeI (fmap f . h)

instance Functor (Const i) where
	fmap _ (Const v) = Const v
