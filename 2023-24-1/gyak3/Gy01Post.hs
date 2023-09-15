{-
- canvas EA oldal: tematika + összes info
	- követelmény:
		- gyak: minimum követelmény: 13 pont
			- max 3 hiányzás
			- órai eleji kisfeladat: 0-2 pont (10 perc)
			- nagy házi feladat: félév során 3 darab, 4-4 pontért
				- mindegyik határideje: vizsgaidőszak eleje
				- canvas-ra egy .hs fájlt kell feltölteni
		- vizsga:
			- jegy ebből származik
			- 2 órás, feladatmegoldás
		- előadás látogatása (BSc hallgatóknak) kötelező

- Ha valaki el van maradva: lambda.inf.elte.hu kezdő Haskell jegyzet
                            Learn you a Haskell! könyv, stb.

- Előzetes ismeretek:
	- BSc "funkcionális programozás" ismerete (készség szinten)
	  (lambda.inf.elte.hu "Kezdő Haskell" jegyzet ajánlott)

Következő kisfeladat:
	- nagyon egyszerű: lényeg, hogy canvas-ba feladatot feltöltse mindenki

Feladatok, technika, ismétlés
	- ghci parancsok
	  :q              kilépés
	  :l <fájl>       betöltés
	  :r              újratöltés
	  :t <kifejezés>  kifejezés típusát megadja
	  :i <azonosító>  információt ír ki (operátorról is)
-}


--------------------------------------------------------------------------------

{-# LANGUAGE InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns -Wno-tabs #-}
module Gy01 where


-- Gyakorló feladatok (ismétlés, függvények, mintaillesztés, ADT-k, osztályok)
--------------------------------------------------------------------------------

-- Definiáld a "xor" műveletet Bool típuson. Használj mintaillesztést,
-- vagy Prelude-ből standard függvényt.

xor :: Bool -> Bool -> Bool
xor a b = let c = a /= b in c

-- case ... of ...

-- let ... in ...

-- ... where ...

-- Függvények
--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen, de típushelyesen és teljes
-- függvényekként. (Azaz nem lehet végtelen ciklus vagy kivétel dobás!).

f1 :: (a, (b, (c, d))) -> (b, c)
f1 (_,(b,(c,_))) = (b,c)

f2 :: (a -> b) -> (a -> b)
f2 f x = f x -- id

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 f g x = f $ g x -- (.)

f4 :: (a -> b -> c) -> b -> a -> c
f4 f x y = f y x -- flip

f5 :: ((a, b) -> c) -> a -> b -> c
f5 f a b = f (a, b) -- curry

f6 :: (a -> b -> c) -> (a, b) -> c
f6 f (a, b) = f a b -- uncurry

f7 :: (a -> (b, c)) -> (a -> b, a -> c)
f7 f = (g , h) -- (fst.f , snd.f)
	where
		g a = fst $ f a
		h a = (snd.f) a

f8 :: (a -> b, a -> c) -> (a -> (b, c))
f8 (g, h) a = (g a,h a)

f9 :: (Either a b -> c) -> (a -> c, b -> c)
f9 f = let -- (f.Left, f.Right)
	g a = f (Left a)
	h b = (f.Right) b
	in (g,h)

f10 :: (a -> c, b -> c) -> (Either a b -> c)
f10 (g,h) x = case x of
	(Left a) -> g a
	(Right b) -> h b

f11 :: Either (a, b) (a, c) -> (a, Either b c)
f11 (Left (a, b)) = (a,Left b)
f11 (Right (a, c)) = (a,Right c)

f12 :: (a, Either b c) -> Either (a, b) (a, c)
f12 (a, Left b) = Left (a, b)
f12 (a, Right c) = Right (a, c)

-- bónusz feladat (nehéz)
f13 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f13 f g = let k x = f x x in f (g k) (g k)


-- Listák
--------------------------------------------------------------------------------

-- Írj egy "applyMany :: [a -> b] -> a -> [b]" függvényt, ami egy
-- listában található minden függvényt alkalmaz egy értékre.
-- Példa: applyMany [(+10), (*10)] 10 == [20, 100]
applyMany :: [a -> b] -> a -> [b]
applyMany [] _ = []
applyMany (x:xs) a = (x a):applyMany xs a

data NonEmptyList a = Last a | Cons a (NonEmptyList a)

-- Definiálj egy "NonEmptyList a" típust, akár ADT-ként, akár típusszinonímaként,
-- aminek az értékei nemüres listák, majd definiáld az alábbi két függvényt rá.

toList :: NonEmptyList a -> [a]
toList (Last a) = [a]
toList (Cons a b) = a:toList b

fromList :: [a] -> Maybe (NonEmptyList a)
fromList [] = Nothing
fromList (x:xs) = Just $ case (fromList xs) of
	Nothing -> Last x
	Just s -> Cons x s


-- Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
-- az összes bemenő függvény kompozíciója.
-- Példa: composeAll [f, g, h] x == f (g (h x))
composeAll :: [a -> a] -> a -> a
composeAll [] x = x
composeAll (f:fs) x = f (composeAll fs x)


-- Definiáld a "merge :: Ord a => [a] -> [a] -> [a]" függvényt, ami két nemcsökkenő
-- rendezett listát összefésül úgy, hogy az eredmény is rendezett maradjon.
-- Példa: merge [2,5,7] [1,3,4,6] == [1,2,3,4,5,6,7]
merge :: Ord a => [a] -> [a] -> [a]
merge [] a = a
merge a [] = a
merge (a:as) (b:bs) = case a < b of
	True -> a:merge as (b:bs)
	False -> b:merge (a:as) bs


-- (bónusz) Definiáld a "mergeSort :: Ord a => [a] -> [a]" függvényt, ami a "merge"
-- iterált felhasználásával rendez egy listát.
-- Példa: mergeSort [5,2,6,3,1] == [1,2,3,5,6]
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort (x:xs) = merge [x] (mergeSort xs)


-- (bónusz) Definiáld a "sublists :: [a] -> [[a]]" függvényt, ami a bemenő lista
-- minden lehetséges részlistáját visszaadja. A részlisták sorrendje az eredményben
-- tetszőleges, a fontos, hogy az össze részlista szerepeljen.
-- Kapcsolódó fogalom: hatványhalmaz
-- Példa: sublists [1, 2]    == [[], [1], [2], [1,2]]
-- Példa: sublists [1, 2, 3] == [[], [1], [2], [3], [1,2], [1,3], [2,3], [1,2,3]]
sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = let k = sublists xs in map (x:) k ++ k


-- osztályok
--------------------------------------------------------------------------------

class Eq' a where
	eq :: a -> a -> Bool

class Eq' a => Ord' a where
	lte :: a -> a -> Bool

class Show' a where
	show' :: a -> String

data Tree a = Leaf a | Node (Tree a) (Tree a)
data Color = Red | Green | Blue

-- írd meg a következő instance-okat
instance Eq' Color where
	eq :: Color -> Color -> Bool
	eq Red Red = True
	eq Green Green = True
	eq Blue Blue = True
	eq _ _ = False

instance Ord' Color where
	lte :: Color -> Color -> Bool
	lte Red _ = True
	lte Green Red = False
	lte Green _ = True
	lte Blue Blue = True
	lte Blue _ = False

instance Show' Color where
	show' Red = "Red"
	show' Green = "Green"
	show' Blue = "Blue"

instance Eq' a => Eq' (Maybe a) where
	eq :: (Maybe a) -> (Maybe a) -> Bool
	eq Nothing Nothing = True
	eq (Just _) Nothing = False
	eq Nothing (Just _) = False
	eq (Just a) (Just b) = eq a b

instance Ord' a => Ord' (Maybe a) where
	lte Nothing _ = True
	lte (Just _) Nothing = False
	lte (Just a) (Just b) = lte a b

instance Show' a => Show' (Maybe a) where
	show' Nothing = "Nothing"
	show' (Just a) = "Just " ++ show' a

instance Eq' a => Eq' [a] where
	eq [] [] = True
	eq (x:xs) (y:ys) = (eq x y) && (eq xs ys)
	eq _ _ = False

instance Ord' a => Ord' [a] where
	lte [] _ = True
	lte (x:xs) (y:ys) = case eq x y of
		True -> lte xs ys
		False -> lte x y
	lte (x:xs) [] = False

instance Show' a => Show' [a] where
	show' [] = "[]"
	show' (x:[]) = "[" ++ show' x ++ "]"
	show' (x:y:xs) = case show' (y:xs) of
		[] -> ""
		(s:ss) -> "[" ++ show' x ++ "," ++ ss

instance Eq' a => Eq' (Tree a) where
	eq (Leaf a) (Leaf b) = eq a b
	eq (Node a b) (Node c d) = eq a c && eq b d
	eq _ _ = False

instance Ord' a => Ord' (Tree a) where
	lte (Leaf a) (Leaf b) = lte a b
	lte (Leaf _) (Node _ _) = True
	lte (Node _ _) (Leaf _) = False
	lte (Node a b) (Node c d) = case eq a c of
		False -> lte a c
		True -> lte b d

instance Show' a => Show' (Tree a) where
	show' (Leaf a) = show' a
	show' (Node a b) = "(" ++ (show' a) ++ "," ++ (show' b) ++ ")"
