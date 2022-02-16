{-# options_ghc -Wincomplete-patterns #-}

module Lesson02 where

-- függvények
--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen, de
-- típushelyesen és totális függvényként (azaz nem lehet végtelen rekurzió
-- vagy kivétel dobás!).

id' :: a -> a
id' x = x

-- polimorf függvény: tetszőleges a, b, c, d típusra működik
--    a, b, c, d: "típusváltozó", kisbetűs nevek típusparaméterek (tetszőleges típusok)
f1 :: (a, (b, (c, d))) -> (b, c)
f1 (a, (b, (c, d))) = (b, c)

   -- érték :: típus   : gyakran érdemes értéket a típusáról elnevezni

   -- hole:
   --   _-t teszünk definícióba, ghci megírja, hogy milyen típusú kifejezést
   --       kéne a helyére írni + lokális scope-beli típusokat
   -- _             _ :: (b, c)
   -- (_, _)        _ :: b     _ :: c


f2 :: (a -> b) -> a -> b
f2 f a = f a 

f2' :: (a -> b) -> a -> b
f2' = ($)

f2'' :: (a -> b) -> (a -> b)
f2'' f = f

f2''' :: (a -> b) -> (a -> b)
f2''' = id

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 f g a = f (g a)

f3' :: (b -> c) -> (a -> b) -> a -> c
f3' = (.)

f4 :: (a -> b -> c) -> b -> a -> c
f4 f b a = f a b

f4' :: (a -> b -> c) -> b -> a -> c
f4' = flip

f5 :: ((a, b) -> c) -> (a -> (b -> c))
f5 f a b = f (a,b)

f5' :: ((a, b) -> c) -> (a -> (b -> c))
f5' = curry 

f6 :: (a -> b -> c) -> (a, b) -> c
f6 f (a,b) = f a b

f6' :: (a -> b -> c) -> (a, b) -> c
f6' = uncurry

f7 :: (a -> (b, c)) -> (a -> b, a -> c)
f7 f = (\a -> fst (f a), \a -> snd (f a)) 

f8 :: (a -> b, a -> c) -> (a -> (b, c))
f8 (f,g) a = (f a, g a)

-- data Either a b = Left a | Right b -- diszjunkt unió, összeg típus , csak a vagy csak b van
-- data (,) a b = (,) a b -- szorzat típus, a és b is van

f9 :: (Either a b -> c) -> (a -> c, b -> c)
f9 f = (\a -> f (Left a), \b -> f (Right b))

f10 :: (a -> c, b -> c) -> Either a b -> c
f10 (f,g) (Left a)  = f a
f10 (f,g) (Right b) = g b

f11 :: Either (a, b) (a, c) -> (a, Either b c)
f11 (Left (a,b))  = (a, Left b)
f11 (Right (a,c)) = (a, Right c)

f12 :: (a, Either b c) -> Either (a, b) (a, c)
f12 (a,Left b)  = Left (a,b)
f12 (a,Right c) = Right (a,c)

-- (nehezebb)
f13 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f13 = undefined

-- (nagyon nehéz)
f14 :: [[a] -> a] -> [a]
f14 = undefined
-- próbálgatni pl. const vagy (!! x) függvénnyel érdemes.

-- (szintén)
f15 :: (((a -> b) -> b) -> c -> a) -> c -> a
f15 = undefined

-- listák
--------------------------------------------------------------------------------

-- Írj egy "applyMany :: [a -> b] -> a -> [b]" függvényt, ami egy
-- listában található minden függvényt alkalmaz egy
-- értékre. Pl. "applyMany [(+10), (*10)] 10 == [20, 100]".
-- applyMany :: [a -> b] -> a -> [b]
-- applyMany = undefined

applyMany :: [a -> b] -> a -> [b]
applyMany []     _ = []
applyMany (f:fs) a = f a : applyMany fs a

applyMany' :: [a -> b] -> a -> [b]
applyMany' fs a = map (\f -> f a) fs

applyMany'' :: [a -> b] -> a -> [b]
applyMany'' fs a = map ($ a) fs
-- Definiálj egy "NonEmptyList a" típust ADT-ként,
-- aminek az értékei nemüres listák.

--   - Írj egy "toList :: NonEmptyList a -> [a]" függvényt!

--   - Írj egy "fromList :: [a] -> Maybe (NonEmptyList a)" függvényt, ami
--     nemüres listát ad vissza egy standard listából, ha az input nem
--     üres.

data NonEmptyList a = End a | NonEmptyList a (NonEmptyList a)

toList :: NonEmptyList a -> [a]
toList (End a) = [a]
toList (NonEmptyList a b) = a : toList b

fromList :: [a] -> Maybe (NonEmptyList a)
fromList [] = Nothing
fromList xs = Just $ fl xs where
    fl [] = error "Mágia, ha a program bármikor is idejut! Ha ez megtörténik, akkor nagyon nagy a baj."
    fl [a] = End a
    fl (x:xs) = NonEmptyList x (fl xs)

data NonEmptyList' a = NonEmpty a [a]

toList' :: NonEmptyList' a -> [a]
toList' (NonEmpty x xs) = (x:xs)

fromList' :: [a] -> Maybe (NonEmptyList' a)
fromList' [] = Nothing
fromList' (x:xs) = Just $ NonEmpty x xs

-- Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
-- az összes bemenő függvény kompozíciója,
-- pl. "composeAll [f, g, h] x == f (g (h x))"
composeAll :: [a -> a] -> a -> a
composeAll (f:fs) a = f (composeAll fs a)
composeAll []     a = a

-- composeAll' :: [a -> a] -> a -> a
-- Rövidebb megoldás: Melyik függvényre hasonlít a fenti rekurzív megoldásnak a definíciója?

-- (bónusz) Definiáld a "sublists :: [a] -> [[a]]" függvényt, ami a bemenő lista
-- minden lehetséges részlistáját visszaadja. Pl:
-- sublists [1, 2] == [[], [1], [2], [1, 2]]
-- sublists "abc" == ["","a","b","c","ab","ac","bc","abc"]
-- A részlisták sorrendje az eredményben tetszőleges, a
-- fontos, hogy az összes részlista szerepeljen.
-- Kapcsolódó fogalom: hatványhalmaz
-- Segítség: Nem kell túlbonyolítani. Azt kell leírni, hogy egy adott elem vagy benne van a részlistában vagy nincs benne.
sublists :: [a] -> [[a]]
sublists []     = [[]]
sublists (x:xs) = let ys = sublists xs in ys ++ map (x:) ys

-- osztályok
--------------------------------------------------------------------------------

data Color = Blue | Yellow | Red
data List a = Nil | Cons a (List a) -- rendes megszokott láncolt lista
-- "rekurzív", induktív adattípus
{-
t =
              adat1
            /      \
        adat2      adat3
        /  \
    adat4  adat5
-}
data BinaryTree a = Leaf a | Node (BinaryTree a) a (BinaryTree a)

t :: BinaryTree String
t = Node (Node (Leaf "adat4") "adat2" (Leaf "adat5")) "adat1" (Leaf "adat3")

t2 :: Num a => BinaryTree a
t2 = Node (Node (Leaf 2) 3 (Leaf 5)) 7 (Leaf 11)

height :: Num b => BinaryTree a -> b
height = undefined

treeSum :: Num a => BinaryTree a -> a
treeSum = undefined -- ezt a függvényt érdemes megcsinálni, segít a következő +/- feladatot könnyebben megoldani.

-- írd meg a következő instance-okat
instance Eq a => Eq (BinaryTree a) where
    (==) = undefined

instance Ord a => Ord (BinaryTree a) where -- inorder bejárás
    (<=) = undefined

instance Show a => Show (BinaryTree a) where -- inorder bejárás
    show = undefined

--------------------------------------

instance Foldable List where
    foldr = undefined

instance Foldable BinaryTree where
    foldr = undefined

listSum :: Num a => List a -> a
listSum = undefined

treeSum' :: Num a => BinaryTree a -> a
treeSum' = undefined