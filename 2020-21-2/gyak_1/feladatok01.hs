
{-# options_ghc -Wincomplete-patterns #-}

import Data.List (foldl')

-- Gyakorló feladatok (ismétlés, függvények, mintaillesztés, ADT-k, osztályok)
--------------------------------------------------------------------------------

-- Definiáld a "xor" műveletet Bool típuson. Használj mintaillesztést,
-- vagy Prelude-ből standard függvényt.
xor :: Bool -> Bool -> Bool
xor = undefined

-- függvények
--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen, de
-- típushelyesen és totális függvényként (nem lehet végtelen loop
-- vagy exception).
f1 :: (a, (b, (c, d))) -> (b, c)
f1 = undefined

f2 :: (a -> b) -> a -> b
f2 = undefined

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 = undefined

f4 :: (a -> b -> c) -> (b -> a -> c)
f4 = undefined                            -- f4 = flip


f5 :: ((a, b) -> c) -> a -> b -> c      -- standard: curry
f5 f a b = f (a, b)
   -- _        -- f :: (a, b) -> c      -- tehát: f-et kéne használni
   -- f _      -- a :: a, b :: b
   -- f (a, b)
   -- "type Tetris"

-- f6-f7 inverz függvények
-- lásd előadás: (b*c)^a = (b^a)*(c^a)
f6 :: (a -> (b, c)) -> (a -> b, a -> c)
f6 f = ((\a -> fst (f a)), (\a -> snd (f a)))
   -- _ :: (a -> b, a -> c)
   -- (_, _)  --   _ :: a -> b      _ :: a -> c

   -- ha valami típusa (a,b) akkor azonnal kiírhatunk egy (_, _) kifejezést
   -- ha valami típusa (a -> b) akkor azonall írhatunk lambdát (*mindig* működik) vagy
   --    felveszünk egy paramétert az = bal oldalán (*nem mindig*)

   -- (\a -> _, \a -> _)    --   _  :: b     _ :: c

f7 :: (a -> b, a -> c) -> (a -> (b, c))
f7 (f, g) a = (f a, g a)

-- a-ból is és b-ből is tudunk c-t adni, akkor
-- a-ból is tudunk c-t adni, és b-ből is tudunk c-adni
f8 :: (Either a b -> c) -> (a -> c, b -> c)
f8 f = (\a -> f (Left a), \b -> f (Right b))
  --   _ :: (a -> c, b -> c)
  --  (_, _)   -- _ :: a -> c   _ :: b -> c
  --  (\a -> _, \b -> _)  -- _ :: c     _ :: c

f9 :: (a -> c, b -> c) -> (Either a b -> c)
f9 = undefined

f10 :: Either (a, b) (a, c) -> (a, Either b c)
f10 = undefined

f11 :: (a, Either b c) -> Either (a, b) (a, c)
f11 = undefined

-- bónusz feladat (nehéz)
f12 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f12 f g = f (g (\a -> f a a)) (g (\a -> f a a))
  -- _ :: b
  -- f (_ :: a) (_ :: a)
  -- f (g (_ :: a -> b)) (_ :: a)
  -- f (g (\a -> (_ :: b))) (_ :: a)
  -- f (g (\a -> f (_ :: a) (_ :: a))) (_ :: a)
  -- f (g (\a -> f a a)) (_ :: a)


-- listák
--------------------------------------------------------------------------------

-- Írj egy "applyMany :: [a -> b] -> a -> [b]" függvényt, ami egy
-- listában található minden függvényt alkalmaz egy
-- értékre. Pl. "applyMany [(+10), (*10)] 10 == [20, 100]".
-- applyMany :: [a -> b] -> a -> [b]
-- applyMany = undefined

applyMany :: [a -> b] -> a -> [b]
applyMany []     a = []
applyMany (f:fs) a = f a : applyMany fs a

applyMany' :: [a -> b] -> a -> [b]
applyMany' fs a = map (\f -> f a) fs
  -- map (_ :: (a -> b) -> b) fs
  -- map (\f -> (_ :: b)) fs

applyMany'' :: [a -> b] -> a -> [b]
applyMany'' fs a = map ($ a) fs


-- Definiálj egy "NonEmptyList a" típust, akár ADT-ként, akár
-- típusszinonímaként, aminek az értékei nemüres listák.

type NonEmptyList a = (a, [a])

fromList :: [a] -> Maybe (NonEmptyList a)
fromList []     = Nothing
fromList (a:as) = Just (a, as)

toList :: NonEmptyList a -> [a]
toList (a, as) = a:as

-- Írj egy "fromList :: [a] -> Maybe (NonEmptyList a)" függvényt, ami
-- nemüres listát ad vissza egy standard listából, ha az input nem
-- üres.

-- emlékeztető: sima lista ADT definíciója:
data List a = Empty | Cons a (List a)

data NonEmptyList' a = Single a | Cons' a (NonEmptyList' a)

fromList' :: [a] -> Maybe (NonEmptyList' a)
fromList' []     = Nothing
fromList' (a:as) = case fromList' as of
  Nothing  -> Just (Single a)
  Just as' -> Just (Cons' a as')
  -- házi: hogyan lehet kispórolni a case Nothing/Just-ot listaelemknél

toList' :: NonEmptyList' a -> [a]
toList' (Single a)   = [a]
toList' (Cons' a as) = a : toList' as


-- Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
-- az összes bemenő függvény kompozíciója,
-- pl. "composeAll [f, g, h] x == f (g (h x))"
composeAll :: [a -> a] -> a -> a
composeAll []     a = a
composeAll (f:fs) a = f (composeAll fs a)

-- tail recursive verzió (akkumulátoros)
composeAll' :: [a -> a] -> a -> a
composeAll' []     a = a
composeAll' (f:fs) a = composeAll' fs (f a)

composeAll'' :: [a -> a] -> a -> a
composeAll'' fs a = foldr (.) id fs a

-- eta konverzió (lentről fölfele: eta expanzió, föntről le: eta redukció)
composeAll''' :: [a -> a] -> a -> a
composeAll''' = foldr (.) id


-- Definiáld a "merge :: Ord a => [a] -> [a] -> [a]" függvényt, ami két nemcsökkenő
-- rendezett listát összefésül úgy, hogy az eredmény is rendezett maradjon.
merge :: Ord a => [a] -> [a] -> [a]
merge = undefined


-- (bónusz) Definiáld a "mergeSort :: Ord a => [a] -> [a]" függvényt, ami a "merge"
-- iterált felhasználásával rendez egy listát.
mergeSort :: Ord a => [a] -> [a]
mergeSort = undefined

   -- (tipp: ne a beszúró rendezést definiáljuk, hanem a rendes merge sortot)
   -- merge [a] [a1, a1 ... , an]  -- O(N)    (ne ezt iteráljuk!)
   --   (minél inkább azonos hosszúságú részlisták merge-elése kéne)


-- (bónusz) Definiáld a "sublists :: [a] -> [[a]]" függvényt, ami a bemenő lista
-- minden lehetséges részlistáját visszaadja. Pl. "sublists [1, 2] == [[],
-- [1], [2], [1, 2]]".  A részlisták sorrendje az eredményben tetszőleges, a
-- fontos, hogy az össze részlista szerepeljen.

-- Kapcsolódó fogalom: hatványhalmaz

sublists :: [a] -> [[a]]
sublists []     = [[]]
sublists (a:as) = map (a:) (sublists as) ++ sublists as

-- ha nem bízok a fordítóban, akkor kihozom egy let-ben
sublists' :: [a] -> [[a]]
sublists' []     = [[]]
sublists' (a:as) = let as' = sublists as in map (a:) as' ++ as'

-- sublists' :: [a] -> [[a]]
-- sublists' []     = [[]]
-- sublists' (a:as) =
--   let as' = sublists as in map (a:) as' ++ as'




-- listával ágazó fa ADT
--------------------------------------------------------------------------------

-- Vegyük a következő ADT-t:
data RTree a = RNode a [RTree a]
  deriving Show

-- fa: minden node-nak 0 vagy több gyereke van
--  ("trie" adatstruktúra)

t1 :: RTree Int
t1 = RNode 100 [RNode 0 [], RNode 1 [], RNode 2 []]

t2 :: RTree Int
t2 = RNode 1000 (replicate 10 t1)

instance Functor RTree where
  -- fmap :: (a -> b) -> RTree a -> RTree b
  fmap f (RNode a ts) = RNode (f a) (fmap (fmap f) ts)
                                   -- []   RTree

  -- minden ADT Functor instance-ra:
  --   minden konstruktorból ugyanolyan konstruktort adunk vissza.
  --     (fmap-elés nem változtahtaja meg a struktúrát, kizárólag "a" fölött map-elhet)

-- Írj "size :: RTree a -> Int" függvényt, ami megszámolja a fában levő
-- "a"-kat. Pl. size (Node 0 [Node 1 []]) == 2
size :: RTree a -> Int
size (RNode _ ts) = foldl' (\acc t -> acc + size t) 1 ts
    -- 1 + sum (map size ts)

-- osztályok
--------------------------------------------------------------------------------

class Eq' a where
  eq :: a -> a -> Bool

class Eq' a => Ord' a where
  lte :: a -> a -> Bool

fun1 :: Ord' a => a -> a -> a -> Bool
fun1 = undefined

fun2 :: Ord' a => a -> a -> a -> Bool
fun2 = undefined

class Show' a where
  show' :: a -> String

data Tree a = Leaf a | Node (Tree a) (Tree a)
data Color = Red | Green | Blue

-- írd meg a következő instance-okat
instance Eq' Color where
  eq = undefined

instance Ord' Color where
  lte = undefined

instance Show' Color where
  show' = undefined

instance Eq' a => Eq' (Maybe a) where
  eq = undefined

instance Ord' a => Ord' (Maybe a) where
  lte = undefined

instance Show' a => Show' (Maybe a) where
  show' = undefined

instance Eq' a => Eq' [a] where
  eq = undefined

instance Ord' a => Ord' [a] where
  lte = undefined

instance Show' a => Show' [a] where
  show' = undefined

instance Eq' a => Eq' (Tree a) where
  eq = undefined

instance Ord' a => Ord' (Tree a) where
  lte = undefined

instance Show' a => Show' (Tree a) where
  show' = undefined

instance Eq' a => Eq' (RTree a) where
  eq = undefined

instance Ord' a => Ord' (RTree a) where
  lte = undefined

instance Show' a => Show' (RTree a) where
  show' = undefined
