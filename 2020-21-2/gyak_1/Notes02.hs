
-- következő BEAD:  magasabbrendű listafüggvény (BSc-szintű)
--------------------------------------------------------------------------------

-- feladatok01.hs


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
