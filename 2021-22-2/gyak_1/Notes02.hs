
{-# options_ghc -Wincomplete-patterns #-}


--------------------------------------------------------------------------------
-- Következő canvas:  közepesen nehéz rekurzív listafüggvény (BSc kisebb feladat szintű)


-- Gyakorló feladatok (ismétlés, függvények, mintaillesztés, ADT-k, osztályok)
--------------------------------------------------------------------------------
-- Definiáld a "xor" műveletet Bool típuson. Használj mintaillesztést,
-- vagy Prelude-ből standard függvényt.
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _     _    = False     -- figyelmeztet a -Wincomplete-patterns


-- -- "case" kifejezéssel
-- xor' :: Bool -> Bool -> Bool
-- xor' x y = case x of
--   True  -> _
--   False -> _

-- xor' :: Bool -> Bool -> Bool
-- xor' x y = case x of True -> True; False -> False

-- "let" / "where"

foo :: Int -> Int -> Int
foo x y = a + b + f y where
  a = 100 + x
  b = 200 + y

  f :: Int -> Int
  f 0 = 10
  f _ = 100

foo2 :: Int -> Int -> Int
foo2 x y =
  let a = 100 + x
      b = 200 + y

      f :: Int -> Int
      f 0 = 10
      f _ = 100

  in a + b + f y

  -- egysoros let példa:  "let x = 10 in x + 20"


-- függvények
--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen, de
-- típushelyesen és totális függvényként (azaz nem lehet végtelen loop
-- vagy kivétel dobás!).

-- (a, b, c, d)

-- id :: a -> a
-- id x = x

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
-- f2 f = f
-- f2 = id
                -- standard verzió: ($)
                -- f $ g $ h x
                -- f (g (h x))
-- f2 = ($)
-- f2 f = f

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith ($) :: [a -> b] -> [a] -> [b]
--   zipWith ($) [(+10), (+20)] [0, 5] == [10,25]


f3 :: (b -> c) -> (a -> b) -> a -> c   -- függvény kompozíció
f3 f g a = f (g a)
   -- _         _ :: c
   -- f _       _ :: b
   -- f (g _)   _ :: a
   -- f (g a)

f3alt :: (b -> c) -> (a -> b) -> a -> c
f3alt = (.)

f3' :: (b -> c) -> (a -> b) -> a -> c
f3' = (.)

f4 :: (a -> b -> c) -> b -> a -> c
f4 f b a = f a b
  -- f4 = flip

f5 :: ((a, b) -> c) -> (a -> (b -> c))  -- figyelem: (->) jobbra zárójelez!
f5 f a b = f (a, b)
  -- f5 = curry

  -- (parciális alkalmazás)
  -- (&&)       :: Bool -> (Bool -> Bool)
  -- (&&) False :: Bool -> Bool


f6 :: (a -> b -> c) -> (a, b) -> c
-- f6 :: (a -> (b -> c)) -> ((a, b) -> c)
f6 f (a, b) = f a b
  -- f6 = uncurry
  -- f6 = \f (a, b) -> f a b
  -- f6 = \f p -> case p of (a, b) -> f a b
  -- f6 = \f p -> f (fst p) (snd p)
  -- f6 = \f -> \p -> f (fst p) (snd p)

  -- GHC fordítás során a következő lesz a definíció
  -- f6 = \f -> \p -> case p of (a, b) -> f a b

f7 :: (a -> (b, c)) -> ((a -> b), (a -> c))
f7 f = (\a -> fst (f a), \a -> snd (f a))
  -- _ :: (a -> b, a -> c)      (azonnal tudom, hogy (_, _) a megoldás)
  -- (_, _)         _ :: a -> b      _ :: a -> c
  -- (\a -> _, _)   _ :: b           _ :: a -> c
  --
  -- f7 f = (fst . f , snd . f)

f8 :: (a -> b, a -> c) -> (a -> (b, c))
-- f8 :: (a -> b, a -> c) -> a -> (b, c)
f8 (f, g) a = (f a, g a)

-- data Either a b = Left a | Right b

f9 :: (Either a b -> c) -> (a -> c, b -> c)  -- a-t *és* b-t is kezel a fv
f9 f = (\a -> f (Left a), \b -> f (Right b))
  -- f9 f = (f . Left, f . Right)

f10 :: (a -> c, b -> c) -> (Either a b -> c)
f10 (f, g) (Left a)  = f a
f10 (f, g) (Right b) = g b

f11 :: Either (a, b) (a, c) -> (a, Either b c)
f11 (Left (a, b))  = (a, Left b)
f11 (Right (a, c)) = (a, Right c)

f12 :: (a, Either b c) -> Either (a, b) (a, c)
f12 (a, Left b)  = Left (a, b)
f12 (a, Right c) = Right (a, c)


-- bónusz feladat (nehéz)
f13 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f13 = undefined


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

-- map f []     = []
-- map f (x:xs) = f x : map f xs

-- applyMany fs a = map (\f -> f a) fs
-- applyMany fs a = [f a | f <- fs]
-- applyMany fs a = map ($ a) fs            -- ($ a) :: (a -> b) -> b


-- Definiálj egy "NonEmptyList a" típust, akár ADT-ként, akár
-- típusszinonímaként, aminek az értékei nemüres listák.

type NonEmptyList a = (a, [a])

toList :: NonEmptyList a -> [a]
toList (a, as) = a:as

fromList :: [a] -> Maybe (NonEmptyList a)
fromList []     = Nothing
fromList (a:as) = Just (a, as)


data NonEmptyList' a = Single a | Cons a (NonEmptyList' a)

toList' :: NonEmptyList' a -> [a]
toList' (Single a)  = [a]
toList' (Cons a as) = a : toList' as

-- opcionális házi: optimalizálni az alábbi definíciót, hogy ne legyen rekurzív Maybe esetszétválasztás.
fromList' :: [a] -> Maybe (NonEmptyList' a)
fromList' []     = Nothing
fromList' (a:as) = case fromList' as of
  Nothing -> Just (Single a)
  Just as -> Just (Cons a as)

-- fromList' (a:as) = Just $ case fromList' as of
--   Nothing -> Single a
--   Just as -> Cons a as


--   - Írj egy "toList :: NonEmptyList a -> [a]" függvényt!

--   - Írj egy "fromList :: [a] -> Maybe (NonEmptyList a)" függvényt, ami
--     nemüres listát ad vissza egy standard listából, ha az input nem
--     üres.


-- Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
-- az összes bemenő függvény kompozíciója,
-- pl. "composeAll [f, g, h] x == f (g (h x))"
composeAll :: [a -> a] -> a -> a
composeAll []     a = a
composeAll (f:fs) a = f (composeAll fs a)

-- composeAll []     = \a -> a
-- composeAll (f:fs) = \a -> f (composeAll fs a)

-- composeAll []     = id
-- composeAll (f:fs) = f . composeAll fs

-- composeAll = foldr (.) id


-- Definiáld a "merge :: Ord a => [a] -> [a] -> [a]" függvényt, ami két nemcsökkenő
-- rendezett listát összefésül úgy, hogy az eredmény is rendezett maradjon.
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
merge [] ys = ys
merge xs [] = xs

-- (bónusz) Definiáld a "mergeSort :: Ord a => [a] -> [a]" függvényt, ami a "merge"
-- iterált felhasználásával rendez egy listát.
mergeSort :: Ord a => [a] -> [a]
mergeSort = undefined


-- (bónusz) Definiáld a "sublists :: [a] -> [[a]]" függvényt, ami a bemenő lista
-- minden lehetséges részlistáját visszaadja. Pl. "sublists [1, 2] == [[],
-- [1], [2], [1, 2]]".  A részlisták sorrendje az eredményben tetszőleges, a
-- fontos, hogy az össze részlista szerepeljen.
-- Kapcsolódó fogalom: hatványhalmaz
sublists :: [a] -> [[a]]
sublists = undefined


-- osztályok
--------------------------------------------------------------------------------

class Eq' a where
  eq :: a -> a -> Bool

class Eq' a => Ord' a where
  lte :: a -> a -> Bool

class Show' a where
  show' :: a -> String

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show, Ord)  -- Ord: "lexikografikus" rendezés

data Color = Red | Green | Blue
  deriving (Eq, Show, Ord)

-- írd meg a következő instance-okat
instance Eq' Color where
  eq Red Red = True
  eq Green Green = True
  eq Blue Blue = True
  eq _ _ = False

instance Ord' Color where
  lte Red   _ = True
  lte Green Green = True
  lte Green Blue = True
  lte Blue Blue = True
  lte _ _ = False

instance Show' Color where
  show' Red = "Red"
  show' Green = "Green"
  show' Blue = "Blue"

instance Eq' a => Eq' (Maybe a) where
  eq = undefined

instance Ord' a => Ord' (Maybe a) where
  lte Nothing _ = True
  lte (Just a) (Just a') = lte a a'
  lte _ _ = False

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
