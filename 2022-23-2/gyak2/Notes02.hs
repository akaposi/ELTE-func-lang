
{-# language InstanceSigs, StandaloneDeriving, UndecidableInstances #-}
{-# options_ghc -Wincomplete-patterns #-}

--
--------------------------------------------------------------------------------

-- Következő kisfeladat:
--   egyszerű függvény valami "data"-val definiált típuson
--   (típus nem rekurzív, és a függvény sem)

-- Gyakorló feladatok (ismétlés, függvények, mintaillesztés, ADT-k, osztályok)
--------------------------------------------------------------------------------

-- Definiáld a "xor" műveletet Bool típuson. Használj mintaillesztést, vagy
-- Prelude-ből standard függvényt.
xor :: Bool -> Bool -> Bool
xor = undefined


-- függvények
--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen, de típushelyesen és totális
-- függvényként (azaz nem lehet végtelen rekurzió vagy kivétel dobás!).

f1 :: (a, (b, (c, d))) -> (b, c)
f1 (a, (b, (c, d))) = (b, c)

f2 :: (a -> b) -> (a -> b)
f2 = id

f2' :: (a -> b) -> (a -> b)
f2' = ($)                        -- f (g (h x))

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 = (.)

f4 :: (a -> b -> c) -> b -> a -> c
f4 = flip

f5 :: ((a, b) -> c) -> a -> b -> c
f5 = curry

f6 :: (a -> b -> c) -> (a, b) -> c
f6 = uncurry

f7 :: (a -> (b, c)) -> ((a -> b), (a -> c)) -- függvények párja
f7 f = (\a -> fst (f a), \a -> snd (f a))
  -- = (fst . f , snd . f)

f8 :: (a -> b, a -> c) -> a -> (b, c)
f8 (f, g) a = (f a, g a)

-- data Either a b = Left a | Right b

--   Left  :: a -> Either a b        (konstruktor 1 db "a" típusú mezővel)
--   Right :: b -> Either a b        (konstruktor 1 db "b" típusú mezővel)

-- f (Left x) = ...     -- mintaillesztés konstruktorra és annak az adatmezőjére
-- f (Right x) = ...

f9 :: (Either a b -> c) -> (a -> c, b -> c)
f9 f = (\a -> f (Left a), \b -> f (Right b))
   -- (f . Left, f . Right)

-- Int, Integer
-- data Integer = Small Int | Large ByteArray


f10 :: (a -> c, b -> c) -> Either a b -> c
f10 (f, g) (Left a)  = f a
f10 (f, g) (Right b) = g b
   -- "case" kifejezéssel:
-- f10 (f, g) eab = case eab of
--    Left a  -> f a
--    Right b -> g b

-- egy sorban:
-- f10 (f, g) eab = case eab of Left a -> f a; Right b -> g b

f11 :: Either (a, b) (a, c) -> (a, Either b c)
f11 (Left (a, b))  = (a, Left b)
f11 (Right (a, c)) = (a, Right c)

{-
x1 :: Either Int Bool
x1 = Left 100

x2 :: Either Int Bool
x2 = Right False

x3 :: Either (Int -> Int) Int
x3 = Left (\x -> x + 10)

x4 :: Either (Int -> Int) Int
x4 = Right 100
-}

-- Nem kizáró választás "a" és "b" között:
data Either' a b = Left' a | Right' b | Both a b

-- zip :: [a] -> [b] -> [(a, b)]
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

zipWith' :: (Either' a b -> c) -> [a] -> [b] -> [c]
zipWith' f (a:as) (b:bs) = f (Both a b) : zipWith' f as bs
zipWith' f (a:as) []     = f (Left' a)  : zipWith' f as []
zipWith' f []     (b:bs) = f (Right' b) : zipWith' f [] bs
zipWith' f []     []    = []


f12 :: (a, Either b c) -> Either (a, b) (a, c)
f12 = undefined


-- (bónusz, nehezebb)
f13 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f13 = undefined


-- Listák
--------------------------------------------------------------------------------

-- Írj egy "applyMany :: [a -> b] -> a -> [b]" függvényt, ami egy
-- listában található minden függvényt alkalmaz egy
-- értékre.
-- Pl. "applyMany [(+10), (*10)] 10 == [20, 100]".

applyMany :: [a -> b] -> a -> [b]
applyMany []     a = []
applyMany (f:fs) a = f a : applyMany fs a

-- applyMany fs a = map (\f -> f a) fs
-- applyMany fs a = map ($ a) fs          -- map (+10)


-- Definiálj egy "NonEmptyList a" típust "data"-ként.n
-- aminek az értékei nemüres listák.

data NonEmptyList a = Single a | More a (NonEmptyList a)
  deriving (Eq, Show)

--   - Írj egy "toList :: NonEmptyList a -> [a]" függvényt!
toList :: NonEmptyList a -> [a]
toList (Single a)  = [a]
toList (More a as) = a : toList as

--   - Írj egy "fromList :: [a] -> Maybe (NonEmptyList a)" függvényt, ami
--     nemüres listát ad vissza egy standard listából, ha az input nem
--     üres.
fromList :: [a] -> Maybe (NonEmptyList a)
fromList []     = Nothing
fromList (a:as) = case fromList as of
  Nothing -> Just (Single a)
  Just as -> Just (More a as)    -- "as" árnyékolja az input "as"-t

-- fromList []     = Nothing
-- fromList (a:as) = f $ case fromList as of
--   Nothing -> Single a
--   Just as -> More a as


-- Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
-- az összes bemenő függvény kompozíciója,
-- pl. "composeAll [f, g, h] x == f (g (h x))"
composeAll :: [a -> a] -> a -> a
composeAll []     a = a
composeAll (f:fs) a = f (composeAll fs a)

-- composeAll = foldr (.) id
-- foldr f   x  [a1, a2, a3] == f a1 (f a2 (f a3 x))
-- foldr (.) id [a1, a2, a3] == a1 . (a2 . (a3 . id)) == a1 . a2 . a3 . id

-- (bónusz) Definiáld a "sublists :: [a] -> [[a]]" függvényt, ami a bemenő lista
-- minden lehetséges részlistáját visszaadja. Pl:
--   sublists [1, 2] == [[], [1], [2], [1, 2]]
--   sublists "abc" == ["","a","b","c","ab","ac","bc","abc"]
-- A részlisták sorrendje az eredményben tetszőleges, a
-- fontos, hogy az összes részlista szerepeljen.
sublists :: [a] -> [[a]]
sublists []     = [[]]
sublists (x:xs) = case sublists xs of
  xss -> xss ++ map (x:) xss

-- Osztályok, algebrai típusok
--------------------------------------------------------------------------------

data Color = Red | Green | Blue
data List a = Nil | Cons a (List a)
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- írd meg a következő instance-okat!

instance Eq Color where
  (==) :: Color -> Color -> Bool
  (==) = undefined

instance Ord Color where
  (<=) :: Color -> Color -> Bool
  (<=)  = undefined

instance Show Color where
  show :: Color -> String
  show  = undefined

instance Eq a => Eq (List a) where
  (==) :: List a -> List a -> Bool
  (==) = undefined

instance Show a => Show (List a) where
  show :: List a -> String
  show = undefined

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) = undefined

instance Show a => Show (Tree a) where
  show :: Tree a -> String
  show = undefined



-- Functor
--------------------------------------------------------------------------------

-- írd meg a következő instance-okat!

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap = undefined

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap = undefined

data Twice a = Twice a a deriving (Eq, Show)

instance Functor Twice where
  fmap :: (a -> b) -> Twice a -> Twice b
  fmap = undefined

data Triple a = Triple a a deriving (Eq, Show)

instance Functor Triple where
  fmap :: (a -> b) -> Triple a -> Triple b
  fmap = undefined

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair c) where
  fmap :: (a -> b) -> Pair c a -> Pair c b
  fmap = undefined


-- Bónusz feladatok
--------------------------------------------------------------------------------

-- Add meg a következő definíciókat típushelyesen,
-- végtelen loop és kivételek nélkül!

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = undefined

apply :: Functor f => f (a -> b) -> a -> f b
apply = undefined

first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first = undefined

second :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
second = undefined

data Sum f g a = Inl (f a) | Inr (g a) deriving Show

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap = undefined

data Product f g a = Product (f a) (g a) deriving Show

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap = undefined

newtype Compose f g a = Compose (f (g a)) deriving Show

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap = undefined

newtype Exp x f a = Exp (x -> f a)

instance Functor f => Functor (Exp x f) where
  fmap = undefined

-- Mire használható ez a függvény? Tipp: a megoldáshoz rekurzió szükséges.
löb :: Functor f => f (f a -> a) -> f a
löb = undefined

newtype Fix f = Fix (f (Fix f))

-- A "StandaloneDeriving" opciót használjuk itt, mert
-- a standard "deriving Show" már nem működik.
deriving instance Show (f (Fix f)) => Show (Fix f)

fold :: Functor f => (f a -> a) -> Fix f -> a
fold = undefined
