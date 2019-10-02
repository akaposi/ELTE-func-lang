{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (Foldable(..))

-- Foldable, lista + fa feladatok
------------------------------------------------------------

-- osztály fold-olható típusokra
-- emlékezzünk: lista foldr :: (a -> b -> b) -> b -> [a] -> b
class Foldable f where
  foldr :: (a -> b -> b) -> b -> f a -> b


-- 1. Írd meg a következő instance-okat.

data Two a = Two a a deriving (Show)
data Three a = Three a a a deriving (Show)
data Id a = Id a deriving (Show)
data Const a b = Const a deriving (Show)

-- példa megoldás. Általánosan az a feladat, hogy az összes "a" típusú
-- elemet f-el kombináljuk egy addot adattípusban.
instance Foldable Two where
  foldr f b (Two a1 a2) = f a1 (f a2 b)

instance Foldable Three where
  foldr = undefined

instance Foldable [] where
  foldr = undefined

instance Foldable Maybe where
  foldr = undefined

instance Foldable (Const a) where
  foldr = undefined

instance Foldable Id where
  foldr = undefined


-- 2. Írd meg a következő függvényeket, amelyek tetszőleges Foldable
--    típuson működnek! Teszteld a megoldásokat a korábban megadott
--    instance-okkal!

-- üres-e?
isEmpty :: Foldable f => f a -> Bool
isEmpty = undefined

-- a-típusú elemek száma (f a)-ban.
length :: Foldable f => f a -> Int
length = undefined

toList :: Foldable f => f a -> [a]
toList = undefined

-- Kombináljuk (<>) segítségével az összes a-t (f a)-ban.
-- Példa: mconcat' ["foo", "bar"] = "foobar"
mconcat' :: (Foldable f, Monoid a) => f a -> a
mconcat' = undefined

foldl' :: Foldable f => (b -> a -> a) -> b -> f a -> b
foldl' = undefined

-- 3. (bónusz) írd meg a következő instance-okat!

data Tree1 a = Node1 a [Tree1 a] deriving (Show)
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a) deriving (Show)

instance Foldable Tree1 where
  foldr = undefined

instance Foldable Tree2 where
  foldr = undefined


-- 4. Írd meg a következő függvényeket típushelyesen!

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = undefined

apply :: Functor f => f (a -> b) -> a -> f b
apply = undefined

first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first = undefined

second :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
second = undefined


-- 5. (BÓNUSZ) Írd meg a következő függvényt. A megoldáshoz mindenképp szükség
--    van rekurzióra. Mire jó ez a függvény? Keress példát használatra listák
--    esetén, azaz ha löb :: [[a] -> a] -> [a]

löb :: Functor f => f (f a -> a) -> f a
plöb = undefined


-- 6. feladat:

-- Reprezentáljuk az irányított gráfokat a következő típussal:

type Graph = [(Int, [Int])]

-- Minden (Int, [Int]) megadja, hogy az első Int-el számozott
-- csúcsból milyen más csúcsokba megy él. Definiáld a "dfs :: Graph ->
-- Int -> [Int]" függvényt, ami megadja egy adott gráf adott csúcsából
-- kiindulú mélységi bejárást. Példa:

--   dfs [(0, [1]), (1, [0])] 1 == [1,0]
--   dfs [(0, [1]), (1, [3, 2]), (2, [2]), (3, [0])] 0 == [0,1,3,2]

dfs :: Graph -> Int -> [Int]
dfs = undefined
