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
  foldr f b (Three x y z) = f x (f y (f z b))

instance Foldable [] where
  foldr f b []     = b
  foldr f b (a:as) = f a (foldr f b as)

instance Foldable Maybe where
  foldr f b Nothing  = b
  foldr f b (Just a) = f a b

instance Foldable (Const a) where
  foldr f b (Const a) = b

instance Foldable Id where
  foldr f b (Id a) = f a b


-- 2. Írd meg a következő függvényeket, amelyek tetszőleges Foldable
--    típuson működnek! Teszteld a megoldásokat a korábban megadott
--    instance-okkal!

-- üres-e?
isEmpty :: Foldable f => f a -> Bool
isEmpty = foldr (\_ _ -> False) True

-- a-típusú elemek száma (f a)-ban.
length :: Foldable f => f a -> Int
length = foldr (\_ -> (+1)) 0

toList :: Foldable f => f a -> [a]
toList = foldr (:) []

elem :: (Foldable f, Eq a) => a -> f a -> Bool
elem a fa = foldr (\a' b -> a == a' || b) False fa

-- Kombináljuk (<>) segítségével az összes a-t (f a)-ban.
-- Példa: mconcat' ["foo", "bar"] = "foobar"
mconcat' :: (Foldable f, Monoid a) => f a -> a
mconcat' = foldr (<>) mempty

-- bónusz feladat: foldl foldr-el
foldl' :: Foldable f => (b -> a -> b) -> b -> f a -> b
foldl' f b fa = foldr (\a k b -> k (f b a)) id fa b

-- csaló verzió
foldl'' :: Foldable f => (b -> a -> b) -> b -> f a -> b
foldl'' f b = go b . toList where
  go b []     = b
  go b (a:as) = go (f b a) as


-- 3. (bónusz) írd meg a következő instance-okat!

data Tree1 a = Node1 a [Tree1 a] deriving (Show)
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a) deriving (Show)

instance Foldable Tree1 where
  foldr f b (Node1 a ts) = f a (foldr (\t b -> foldr f b t) b ts)

instance Foldable Tree2 where
  foldr f b (Leaf2 a)   = f a b
  foldr f b (Node2 l r) = foldr f (foldr f b r) l

-- 4. Írd meg a következő függvényeket típushelyesen!
-- (<$>) ugyanaz mint fmap, infix verzió
funzip :: Functor f => f (a, b) -> (f a, f b)
funzip fab = (fst <$> fab, snd <$> fab)

apply :: Functor f => f (a -> b) -> a -> f b
apply ff a = fmap (\f -> f a) ff -- ($ a) <$> ff

first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first f (a, c) = (\b -> (b, c)) <$> f a

second :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
second f (c, a) = (\b -> (c, b)) <$> f a


-- 5. (BÓNUSZ) Írd meg a következő függvényt. A megoldáshoz mindenképp szükség
--    van rekurzióra. Mire jó ez a függvény? Keress példát használatra listák
--    esetén, azaz ha löb :: [[a] -> a] -> [a]

löb :: Functor f => f (f a -> a) -> f a
löb ffa = let res = fmap ($ res) ffa in res

-- példa: lista celláinak Excel-szerű kiértékelése.
-- minden cella hivatkozhat a lista bármely elemére.
l1 :: [[Int] -> Int]
l1 = [const 10, const 20, \l -> l!!0 + l!!1, \l -> l!!2 * 20]

l1' = löb l1  -- == [10,20,30,600]


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
dfs g = reverse . go []
  where
  go visited i | elem i visited = visited
  go visited i = case lookup i g of
    Just is -> foldl' go (i:visited) is
    Nothing -> error "impossible"
