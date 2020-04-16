
import Prelude hiding (Foldable(..))

-- Foldable, lista + fa feladatok
------------------------------------------------------------

-- osztály fold-olható típusokra
-- emlékezzünk: lista foldr :: (a -> b -> b) -> b -> [a] -> b
class Foldable f where
  foldr :: (a -> b -> b) -> b -> f a -> b


-- 1. Írd meg a következő instance-okat.
data Two a     = Two a a deriving (Show)
data Three a   = Three a a a deriving (Show)
data Id a      = Id a deriving (Show)
data Const a b = Const a deriving (Show)

instance Foldable Two where
  foldr = undefined

instance Foldable Three where
  foldr = undefined

instance Foldable (Const a) where
  foldr = undefined

instance Foldable Id where
  foldr = undefined

instance Foldable [] where
  foldr = undefined

instance Foldable Maybe where
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

elem :: (Foldable f, Eq a) => a -> f a -> Bool
elem = undefined

-- Kombináljuk (<>) segítségével az összes a-t (f a)-ban.
-- Példa: mconcat' ["foo", "bar"] = "foobar"
mconcat' :: (Foldable f, Monoid a) => f a -> a
mconcat' = undefined


-- 3. Írd meg a következő instance-okat!
------------------------------------------------------------

data Tree1 a = Node1 a [Tree1 a] deriving (Show)

instance Foldable Tree1 where
  foldr = undefined

data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a) deriving (Show)

instance Foldable Tree2 where
  foldr = undefined
