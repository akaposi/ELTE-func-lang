
-- Semigroup, Monoid, Functor
------------------------------------------------------------

import Prelude hiding (Either(..), Functor(..), Semigroup(..), Monoid(..))

infixr 6 <>
class Semigroup a where
  (<>) :: a -> a -> a                  -- asszociatív kombiniálása az értékeknek

class Semigroup a => Monoid a where
  mempty :: a                          -- mempty egységeleme a (<>) műveletnek    (mempty <> x = x   és  x <> mempty = x)

class Functor f where
  fmap :: (a -> b) -> f a -> f b       -- fmap id x = x     és    fmap (f . g) x = fmap f (fmap g x)


-- Feladat: írd meg a következő instance-okat! (+megfelel az osztály törvényeknek!)

instance Semigroup [a] where
  (<>) = (++)      -- parciális applikáció: 0 darab argumentum
  -- xs <> ys = xs ++ ys

instance Monoid [a] where
  mempty = []

-- instance Eq a => Eq (Maybe a) where
--   Just x == Just y = (x == y)
--   Nothing == Nothing = True
--   _ == _ = False

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (<>) = undefined
  --  felhasználhatjuk a <> művelet a és b típuson

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = undefined

instance Semigroup b => Semigroup (a -> b) where
  (<>) = undefined

instance Monoid b => Monoid (a -> b) where
  mempty = undefined


-- Feladat: írj Functor instance-t az összes alábbi típushoz!
-- Általánosan fmap : függvényt alkalmazzuk az utolsó típusparaméter összes előfordulásán
--                    az adatstruktúrában.


data    Foo1 a      = Foo1 Int a a a
data    Foo2 a      = Foo2 Bool a Bool
data    Foo3 a      = Foo3 a a a a a
data    Tree1 a     = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show  -- data List a = Nil | Cons a (List a)    rekurzív típus
data    Tree2 a     = Node2 a [Tree2 a] deriving Show -- listában vannak a részfák (0 vagy több részfa van)
                                         --
data    Pair a b    = Pair a b
data    Either' a b = Left' a | Right' b
data    Tree3 i a   = Leaf3 a | Node3 (i -> Tree3 i a)
newtype Id a        = Id a
newtype Const a b   = Const a
newtype Fun a b     = Fun (a -> b)

-- bónusz feladat: foldl függvényt definiálni listára, úgy hogy csak foldr-t és lambda kifejezést használva (rekurziót sem lehet)

instance Functor Foo1 where
  -- fmap :: (a -> b) -> Foo1 a -> Foo1 b
  fmap f (Foo1 n x y z) = Foo1 n (f x) (f y) (f z)

instance Functor Tree1 where
  -- fmap :: (a -> b) -> Tree1 a -> Tree1 b
  fmap f (Leaf1 a)   = Leaf1 (f a)
  fmap f (Node1 l r) = Node1 (fmap f l) (fmap f r)

instance Functor (Pair a) where    -- extra (utolsó) paraméter fölött map-elünk
  -- fmap :: (b -> c) -> Pair a b -> Pair a c
  fmap f (Pair a b) = Pair a (f b) -- (második mezőnek van b típusa)

-- példa :
t1 :: Tree1 Int
t1 = Node1 (Leaf1 100) (Leaf1 200)

t2 :: Tree1 Bool
t2 = fmap even t1  -- Node1 (Leaf1 True) (Leaf1 True)

instance Functor Foo2 where
  fmap = undefined

instance Functor Foo3 where
  fmap = undefined

leaf2 :: Tree2 Int
leaf2 = Node2 1000 []     -- levél konstruktor

tree2 :: Tree2 Int
tree2 = Node2 100 [leaf2, leaf2]

tree3 :: Tree2 Int
tree3 = Node2 100 [tree2, tree2, tree2]

instance Functor Tree2 where
  -- konstruktorok nem változnak!
  fmap f (Node2 a ts) = Node2 (f a) (map (fmap f) ts)   -- ts :: [Tree2]
                           -- (lista map) (Tree2 fmap)

-- data Tree3 i a = Leaf3 a | Node3 (i -> Tree3 i a)
-- tᵢ részfa minden i-re

tree4 :: Tree3 Int Int
tree4 = Node3 (\i -> Leaf3 i)

--                                                           Node3
--                                         ... Leaf3 (-1), Leaf3 0, Leaf3 1, Leaf3 2, ...

-- házi feladat lerajzolni (vizualizálni mint fa)
tree5 :: Tree3 Int Int
tree5 = Node3 (\i -> Node3 (\j -> Leaf3 (i + j)))

instance Functor (Tree3 i) where
  -- fmap f (Node3 g) = Node3 (fmap f . g)       --
  fmap f (Node3 g) = Node3 (\i -> fmap f (g i))  -- minden i-re, map-elem az i-edik részfát

instance Functor (Either' a) where
  fmap = undefined

instance Functor Id where
  fmap = undefined

instance Functor (Const a) where
  fmap = undefined

instance Functor (Fun a) where
  fmap = undefined
