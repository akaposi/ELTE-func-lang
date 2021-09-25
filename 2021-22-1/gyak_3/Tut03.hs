
{-# language InstanceSigs #-}     -- nyelvi opciók
                                  -- instance metódus típusannotáció

-- részleges mintaillesztésre figyelmeztet
{-# options_ghc -Wincomplete-patterns #-}

------------------------------------------------------------

import Prelude hiding (Either(..), Functor(..), Semigroup(..), Monoid(..))

-- részleges minta warning:
-- foo :: Bool -> Bool
-- foo True = False

------------------------------------------------------------

sumMaybe :: [Maybe Int] -> Int
sumMaybe []            = 0
sumMaybe (Nothing:mis) = sumMaybe mis
sumMaybe (Just n :mis) = n + sumMaybe mis

--
sumMaybe2 :: [Maybe Int] -> Int
sumMaybe2 []       = 0
sumMaybe2 (mi:mis) = case mi of
  Nothing -> sumMaybe mis
  Just n  -> n + sumMaybe mis

-- case-ek 1 sorba írva: ;-el elválasztva:
-- sumMaybe2 (mi:mis) =
--   (case mi of Nothing -> 0; Just n -> n) + sumMaybe mis

------------------------------------------------------------


data Tree a = Leaf a | Node (Tree a) (Tree a)

class Eq' a where
  eq :: a -> a -> Bool

class Show' a where
  show' :: a -> String


instance Eq' a => Eq' (Tree a) where
  eq :: Tree a -> Tree a -> Bool
  eq (Leaf a)   (Leaf a')    = eq a a'
  eq (Node l r) (Node l' r') = eq l l' && eq r r'
  eq _          _            = False

instance Show' a => Show' (Tree a) where
  show' = undefined

-- Semigroup, Monoid, Functor
------------------------------------------------------------

-- Előadásról: Semigroup, Monoid, Functor

infixr 6 <>
class Semigroup a where
  (<>) :: a -> a -> a   -- asszociatív bináris művelet
                        -- x <> (y <> z) = (x <> y) <> z

class Semigroup a => Monoid a where
  mempty :: a           -- Semigroup + egységelem
                        -- mempty <> x = x
                        -- x <> mempty = x

-- standard:
-- instance Semigroup [a] where
--   (<>) = (++)

-- instance Monoid [a] where
--   mempty = []

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- Feladat: írd meg a következő instance-okat! (+feleljen meg az osztály törvényeknek!)
-- Tipp: ugorjuk át a nehezebb feladatokat.

instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (<>) :: (a, b) -> (a, b) -> (a, b)
  (<>) = undefined

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty :: (a, b)
  mempty = undefined

instance Semigroup b => Semigroup (a -> b) where

  (<>) :: (a -> b) -> (a -> b) -> a -> b
  (<>) = undefined

instance Monoid b => Monoid (a -> b) where
  mempty :: a -> b
  mempty = undefined

--------------------------------------------------------------------------------

-- map :: (a -> b) -> [a] -> [b]

-- általánosítás:
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b     -- "f" 1-paraméteres típus!

-- instance Functor Maybe

-- instance Functor [] where
--    fmap :: (a -> b) -> [] a -> [] b    -- Maybe Int     [] Int    [Int]
--    fmap = map

--    fmap :: (a -> b) -> [a] -> [b]    -- szintaktikus cukorka lista típusra!
--    fmap = map

-- min 1 paraméter
--  utolsó típusparaméter fölött map-elünk (Functor instance-al)

data    Foo1 a      = Foo1 Int a a a deriving Show
data    Foo2 a      = Foo2 Bool a Bool
data    Foo3 a      = Foo3 a a a a a
data    Tree1 a     = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show
data    Tree2 a     = Node2 a [Tree2 a] deriving Show
data    Pair a b    = Pair a b
data    Either a b  = Left a | Right b
data    Tree3 i a   = Leaf3 a | Node3 (i -> Tree3 i a)  -- i-szeres elágazás
newtype Id a        = Id a
newtype Const a b   = Const a
newtype Fun a b     = Fun (a -> b)

               -- 4 mező, a típusú mezőt fmap-eljük!
-- data Foo1 a = Foo1 Int a a a deriving Show

instance Functor Foo1 where
  fmap :: (a -> b) -> Foo1 a -> Foo1 b       -- InstanceSigs
  fmap f (Foo1 n x y z) = Foo1 n (f x) (f y) (f z)

instance Functor Foo2 where
  fmap = undefined

instance Functor Foo3 where
  fmap = undefined

instance Functor Tree1 where
  fmap = undefined

-- (Bool, Bool)
-- (True, False)

-- data Pair a b = Pair a b
--   Pair Int Int  : konkrét típus
--   Pair Int      : 1-paraméteres típus

instance Functor (Pair c) where
  fmap :: (a -> b) -> Pair c a -> Pair c b
  fmap f (Pair c a) = Pair c (f a)

-- data Tree2 a = Node2 a [Tree2 a] deriving Show

t1' :: Tree2 Int                          -- Node x [] : "levél"
t1' = Node2 10 [ Node2 12 [], Node2 20 [] ]

-- általánosan: rekurzív típusban egy másik típust használok

instance Functor Tree2 where
  fmap :: (a -> b) -> Tree2 a -> Tree2 b
  fmap f (Node2 a ts) = Node2 (f a) (map (fmap f) ts)
     -- _    :: [Tree2 b]
     -- f    :: a -> b
     -- ts   :: [Tree2 a]
     -- fmap :: (a -> b) -> Tree2 a -> Tree2 b

  -- Mivel Tree2-ben listára hivatkozunk rekurzívan
  -- rekurzív függvényt írok Tree2-n, a listákra vonatkozó függvényt használom fel

-- data Tree3 i a = Leaf3 a | Node3 (i -> Tree3 i a)  -- i-szeres elágazás

-- Tree3 Bool a
-- Node3 :: (Bool -> Tree3 Bool a) -> Tree3 Bool a
--          (Tree3 Bool a, Tree3 Bool a) -> Tree3 Bool a
--  i = Bool --> bináris fa
--  i = 3-elemű típus --> ternáris fa
--  i = ()            --> unáris
--  i = Int           --> 2^64 elágazású fa

t1 :: Tree3 Bool Int
t1 = Node3 (\i -> if i then Leaf3 10 else Leaf3 20)

t2 :: Tree3 Int Int
t2 = Node3 (\i -> Leaf3 i)
   -- i-edik részfa definíciója: Leaf i

t3 :: Tree3 Int Int
t3 = Node3 $ \i -> Node3 $ \j -> Leaf3 (i + j)

-- fmap id x = x    -->  fmap mindig megőrzi a konstruktorokat

instance Functor (Tree3 i) where
  fmap :: (a -> b) -> Tree3 i a -> Tree3 i b
  fmap f (Leaf3 a) = Leaf3 (f a)
  fmap f (Node3 g) = Node3 (\i -> fmap f (g i))
     -- "i-edik map-elt részfa = i-edik eredeti részfa map-elése"
     --

instance Functor (Either a) where
  fmap = undefined

instance Functor Id where
  fmap = undefined

instance Functor (Const a) where
  fmap = undefined

instance Functor (Fun a) where
  fmap = undefined
