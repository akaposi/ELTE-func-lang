{-# language InstanceSigs #-}

-- Típusosztályok + Algebrai adattípusok

------------------------------------------------------------

-- f :: Eq a => a -> a -> Bool
-- f x y = not (x == y)


-- Motiváció: egyenlőségvizsgálat

eqBool :: Bool -> Bool -> Bool
eqBool True True = True
eqBool False False = True
eqBool _ _ = False

eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList eq [] [] = True
eqList eq (x:xs) (y:ys) = eq x y && eqList eq xs ys
eqList _ _ _ = False

eqPair :: (a -> a -> Bool) -> (b -> b -> Bool) -> (a, b) -> (a, b) -> Bool
eqPair eqa eqb (x, y) (x', y') = eqa x x' && eqb y y'

eqListPairBool :: [(Bool, Bool)] -> [(Bool, Bool)] -> Bool
eqListPairBool = eqList (eqPair eqBool eqBool)

-- paraméteres típusok egyenlősége függ a paraméterek egyenlőségétől

class Eq' a where       -- osztály deklaráció
  eq :: a -> a -> Bool  -- metódus (egy vagy több metódus egy osztályban)


-- instance: implementáció valamilyen konkrét típusra
instance Eq' Bool where
  eq :: Bool -> Bool -> Bool
  eq = eqBool

-- instance megszorítás (constraint)
instance Eq' a => Eq' [a] where

  -- eq :: [a] -> [a] -> Bool
  -- eq = eqList eq

  eq :: [a] -> [a] -> Bool
  eq []     []     = True
  eq (x:xs) (y:ys) = eq x y    &&   eq xs ys
                  -- "a" eq-ja     "[a]" eq-ja
  eq _      _      = False

-- több megszorítás: zárójelben, vesszővel elválasztva
instance (Eq' a, Eq' b) => Eq' (a, b) where
  eq (x, y) (x', y') = eq x x' && eq y y'

eqListPairBool' :: [(Bool, Bool)] -> [(Bool, Bool)] -> Bool
eqListPairBool' = eq
  -- [(Bool, Bool)]

-- Eq a   : metódusa (==)
-- Ord a  : (<), (<=), stb.

-- class Show a where
--   show :: a -> String


-- Algebrai adattípusok
------------------------------------------------------------

-- enumerációk:

data Color = Red | Blue | Green

-- Color            : típus konstruktor
-- Red, Green, Blue : adat konstruktorok

-- megkapom a mintaillesztést erre a típusra

f :: Color -> Int
f Red   = 0
f Green = 1
f Blue  = 2

instance Show Color where
  show Red   = "Red"
  show Green = "Green"
  show Blue  = "Blue"

-- konstruktorokban lehet adatot tárolni

              -- két darab Int típusú mező
data PairInt = MkPairInt Int Int
  -- MkPairInt :: Int -> Int -> PairInt

g :: PairInt -> Int
g (MkPairInt x y) = x + y

-- paraméteres "data"
-- Pl: lista típus újradefiniálása:

-- List "egy paraméteres típuskonstruktor"
-- "rekurzív" adattípus
data List a = Nil | Cons a (List a)

list1 :: List Int
list1 = Nil         -- []

list2 :: List Int
list2 = Cons 100 (Cons 200 (Cons 300 Nil))
     -- [100, 200, 300]
     -- 100 : 200 : 300 : []
     -- (:) 100 ((:) 200 ((:) 300 []))
   -- (+) 10 20  -- 10 + 20

map' :: (a -> b) -> List a -> List b
map' f Nil         = Nil
map' f (Cons a as) = Cons (f a) (map' f as)

-- standard lista verzió
-- map f []       = []
-- map f (a : as) = f a : map f as

-- bináris fa típusa

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show, Ord)

t1 :: Tree Int
t1 = Leaf 100

t2 :: Tree Int
t2 = Node (Leaf 10) (Leaf 20)

t3 :: Tree Int
t3 = Node (Node t2 t2) t1

t4 :: Tree Int
t4 = Node
       (Node
         (Leaf 0)
         (Leaf 1))
       (Node
         (Leaf 3)
         (Leaf 4))

------------------------------------------------------------

-- Miért "algebrai" adattípus?

-- Bizonyos paraméteres típusok ~ aritmetikai műveletknek
--  (lehetséges értékek számát tudjuk megkapni)

-- típus egy értékkel
data One = One deriving (Show)  -- a típus "One" nem ugyanaz mint az érték
                                -- "One"

-- enum 1 értékkel

-- típus 0 értékkel
data Zero

-- jelölés: ha "a" típus, akkor |a| jelentse az "a" értékeinek számát
--    |Zero| = 0
--    |One|  = 1

-- azt szeretném, hogy |Mul a b| = |a| * |b|
data Mul a b = Pair a b   -- (a, b)
                          -- matek: pár típus "szorzat" halmaz

data Add a b = Inl a | Inr b

add :: Add Bool Bool
add = Inl False

add1 :: Add Bool Bool
add1 = Inr True

-- Inl True, Inl False, Inr True, Inr False  (4 darab)
-- 2 + 2 = 2 * 2
-- |Add Color Color| = |Color| + |Color| = 3 + 3 = 6

-- |Exp a b| = |a|^|b|
data Exp a b = Fun (b -> a)

-- hatványhalmaz:
-- |a -> Bool| = |Bool|^|a| = 2^|a|

-- aritmetikai azonosságok típusokra is igazak!

-- a, b típusok,  hogy |a| = |b|
-- mindig tudunk oda-vissza konvertálni

-- a * b = b * a
swap :: Mul a b -> Mul b a
swap (Pair x y) = Pair y x

-- a * 1 = a
to :: Mul a One -> a
to (Pair a _) = a

from :: a -> Mul a One
from a = Pair a One

-- a * (b + c) = a * b + a * c
to1 :: Mul a (Add b c) -> Add (Mul a b) (Mul a c)
to1 (Pair a (Inl b)) = Inl (Pair a b)
to1 (Pair a (Inr c)) = Inr (Pair a c)

from1 :: Add (Mul a b) (Mul a c) -> Mul a (Add b c)
from1 (Inl (Pair a b)) = Pair a (Inl b)
from1 (Inr (Pair a c)) = Pair a (Inr c)

getA :: Mul a (Add b c) -> a
getA (Pair a _) = a

getA' :: Add (Mul a b) (Mul a c) -> a
getA' (Inl (Pair a _)) = a
getA' (Inr (Pair a _)) = a

 -- f :: (a, b, c) -> d
 -- f :: a -> (b -> (c -> d))    -- (->) jobbra zárójelez
 -- f :: a -> b -> c -> d

--   c^(a*b) = (c^b)^a
curry' :: ((a, b) -> c) -> (a -> (b -> c))
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (a, b) = f a b
