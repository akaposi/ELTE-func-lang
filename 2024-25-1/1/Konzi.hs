module Konzi where

{-
A haskell
LUSTA (!!), pl végtelen lista

pl fromIntegral
nincsenek implicit típuskonverziók
v
ERŐSEN STATIKUSAN TÍPUSOZOTT
         ^
         fordítási időben létezik típusa minden részkifejezésnek

FUNKCIONÁLIS
^ minden kifejezés egy függvény
-}

-- pl.:

g :: Int
g = div 1 0
--   ^ prefix

f :: Int
f = head [0, 1 `div` 0]
--               ^ infix

-- read :: Read a => String -> a (parseol)
-- show :: Show a => a -> String (kiír)

{-

MINTAILLESZTÉS & POLIMORFIZMUS

-}

not' :: Bool -> Bool -- { True -> False, False -> True }
not' True = False
not' False = True

h :: Int -> Double
h a = fromIntegral a / 3


-- Totális
-- Parciális (nem fedi le az összes bemenetet)


id' :: a -> a
id' x = x

--                  v
const' :: Eq a => a -> b -> a
const' x _ = x

i :: Ord a => a -> a -> Bool
i a b = a == b

-- Algebrai adattípusok
data MyFavouriteType = A Int | B Double | C Char Char Char | Q Int deriving Show

m :: MyFavouriteType -> MyFavouriteType
m (Q b) = A b
m (A almafa) = (B 0)
m (B balma) = (C 'a' 'b' 'c')
m (C c1 c2 c3) = (C c2 c3 c1)

add1 :: [Int] -> [Int]
add1 [] = []
add1 (x : xs) = x + 1 : add1 xs

add1' :: [Int] -> [Int]
add1' xs = add1helper xs []

add1helper :: [Int] -> [Int] -> [Int]
add1helper [] xs = xs
add1helper (x : xs) ys = add1helper xs (x + 1 : ys)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

data BinTree a = Leaf a | Node (BinTree a) a (BinTree a) deriving Show


add1BT :: BinTree Int -> BinTree Int
add1BT (Leaf x) = Leaf (x + 1)
add1BT (Node l a r) = Node (add1BT l) (a + 1) (add1BT r)
