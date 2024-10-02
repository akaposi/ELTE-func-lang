import Prelude

-- 3 :: Int :: * :: * :: * ...
-- System F (Girard), polimorf lambda kalkulus (Reynolds)
-- Per Martin-Löf: dependent type theory , Set : Set₁ : Set₂ : ...
-- 3 : Nat : Set : Set <- függő típuselmélet első változata (System U)
-- elsőrendű Peano -> Martin-Löf függő típuselmélet -> System F programok terminálása = másodrendű Peano aritmetika -> ZF halmazelmélet -> NBG-halmazelmélet -> ... -> ... 

-- constraint: amit => ele lehet irni
-- id :: a -> a
-- (+) :: Num a => a -> a -> a
-- (+) (instanceNumInt) 3 4 = 7

-- Eq

-- (==) :: a -> a -> Bool

data Tree a = Node (Tree a) (Tree a) | Leaf a
  deriving (Show)

furaFa :: Tree Int
furaFa = Node furaFa furaFa

{-
    _________
   /         v
| furaFa | Node | p1 | p2 | ...
    ^\^\__________/     /
      \________________/
-}

instance Eq a => Eq (Tree a) where
  Node t1 t2 == Node t1' t2' = t1 == t1' && t2 == t2'
  Node _  _  == Leaf _       = False
  Leaf _     == Node _ _     = False
  Leaf a1    == Leaf a2      = a1 == a2

main = print $ furaFa == furaFa

{-
a && a

"common subexpression elimination" optimalizalo technika

let x = furaFa == furaFa in x && x


furaFa == furaFa =(rov.felold)
Node furaFa furaFa == Node furaFa furaFa =(== def)
furaFa == furaFa && furaFa == furaFa =(rov.felold)
Node furaFa furaFa == Node furaFa furaFa && furaFa == furaFa =
(furaFa == furaFa && furaFa == furaFa) && furaFa == furaFa =(...)

ha van optimalizacio:
furaFa == furaFa =(rov.felold)
Node furaFa furaFa == Node furaFa furaFa =(== def)
furaFa == furaFa && furaFa == furaFa =(optim)
let x = furaFa == furaFa in x && x =
let x = (furaFa == furaFa && furaFa == furaFa) in x && x =(optim)
let x = (let y = furaFa == furaFa in y && y) in x && x =(...)
...

mindenkepp betelik a memoria

HF: Haskell program, ami orokke fut es betelik a memoria
HF: Haskell program, ami orokke fut es a memoria konstans
-}

ex1 :: Bool
ex1 = Node (Node (Leaf ()) (Leaf ())) (Leaf ()) ==
  Node (Leaf ()) (Node (Leaf ()) (Leaf ()))

{-
  / \          /\
 /\  ()  =?     /\
() ()
-}

{-
-- most csak a pozitivakra gondolunk
instance Eq (Integer -> Bool) where
  -- f == g = f 0 == g 0 && f 1 == g 1 && f 2 == g 2 && ...
  f == g = go 0
    where
      go i | f i == g i = go (i+1)
           | otherwise  = False

instance Eq (Int -> Bool) where
  -- f == g = f 0 == g 0 && f 1 == g 1 && f 2 == g 2 && ...
  f == g = go 0
    where
      go i | i == maxBound = f i == g i
           | f i == g i = go (i+1)
           | otherwise  = False
-}

-- f1, f2 :: Int -> Bool
f1, f2 :: Integer -> Bool
f1 i = odd i
-- f2 i = odd i
-- f2 i | i == 314869450928509832 = False
f2 i | i == 500000 = True
     | otherwise = odd i

start :: Enum a => a
start = toEnum 0

instance (Enum a, Eq b) => Eq (a -> b) where
  -- nem mukodik veges tipusokra
  f == g = go start
    where
      go :: a -> Bool
      go i | f i == g i = go (succ i)
           | otherwise  = False

-- Enum kizarolag a .. notation miatt van

g1, g2 :: Bool -> Bool
g1 = not
g2 = not

-- g1 == g2 exceptiont ad

-- const True == constr True vegtelen ciklust ad

-- (\x -> x+x) (1+2) = (\x -> x+x) 3 = 3+3 = 6  -- CBV, ertek szerinti parameteratadas, strict evaluation
-- (\x -> x+x) (1+2) = (1+2)+(1+2) = 3+(1+2) = 3+3 = 6  -- call by name, lazy evaluation, non-strict evaluation
-- (\x -> 4) (1+2) = (\x -> 4) 3 = 4 -- CBV
-- (\x -> 4) (1+2) = 4 -- call by name

-- (\x -> x+x) (1+2) = (1+2)+(1+2) = 3+3 = 6 -- call by need, szukseg szerinti kiertekeles, lazy
{-
(\x -> x+x) (1+2) = ·+· =   ·+· = 6
                    | |     | | 
                     \|      \| 
                     VV      VV 
                     1+2      3        -}

szam :: Int
szam = (1+2)+(1+2) -- let x = (1+2) in x+x  -- common subexpression elimination

-- Int, unboxed Int

go :: Bool -> Bool
go i | g1 i == g2 i = go (succ i)
     | otherwise  = False

{-
go :: Bool -> Bool
go i = case g1 i == g2 i of
  True -> go (succ i)
  _    -> False
-}
