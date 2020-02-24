
-- standard: Either
data Either' a b = Left' a | Right' b

-- hogyan működik:
e1 :: Either' Bool Int
e1 = Left' True

e2 :: Either' Bool Int
e2 = Right' 100

-- mintaillesztéssel tudunk függvényt definiálni Either'-en
f1 :: Either' Bool Int -> Either' Bool Int
f1 (Left' b)  = Right' (if b then 0 else 1)
f1 (Right' n) = Right' n

-- 1. feladat
instance (Eq a, Eq b) => Eq (Either' a b) where
  (==) = undefined

-- 2. feladat
instance (Show a, Show b) => Show (Either' a b) where
  show = undefined

-- többi feladat:
-- github repo: /gyak_1/gyakfeladatok_01.hs
--  f1-től f9-ig feladatok
