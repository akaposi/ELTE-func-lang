{-# OPTIONS_GHC -Wincomplete-patterns #-}
module Notes02 where
import Data.Functor.Contravariant

data Alma = A Int Int -- |Alma| = |Int| * |Int|


getInt1 :: Alma -> Int
getInt1 (A 10 i2) = i2
getInt1 _ = 10

data Felsorolas = B | C | D | E

felsorolasToNum :: Felsorolas -> Int
felsorolasToNum B = 3
felsorolasToNum C = 4
felsorolasToNum D = 5
felsorolasToNum E = 6

data Colour = RGB Int Int Int | HSL Int Int Int | HSV Int Int Int
-- (|Int| * |Int| * |Int|) + (|Int| * |Int| * |Int|) + (|Int| * |Int| * |Int|)

data Product a b = Product a b
data Sum a b = Left' a | Right' b

-- |Product a b| = |A| * |B|
-- |Sum a b| = |A| + |B|
-- |a -> b| = |B| ^ |A|

exampleSum :: Sum Bool Int
exampleSum = Left' True

{-

class Product<A,B> {
  A left;
  B right;
}

-}

data List a = Nil | Cons a (List a)
-- |List a| = 1 + (|a| * |List a|)

-- Típusosztályok
data Gyumolcs = Alma | Banan | Korte
  deriving Show

compareObjects :: Eq a => a -> a -> Bool
compareObjects a b = a == b

class Eq' a where
  eq :: a -> a -> Bool
  eq a b = not (neq a b)
  neq :: a -> a -> Bool
  neq a b = not (eq a b)

  tripleeq :: a -> a -> a -> Bool
  tripleeq a b c = eq a b && eq b c

  -- a, b => a ÉS b szükséges
  -- a | b => a VAGY b szükséges

  {-# MINIMAL eq | neq #-}


--- Instance írás
instance Eq Gyumolcs where
  Alma == Alma = True
  Banan == Banan = True
  Korte == Korte = True
  _ == _ = False


-- Ord előfeltétele Eq
-- nem engedi meg
---instance Ord Colour where


--- Kind rendszer

-- maybe' :: Maybe a -> b -> (a -> b) -> b
-- maybe' :: f a -> b -> (a -> b) -> b
-- Higher Kinded / Magasabbrendű Polimorfizmus

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

map'' :: (a -> b) -> Maybe a -> Maybe b
map'' f Nothing = Nothing
map'' f (Just a) = Just (f a)


----------------             q az fix
map''' :: (a -> b) -> Either q a -> Either q b
map''' f (Left q) = Left q
map''' f (Right a) = Right (f a)


--- :i Functor
{-

fmap :: (a -> b) -> f a -> f b

f = (->) r

fmap :: (a -> b) -> (->) r a -> (->) r b
fmap :: (a -> b) -> (r -> a) -> r -> b

Mi van ha a másodikat "fixáljuk" le?

"fmap" :: (a -> b) -> (a -> r) -> b -> r

kovariancia/kontravariancia
"kimeneti" / "bemeneti"
Functor

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}

Contravariant
class Contravariant f where
  contramap :: (a' -> a) -> f a -> f a'
  (>$) :: b -> f b -> f a
  {-# MINIMAL contramap #-}

-}

phantom :: (Contravariant f, Functor f) => f a -> f b
phantom x = contramap (\_ -> True) (fmap (\_ -> True) x)

-- Const
data Const a b = MkConst { getConst :: a }
