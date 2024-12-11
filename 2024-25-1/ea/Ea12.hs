{-# LANGUAGE ImpredicativeTypes #-}
import Prelude hiding (Bool, not, and, showList, pred, head, tail)

-- ez 18 perccel rovidebb

-- (Nat -> Bool) -> Bool   <- ezek egyenlosege eldontheto (Seemingly impossible functional programs)

-- Haskell totalis resze (Elementary strong functional programming (David Turner))

-- ket tipus: (->), forall
-- pl. id :: forall a . a -> a
--     (\x y -> x), (\x y -> y) :: forall a . a -> a -> a
{-
Ty : Set
Tm : Ty → Set
_⇒_ : Ty → Ty → Ty
lam : (Tm A → Tm B) ≅ Tm (A ⇒ B) : app
∀   : (Ty → Ty) → Ty
Lam : ((A:Ty) → Tm (F A)) ≅ Tm (∀ A) : App
-}
-- System F = totalis reszhalmaza a Haskellnek, es ebben "mindent" meg lehet csinalni, amit Haskellben is
-- Haskell eredetileg a System F egy reszhalmazara epult (Hindley-Milner)
-- Bool -> (forall a. a -> a) -> Int   <- ezt System F-ben lehet irni, HM-ben nem

f :: Bool -> (forall a. a -> a) -> Int
-- f b g = g @Int 3
f = \b g -> g @Int 3

f' :: forall a. Bool -> (a -> a) -> Int
-- f' :: Bool -> (a -> a) -> Int
-- f' b g = g 3
-- g :: a -> a
f' b g = 3

-- Haskell = System F + fixpont-kombinator + rekurziv tipusok

fix :: (a -> a) -> a
fix = \f -> f (fix f)

-- System F mindenre eleg:

type Bool = forall a . a -> a -> a
true, false :: Bool
true = \x y -> x
false = \x y -> y
ite :: Bool -> forall c . c -> c -> c
-- ite :: Bool -> Bool -- forall c . c -> c -> c
-- ite b t f = b t f
-- ite b t = b t
-- ite b = b
-- ite = \b -> b
ite = id

not :: Bool -> Bool
not b = ite b false true

showBool :: Bool -> String
showBool b = b "true" "false"

and :: Bool -> Bool -> Bool
and x y = ite x y false

type Three = forall a . a -> a -> a -> a

-- Church kodolt Peano termeszetes szamok
type Nat = forall a . a -> (a -> a) -> a
n0, n1, n2, n3 :: Nat
n0 = \z s -> z
n1 = \z s -> s z
n2 = \z s -> s (s z)
n3 = \z s -> s (s (s z))
zero :: Nat
zero = \z s -> z
suc  :: Nat -> Nat
suc n = \z s -> s (n z s)

-- suc n2 =
-- \z s.s (n2 z s) =
-- \z s.s (s (s z)) =
-- n3

-- data Nat = Zero | Suc Nat
-- Peano termeszetes szamok

iteNat :: Nat -> forall c. c -> (c -> c) -> c
iteNat = id
{-
recNat :: Nat -> forall c. c -> (Nat -> c -> c) -> c
recNat n z s = snd $ iteNat @(Nat,_) n
  (zero,z)
  (\(n,c)->(suc n,s n c))
-}

pred :: Nat -> Nat
pred n = snd $ iteNat n @(Nat,Nat) (zero,zero) (\(n,_) -> (suc n,n))

-- add Zero    y = y
-- add (Suc x) y = Suc (add x y)
add :: Nat -> Nat -> Nat
add x y = iteNat x y suc

showNat :: Nat -> String
showNat n = show $ n @Int 0 (+1)

toNat :: Int -> Nat
toNat n | n < 0 = error "negative number"
toNat 0 = zero
toNat n = suc (toNat (n-1))

-- mul Zero    y = Zero
-- mul (Suc x) y = y + mul x y
-- (1+x)*y = 1*y + x*y = y + x*y
mul :: Nat -> Nat -> Nat
mul x y = iteNat x zero (add y)

-- pred :: Nat -> Nat
-- pred Zero = Zero
-- pred (Suc n) = n


-- [] :: * -> *
type List a = forall b . b -> (a -> b -> b) -> b
nil :: List a
nil = \n c -> n
-- aList = [1,2,4] = 1:2:4:[]
aList, aList' :: List Int
aList = \n c -> c 1 (c 2 (c 4 n))
aList' = cons 1 (cons 2 (cons 5 nil))

cons :: a -> List a -> List a
cons a as = \n c -> c a (as n c)
-- n :: b
-- c :: (a -> b -> b)
-- ? :: b
-- a :: a
-- as :: List a
-- as :: forall b . b -> (a -> b -> b) -> b

showList :: Show a => List a -> String
showList xs = xs @String
  "[]"
  (\a s -> show a ++ ":" ++ s)

-- foldr = iteList
-- foldr :: (a -> c -> c) -> c -> [ga] -> c
iteList :: List a -> forall c . c -> (a -> c -> c) -> c
iteList = id

-- conc Nil         ys = ys
-- conc (Cons x xs) ys = Cons x (conc xs ys)
-- conc :: List Int -> List Int -> List Int
conc :: List a -> List a -> List a
conc xs ys =
  iteList xs @(List _) ys (\a xsys -> cons a xsys)

-- parametricitas (parametricity), termeszetsseg (naturality), uniformity
-- forall a . a -> a                      ()
-- forall a . a                           Empty
-- forall a . a -> (a -> a)               Bool
-- forall c . (a -> b -> c) -> c          (a,b)
-- forall c . (a -> c) -> (b -> c) -> c   Either a b
-- forall c . (a -> a) -> a               data List = Cons List

-- exists (\s.(s -> Int , s -> s , s))     Stream Int
-- exists f = forall c . (forall x . f x -> c) -> c

-- type Stream a = exists s.(s -> a , s -> s , s)
type Stream a = forall c .
   (forall s . (s -> a , s -> s , s) -> c) -> c

head :: Stream a -> a
head s = s (\(h,t,s) -> h s)

tail :: Stream a -> Stream a
tail s = s (\(h,t,s) -> \f -> f (h,t,t s))

-- az a stream, amiben a termeszetes szamok bent vannak 0-tol kezdve
nats :: Stream Nat
nats = \f -> f @Nat (id,suc,zero)

ex :: String
ex = showNat (head @Nat (tail @Nat (tail @Nat nats)))
