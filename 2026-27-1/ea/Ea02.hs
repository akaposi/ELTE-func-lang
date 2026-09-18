{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{- HLINT ignore "Use newtype instead of data" -}
-- Functor

-- >>> :i Functor 
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--   (<$) :: a -> f b -> f a
--   {-# MINIMAL fmap #-}
--   	-- Defined in ‘GHC.Internal.Base’
-- instance Functor ((->) r) -- Defined in ‘GHC.Internal.Base’
-- instance Functor IO -- Defined in ‘GHC.Internal.Base’
-- instance Functor [] -- Defined in ‘GHC.Internal.Base’
-- instance Functor Maybe -- Defined in ‘GHC.Internal.Base’
-- instance Functor Solo -- Defined in ‘GHC.Internal.Base’
-- instance Functor ((,) a) -- Defined in ‘GHC.Internal.Base’
-- instance Functor ((,,) a b) -- Defined in ‘GHC.Internal.Base’
-- instance Functor ((,,,) a b c) -- Defined in ‘GHC.Internal.Base’
-- instance Functor ((,,,,) a b c d) -- Defined in ‘GHC.Internal.Base’
-- instance Functor ((,,,,,) a b c d e)
--   -- Defined in ‘GHC.Internal.Base’
-- instance Functor ((,,,,,,) a b c d e f)
--   -- Defined in ‘GHC.Internal.Base’
-- instance Functor (Either a)
--   -- Defined in ‘GHC.Internal.Data.Either’

-- (<$) a fb = fmap (const a) fb
--
-- Higher-Kinded / Higher-Order Polymorphism
--


-- >>> :k Int 
-- Int :: *
-- >>> :k Either
-- Either :: * -> * -> *

import Data.Kind
import GHC.TypeLits
import Data.Functor.Contravariant
import Unsafe.Coerce

-- >>> :k Type
-- Type :: *
-- A típusnak típus a típusa
-- Girard's Paradox

-- >>> :k Eq
-- Eq :: * -> Constraint

-- >>> :k (->)
-- (->) :: * -> * -> *

-- >>> :k (=>)
-- parse error on input `=>'

b :: a -> Eq a => a -> Bool
b a1 a2 = a1 == a2

-- c = (==)

-- >>> :k 3
-- 3 :: Natural


-- GADT
type Vector :: Natural -> Type -> Type
data Vector (n :: Natural) a where
  VN :: Vector 0 a
  VS :: a -> Vector n a -> Vector (n + 1) a

bool3 :: Vector 3 Bool
bool3 = VS True $ VS False  $ VS True VN

-- >>> head []
-- Prelude.head: empty list
-- >>> :t head
-- head :: HasCallStack => [a] -> a


headV :: Vector (n + 1) a -> a
headV (VS a _) = a

--- >>> headV VN
-- Couldn't match type `1' with `0'
-- Expected: Vector (0 + 1) a_aKQY[sk:1]
--   Actual: Vector 0 a_aKQY[sk:1]
-- In the first argument of `headV', namely `VN'
-- In the expression: headV VN
-- In an equation for `it_aKPz': it_aKPz = headV VN

-- >>> :k! 2 + 2


-- Vissza Functor
-- A funktor megtartja a paraméterének struktúráját
-- identitás: fmap id == id
-- kompozíció: fmap f . fmap g == fmap (f . g)

data List a = Nil | Cons a (List a) deriving Show

--- 0. Leilleszük az összes konstruktort és minden ágra a jobb oldalra ugyanazt visszaírjuk
--- 1. Minden paraméteren egyesével végigmegyünk és mindegyik "helyben marad". Minden paraméterrel az alábbit elvégezzük:
--- 2a. t :: a -> f t
--- 2b. t :: f (g (... (a))), ahol f, g, ... mind Functor  -> fmap (fmap (fmap ... f)) t, annyiszor, ahány f, g, ... van
--- 2c. t :: e -> t

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)


data Compose f g a = MkCompose (f (g a)) deriving Show

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (MkCompose fga) = MkCompose (fmap (fmap f) fga)

data Cont e a = MkCont ((a -> e) -> e)

{-
--      |-----------------------| az "e" csak itt létezik
left :: (forall e. (a -> e) -> e) -> a
left f = f id

right :: a -> (a -> e) -> e
right a f = f a 
-}

instance Functor (Cont e) where
  fmap f (MkCont g) = MkCont undefined

  -- a -> e, ez nem Funktor (a-ban)


-- A funktorok varianciájáról
-- Kovariáns Funktor
-- A kimenetet postcomponálja

-- Kontravariáns Funktor
-- A bemenetet precomponálja

-- >>> :i Contravariant
-- type Contravariant :: (* -> *) -> Constraint
-- class Contravariant f where
--   contramap :: (a' -> a) -> f a -> f a'
--   (>$) :: b -> f b -> f a
--   {-# MINIMAL contramap #-}
--   	-- Defined in ‘Data.Functor.Contravariant’
-- instance Contravariant Comparison
--   -- Defined in ‘Data.Functor.Contravariant’
-- instance Contravariant Equivalence
--   -- Defined in ‘Data.Functor.Contravariant’
-- instance Contravariant (Op a)
--   -- Defined in ‘Data.Functor.Contravariant’
-- instance Contravariant Predicate
--   -- Defined in ‘Data.Functor.Contravariant’

data Predicate' a = MkP (a -> Bool)

instance Contravariant Predicate' where
  contramap f (MkP p) = MkP $ \a' -> p (f a')

-- Precomponálás!!!!

data Cont' e a = MkC' ((a -> e) -> e) deriving Functor

-- Van Laarhoven: https://mail.haskell.org/pipermail/haskell-prime/2007-March/002137.html
-- fmap_<a,u> -- fmapolok valami a-ból u-ba
-- cmap_<u,a> -- cmapolok valami u-ból a-ba

-- fmap_<a,List a>
--
--
-- VL:
-- fmap_<a,a> f = f
-- fmap_<a, T x> f = fmap (fmap_<a, x> f)  (! Ahol T funktor !)
-- fmap_<a, b> f = id                      (! Ahol b független a-tól !)
-- fmap_<a, G x> f = cmap (cmap_<x,a> f)   (! Ahol G kontravariáns funktor !)

--

data Fix f a = MkFix (f (Fix f a)) -- (a opcionális)

h :: Fix Maybe Int
h = MkFix (Just (MkFix Nothing))

type family F x where
  F x = Either () x
--         +   1  x
--  \x -> 1 + x

-- >>> :i ()
-- type Unit :: *
-- data Unit = ()
--   	-- Defined in ‘GHC.Tuple’
-- instance Bounded () -- Defined in ‘GHC.Internal.Enum’
-- instance Enum () -- Defined in ‘GHC.Internal.Enum’
-- instance Ord () -- Defined in ‘GHC.Classes’
-- instance Eq () -- Defined in ‘GHC.Classes’
-- instance Monoid () -- Defined in ‘GHC.Internal.Base’
-- instance Read () -- Defined in ‘GHC.Internal.Read’
-- instance Semigroup () -- Defined in ‘GHC.Internal.Base’
-- instance Show () -- Defined in ‘GHC.Internal.Show’


--           V a haskell it típuscsaládok esetén nem enged parciális applikálás, tehát F nem lehet
jleft :: Fix (Either ()) a -> Natural
jleft (MkFix t) = case t of
  Left () -> 0
  Right t -> 1 + jleft t

jright :: Natural -> Fix (Either ()) a
jright 0 = MkFix (Left ())
jright k = MkFix (Right (jright $ k - 1))


-- >>> jleft (jright 15)
-- 15


--- List a = [] | x : xs
data ListF a f = NilF | ConsF a f
--- \x -> 1 + a * x

lleft :: Fix (ListF a) b -> [a]
lleft (MkFix l) = case l of
  NilF -> []
  ConsF x xs' -> x : lleft xs'

lright :: [a] -> Fix (ListF a) b
lright [] = MkFix NilF
lright (x:xs) = MkFix (ConsF x $ lright xs)

-- recursion-schemes
-- ListF : Base Functor
-- Base Functor-ja egy T típusnak az egy olyan F funktor, ahol Fix F = T 


-- >>> :t unsafeCoerce
-- unsafeCoerce :: a -> b


data A a where
  B :: Int -> A Int

deriving instance Show a => Show (A a)

-- >>> unsafeCoerce (B 68) :: A Char
-- B 'D'

-- >>> unsafeCoerce 1 2
