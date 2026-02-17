{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ea2 where

import Data.Kind

-- |Bool| = 2

data Three
  = One
  | Two
  | Three

-- |Three| = 3

-- data Weekday -- |Weekday| = 7
--   = Monday
--   | ...
--   | Sunday

{-
data Either a b
  = Left a
  | Right b
-}

-- |Either Bool Three| = 5
-- |Either a b| = |a| + |b|

-- |(Bool, Tree)| = 6
-- |(a, b)| = |a| * |b|

-- |Bool -> Three| = 9
-- |a -> b| = |b|^|a|

{-
f :: Bool -> Three
f False = ?
f True = ?
-}

-- a + b = b + a
-- Either a b ≅ Either b a -- isomorphism (bijection)

{-
a ≅ b
f :: a -> b
g :: b -> a
g (f x) = x
f (g x) = x
-}

swapEither :: Either a b -> Either b a
swapEither (Left a) = Right a
swapEither (Right b) = Left b

-- (a -> b, a -> c) ≅ a -> (b, c)    b^a * c^a = (b*c)^a

to :: (a -> b, a -> c) -> a -> (b, c)
to (f, g) a = (f a, g a)

from :: (a -> (b, c)) -> (a -> b, a -> c)
from f = (\a -> fst (f a), \a -> snd (f a))

-- (Bool -> Either a b) ≅ Either (Bool -> a) (Either (Bool, a, b) (Bool -> b))



-- type of types = kind

-- type constructors:
-- Bool :: Type
-- Int :: Type
-- Maybe :: Type -> Type
-- [] :: Type -> Type
-- Either :: Type -> Type -> Type

type Tree :: Type -> Type
data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show)

-- id' :: a -> a
id' :: forall (a :: Type). a -> a
id' x = x

-- wrong :: Maybe -> Maybe   -- kind error
-- wrong = _

-- id'' :: f a -> f a
id'' :: forall (f :: Type -> Type) (a :: Type). f a -> f a
id'' x = x
-- higher-kinded polymorphism
-- not supported by most languages
-- except: Haskell, Ocaml,... C++

-- id'' True -> type error   Bool != f a
-- id'' Nothing = Nothing :: Maybe a
-- id'' [1, 2, 3] = [1, 2, 3] :: [Int] = [] Int



{-
class Functor (f :: Type -> Type) where
  fmap :: (a -> b) -> f a -> f b
-}

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

{-
instance Functor (Either e) where
  fmap :: (a -> b) -> Either e a -> Either e b
  fmap f (Left e) = Left e
  fmap f (Right a) = Right (f a)
-}


data FunFromInt a = FunFromInt (Int -> a)

instance Functor FunFromInt where
  fmap :: (a -> b) -> FunFromInt a -> FunFromInt b
  -- fmap f (FunFromInt g) = FunFromInt (f . g)
  fmap f (FunFromInt g) = FunFromInt $ \n -> f (g n)

data FunToInt a = FunToInt (a -> Int)

-- instance Functor FunToInt where
--   fmap :: (a -> b) -> FunToInt a -> FunToInt b
--   fmap f (FunToInt g) = FunToInt $ \b -> g _ -- not possible

-- type argument can be covariant (positive) or contravariant (negative)
data Crazy a = Crazy   (((Int -> a) -> Int) -> (Int -> a))  -- is a functor
--                        -      +      -        -     +
data Crazy2 a = Crazy2 (((a -> Int) -> Int) -> (Int -> a))  -- not a functor

-- Functor = covariant functor

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

-- instance Contravariant FunToInt

data Endo a = Endo (a -> a)
-- invariant functor: (a -> b) -> (b -> a) -> f a -> f b

data Unit a = Unit -- bivariant: f a -> f b

{-
     bivariant
     v      v
covariant  contravariant
     v      v
     invariant
-}


-- fixpoint function
fix :: (a -> a) -> a
fix f = f (fix f)
-- fix f = f (fix f) = f (f (fix f)) = ... = f (f (f (...)))

ones :: [Int]
ones = fix $ \xs -> 1 : xs
-- ones = 1 : ones = 1 : 1 : ones = 1 : 1 : 1 : ...

type Fix :: (Type -> Type) -> Type
data Fix f = MkFix (f (Fix f))

-- data List a = Nil | Cons a (List a)

data ListF a b -- ≅ Maybe (a, b)
  = NilF
  | ConsF a b
  deriving (Functor)

-- Functor (ListF a)

class Bifunctor (f :: Type -> Type -> Type) where
  bimap :: (a -> b) -> (c -> d) -> f a c -> f b d

instance Bifunctor ListF where
  bimap f g NilF = NilF
  bimap f g (ConsF a b) = ConsF (f a) (g b)

type List a = Fix (ListF a) -- MkFix :: ListF a (List a) -> List a

example :: List Int
example = MkFix (ConsF 1 (MkFix (ConsF 2 (MkFix NilF))))

fromFix :: List a -> [a]
fromFix (MkFix NilF) = []
fromFix (MkFix (ConsF x xs)) = x : fromFix xs

type Nat = Fix Maybe -- MkFix :: Maybe Nat -> Nat
-- Zero | Succ Nat

fold :: Functor f => (f a -> a) -> Fix f -> a
fold f (MkFix x) = f (fmap (fold f) x)

foldr_ :: (a -> b -> b) -> b -> List a -> b
foldr_ f b = fold $ \xs -> case xs of
  NilF -> b
  ConsF x r -> f x r


map' :: Bifunctor f => (a -> b) -> Fix (f a) -> Fix (f b)
map' f (MkFix x) = MkFix (bimap f (map' f) x)
