module Ea3 where

import Data.Monoid hiding (Endo)

-- Semigroup, Monoid :: Type -> Constraint

-- Functor :: (Type -> Type) -> Constraint

-- instance Functor []
-- instance Functor Maybe

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- f :: Type -> Type
-- [] :: Type -> Type
-- Maybe :: Type -> Type

-- Haskell, OCaml, ... C++

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)
data Tree' a = Leaf' | Node' (Tree' a) a (Tree' a)
  deriving (Show)

{-
data Either a b
  = Left a
  | Right b

Either :: Type -> Type -> Type
-}

{-
instance Functor (Either e) where
  fmap :: (a -> b) -> Either e a -> Either e b
  fmap (Left e) = Left e
  fmap (Right a) = Right (f a)
-}

-- Endo :: Type -> Type
newtype Endo a = Endo {runEndo :: a -> a}

-- instance Functor Endo where
--   -- (a -> b) -> Endo a -> Endo b
--   fmap f (Endo g) = Endo (\b -> f (g _))

newtype FunFromInt a = FunFromInt (Int -> a)

instance Functor FunFromInt where
  -- (a -> b) -> FunFromInt a -> FunFromInt b
  fmap f (FunFromInt g) = FunFromInt $ f . g

newtype FunToInt a = FunToInt (a -> Int)

-- instance Functor FunToInt where
--   -- (a -> b) -> FunToInt a -> FunToInt b
--   fmap f (FunToInt g) = FunToInt $ \b -> g _

newtype Weird a = Weird ((a -> Int) -> Int)

instance Functor Weird where
  fmap :: (a -> b) -> Weird a -> Weird b
  fmap f (Weird g) = Weird $ \h -> g (\a -> h (f a))

newtype Weird' a = Weird' ((a -> a) -> a) -- not a functor
--                                     ^ positive position
--                               ^ negative position
--                          ^ positive position
newtype Weird'' a = Weird'' ((((a -> Int) -> a) -> Int) -> a) -- is a functor
  deriving (Functor)

-- We have functor instance if `a` appears only in positive position

-- Any type has at most one good functor instance

-- fmap :: Functor f => (Int -> Int) -> f Int -> f Int
-- (+ 2) :: Int -> Int
-- (* 3) :: (->) Int Int        f Int     -- f = (->) Int
-- (->) :: Type -> Type -> Type

{-
instance Functor ((->) i) where
  fmap :: (a -> b) -> (->) i a -> (->) i b
  fmap :: (a -> b) -> (i -> a) -> (i -> b)
  fmap = (.)
-}

-- Functor (Either a)

-- f :: Type -> Type -> Type
class Bifunctor f where
  bimap :: (a -> b) -> (c -> d) -> f a c -> f b d

instance Bifunctor Either where
  bimap f g (Left a) = Left (f a)
  bimap f g (Right b) = Right (g b)

-- instance Functor ((,) a)

-- (,) :: Type -> Type -> Type
instance Bifunctor (,) where
  bimap f g (a, b) = (f a, g b)

-- instance Bifunctor (->) where
--   bimap :: (a -> c) -> (b -> d) -> (a -> b) -> (c -> d)
--   bimap f g h c = g (h _)

-- class Trifunctor, class Quadfunctor

-- Functor = covariant Functor

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

-- no Contravariant Maybe, []

-- newtype FunToInt a = FunToInt (a -> Int)
instance Contravariant FunToInt where
  contramap :: (b -> a) -> FunToInt a -> FunToInt b
  contramap f (FunToInt g) = FunToInt $ \b -> g (f b)

-- newtype Endo a = Endo (a -> a)
-- no Controvariant Endo

class Invariant f where
  invmap :: (a -> b) -> (b -> a) -> f a -> f b

instance Invariant Endo where
  invmap f g (Endo h) = Endo $ f . h . g

-- Invariant [], Invariant Maybe, Invariant FunToInt
-- (GADTs have no Invariant instance)

data Empty a = Empty

instance Functor Empty where
  fmap _ Empty = Empty

instance Contravariant Empty where
  contramap _ Empty = Empty

-- bivariant functor

{-
     invariant
      ^      ^
covariant   contravariant
      ^      ^
      bivariant
-}

-- f :: Type -> Type -> Type
class Profunctor f where
  dimap :: (b -> a) -> (c -> d) -> f a c -> f b d

-- instance Profunctor (->)


{-
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)
-}


{-
class Foldable f where
  foldMap :: Monoid m => (a -> m) -> f a -> m
  foldMap f xs = foldr (\x r -> f x <> r) mempty xs

  fold :: Monoid m => f m -> m
  fold = foldMap id

  foldl :: (b -> a -> b) -> b -> f a -> b
  foldr :: (a -> b -> b) -> b -> f a -> b
-}

-- mconcat :: Monoid m => [m] -> m

-- Foldable []
foldMapList :: Monoid m => (a -> m) -> [a] -> m
foldMapList f [] = mempty
foldMapList f (x:xs) = f x <> foldMapList f xs

foldMapList' :: Monoid m => (a -> m) -> [a] -> m
foldMapList' f xs = foldr (\x r -> f x <> r) mempty xs

-- Foldable Maybe
foldMapMaybe :: Monoid m => (a -> m) -> Maybe a -> m
foldMapMaybe f Nothing = mempty
foldMapMaybe f (Just x) = f x

-- data Tree a = Leaf a | Node (Tree a) (Tree a)
instance Foldable Tree where
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l <> foldMap f r

sum' :: Foldable f => f Int -> Int
sum' xs = getSum $ foldMap Sum xs

product' :: Foldable f => f Int -> Int
product' xs = getProduct $ foldMap Product xs

-- and' :: Foldable f => f Bool -> Bool


-- [1, 2, 3]
-- f 1 (f 2 (f 3 x))

-- (f 1 . f 2 . f 3) x

-- newtype Endo a = Endo (a -> a)
instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id

foldr_ :: forall f a b. Foldable f => (a -> b -> b) -> b -> f a -> b
foldr_ f b xs = runEndo (foldMap g xs) b
  where
  g :: a -> Endo b
  g x = Endo (f x)

-- [1, 2, 3]
-- f (f (f b 1) 2) 3
-- flip f 3 (flip f 2 (flip f 1 b))
-- (flip f 3 . flip f 2 . flip f 1) b

newtype Endo' a = Endo' (a -> a)

instance Semigroup (Endo' a) where
  Endo' f <> Endo' g = Endo' (g . f)

-- Data.Monoid
-- newtype Dual a = Dual {getDual a}

{-
instance Semigroup a => Semigroup (Dual a) where
  Dual x <> Dual y = Dual (y <> x)
-}

foldl_ :: forall f a b. Foldable f => (b -> a -> b) -> b -> f a -> b
foldl_ f b xs = runEndo (getDual (foldMap g xs)) b
  where
  g :: a -> Dual (Endo b)
  g x = Dual (Endo (flip f x))
