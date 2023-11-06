{-# LANGUAGE DeriveFunctor, DeriveFoldable, InstanceSigs, NoMonomorphismRestriction, LambdaCase #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module Gy06 where

import Control.Applicative
import Control.Monad
import Data.Traversable
import Prelude hiding (NonEmpty(..), Either(..))

newtype State s a = State { runState :: s -> (s, a) } deriving Functor

instance Applicative (State s) where
  (<*>) = ap
  pure x = State $ \s -> (s,x)

instance Monad (State s) where
  (State fa) >>= f = State $ \s -> let (s', a) = fa s in runState (f a) s'

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ const (s, ())

modify :: (s -> s) -> State s ()
modify f = get >>= put . f


-- mapM emlékeztető:
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x : xs) = f x >>= \x' -> mapM' f xs >>= \xs' -> return (x' : xs')

-- A Monad feltétel nem szükséges ehhez
-- A >>= paramétereit nem használjuk fel új "számítás" felépítésében (csak retunrben)
-- Az elvégzett számítások (f x és mapM' f xs) függetlenek egymástól
-- A monádnak egy gyengébb verziója ezt kielégíti
-- Applicative:
{-
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
        -- Defined in ‘GHC.Base’
-}
-- Applicative hasznossága:
-- Pl párhuzamosítás
-- Mivel a számítások nem függnek egymástól, csak az eredményük szükséges ezért tetszőleges sorrendben elvégezhetőek, akár egyszerre
-- Pl környezetfüggetlen nyelvtanok

-- Írjuk meg Applicative megkötéssel a mapM-et!
mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA = undefined

-- Ezzel úgymond bejártunk egy adatszerkezetet és összegyűjtöttük a melléhatásokat
-- Olyasmi mint egy felturbózott foreach ciklus

-- Írjunk mapA-t fára:
data Tree a = Leaf a | Node (Tree a) a  (Tree a) deriving (Eq, Show, Functor, Foldable)

mapATree :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
mapATree = undefined

newtype Single a = Single a deriving (Eq, Show, Foldable, Functor)
data Tuple a = Tuple a a deriving (Eq, Show, Foldable, Functor)
data Quintuple a = Quintuple a a a a a deriving (Eq, Show, Foldable, Functor)
data NonEmpty a = Last a | NECons a (NonEmpty a) deriving (Eq, Show, Foldable, Functor)

-- Írjuk meg még pér másik adatszerkezetre is

mapASingle :: Applicative f => (a -> f b) -> Single a -> f (Single b)
mapASingle = undefined

mapAMaybe :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
mapAMaybe = undefined

mapATuple :: Applicative f => (a -> f b) -> Tuple a -> f (Tuple b)
mapATuple = undefined

mapAQuintuple :: Applicative f => (a -> f b) -> Quintuple a -> f (Quintuple b)
mapAQuintuple = undefined

mapANE :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
mapANE = undefined

-- Általánosítható tuljadonság: Traversable típusosztály
{-
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)
  {-# MINIMAL traverse | sequenceA #-}
        -- Defined in ‘Data.Traversable’
-}

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse = mapATree
  sequenceA :: Applicative f => Tree (f a) -> f (Tree a)
  sequenceA = undefined

-- Gyakorlás
data BiList a = BiCons a a (BiList a) | BiNill deriving (Eq, Show, Foldable, Functor)
data Either e a = Left e | Right a deriving (Eq, Show, Foldable, Functor)
data SplitList e a = LeftCons e (SplitList e a) | RightCons a (SplitList e a) | SNill deriving (Eq, Show, Functor, Foldable)
data Wrap f a = Wrap (f a) deriving (Eq, Show, Foldable, Functor)
data WrapList f a = WrapCons (f a) (WrapList f a) | WrapNill deriving (Eq, Show, Functor, Foldable)
data Compose f g a = Compose (f (g a)) deriving (Eq, Show, Functor, Foldable)
data Tree2 a = Leaf2 | Node2 (Tree2 a) a (Tree2 a) deriving (Eq, Show, Functor, Foldable)

instance Traversable BiList where

instance Traversable NonEmpty where

instance Traversable (Either q) where

instance Traversable (SplitList q) where

instance Traversable f => Traversable (Wrap f) where

instance Traversable f => Traversable (WrapList f) where

instance (Traversable f, Traversable g) => Traversable (Compose f g) where

instance Traversable Tree2 where

-- Traversable függvények gyakorlása:
-- Címkézés:
-- Címkézzünk fát kézzel majd bejárással
labelTree :: Tree a -> Tree (a, Int)
labelTree = undefined

labelTree' :: Tree a -> Tree (a, Int)
labelTree' = undefined

-- Írjuk ki egy fa összes elemét
-- Olvassunk be egy elemet egy fa összes elemhéz és adjuk hozzá
-- Írjuk ki egy fájlba a fa taralmát
