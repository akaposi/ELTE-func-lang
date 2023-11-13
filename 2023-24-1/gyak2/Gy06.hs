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

labelTreeSt :: Tree a -> State Int (Tree (a, Int))
labelTreeSt (Leaf a) = do
  i <- get
  put (i + 1)
  return (Leaf (a, i + 1))
labelTreeSt (Node l a r) = do
  l' <- labelTreeSt l -- L
  i <- get  --- C
  put (i + 1)
  r' <- labelTreeSt r -- R
  return (Node l' (a, i + 1) r')



-- mapM emlékeztető:
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x : xs) = f x >>= \x' -> mapM' f xs >>= \xs' -> return (x' : xs')
{-
do
 x' <- f x
 xs' <- mapM' f xs
 return (x' : xs')
-}

printAll :: Show a => [a] -> IO ()
printAll xs = mapM_ print xs

accumAll :: [Int] -> State Int [Int]
accumAll xs = mapM (\x -> modify (+x) >> return x) xs

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
mapA f [] = pure []
mapA f (x : xs) = (:) <$> f x <*> mapA f xs

-- fmap :: (a -> b) -> f a -> f b
-- (<$>)
-- b = c -> d
-- (<$>) :: (a -> c -> d) -> f a -> f (c -> d)
-- \f fa fc -> f <$> fa <*> fc :: f d
-- fa :: f a
-- fc :: f c
-- f :: a -> c -> d

-- liftA2 f a b = f <$> a <*> b
-- liftA3 f a b c = f <$> a <*> b <*> c
-- liftA4 f a b c d = f <$> a <*> b <*> c <*> d



-- Ezzel úgymond bejártunk egy adatszerkezetet és összegyűjtöttük a melléhatásokat
-- Olyasmi mint egy felturbózott foreach ciklus

-- Írjunk mapA-t fára:
data Tree a = Leaf a | Node (Tree a) a  (Tree a) deriving (Eq, Show, Functor, Foldable)

mapATree :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
mapATree f (Leaf a) = Leaf <$> f a
mapATree f (Node l a r) = Node <$> mapATree f l <*> f a <*> mapATree f r

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
  sequenceA (Leaf a) = Leaf <$> a
  sequenceA (Node l a r) = Node <$> sequenceA l <*> a <*> sequenceA r

traverse' :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverse' f xs = sequenceA (f <$> xs)

sequenceA' :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequenceA' xs = traverse id xs

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
  traverse :: Applicative f => (a -> f b) -> Either q a -> f (Either q b)
  traverse f (Left q) = Left <$> pure q -- Left q
  traverse f (Right a) = Right <$> f a
  sequenceA :: Applicative f => Either q (f a) -> f (Either q a)
  sequenceA (Left q) = Left <$> pure q
  sequenceA (Right a) = Right <$> a

instance Traversable (SplitList q) where

instance Traversable f => Traversable (Wrap f) where
  traverse :: (Traversable f, Applicative f1) => (a -> f1 b) -> Wrap f a -> f1 (Wrap f b)
  traverse f (Wrap fa) = Wrap <$> traverse f fa

instance Traversable f => Traversable (WrapList f) where

instance (Traversable f, Traversable g) => Traversable (Compose f g) where

instance Traversable Tree2 where

{-
labelTreeSt :: Tree a -> State Int (Tree (a, Int))
labelTreeSt (Leaf a) = do
  i <- get
  put (i + 1)
  return (Leaf (a, i + 1))
labelTreeSt (Node l a r) = do
  l' <- labelTreeSt l -- L
  i <- get  --- C
  put (i + 1)
  r' <- labelTreeSt r -- R
  return (Node l' (a, i + 1) r')
-}

-- Traversable függvények gyakorlása:
-- Címkézés:
-- Címkézzünk fát kézzel majd bejárással
labelTree :: Tree a -> Tree (a, Int)
labelTree tr = snd (runState (labelTreeSt tr) 0)

labelTree' :: Tree a -> Tree (a, Int)
labelTree' tr = snd (runState (traverse (\x -> f x) tr) 0)
  where
    f :: a -> State Int (a, Int)
    f x = do
      i <- get
      put (i + 1)
      return (x, i + 1)

-- Írjuk ki egy fa összes elemét
printTree :: Show a => Tree a -> IO ()
printTree tr = mapM_ print tr
-- Olvassunk be egy elemet egy fa összes elemhéz és adjuk hozzá
-- Írjuk ki egy fájlba a fa taralmát

treeToFile :: Show a => Tree a -> IO ()
treeToFile tr = mapM_ (\a -> appendFile "alma.txt" (show a)) tr
