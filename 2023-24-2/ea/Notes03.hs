{-# LANGUAGE KindSignatures, GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Notes3 where

import Data.Void
import Data.Functor.Contravariant

-- GHCi parancs :k
-- Két fő dolog: Type és a Constraint kind szinten

class Mappable (f :: * -> *) where
  map' :: (a -> b) -> f a -> f b


---                V
instance Mappable [] where
  map' :: (a -> b) -> [a] -> [b]
  map' f [] = []
  map' f (x : xs) = f x : map' f xs


data Endo a = MkEndo (a -> a)

instance Functor Endo where
  fmap f (MkEndo aa) = MkEndo undefined

data DoesNotExist a = MKDNE (a -> Void)

instance Functor DoesNotExist where
  fmap f (MKDNE av) = MKDNE undefined


-- Mikor lehet valami Funktor és Contravariant is???
-- 'a' meg se jelenik a típusban

--           v fel se használja
data Const b a = MkConst b deriving Show


instance Functor (Const c) where
  fmap f (MkConst b) = MkConst b

instance Contravariant (Const c) where
  contramap f (MkConst c) = MkConst c

-- Hogyan írunk Functor instance-ot?

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node t a t') = Node (fmap f t) (f a) (fmap f t')
  --            ^ Tree a      ^ Tree b

data BigTree f a = Leaf' (f a) | Node' (BigTree f a) (f a) (BigTree f a) deriving Show

instance Functor f => Functor (BigTree f) where
  fmap f (Leaf' a) = Leaf' (fmap f a)
  fmap f (Node' t a t') = Node' (fmap f t) (fmap f a) (fmap f t')


-- GADT

data Tuple a b where
  MkTuple :: a -> b -> Tuple a b

data Either' a b where
  MkLeft :: a -> Either' a b
  MkRight :: b -> Either' a b

data Alma a where
  Alma2 :: Eq a => a -> Alma a

instance Eq (Alma a) where
  (Alma2 a) == (Alma2 b) = a == b


data Dict c where
  MkDict :: c => Dict c


funkyEq :: a -> a -> Dict (Eq a) -> Bool
funkyEq a b MkDict = a == b


type Implies c d = Dict c -> Dict d
type And c d = (Dict c, Dict d)
type Or c d = Either (Dict c) (Dict d)
type Not c = Dict c -> Void
