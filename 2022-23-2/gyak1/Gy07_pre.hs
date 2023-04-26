{-# LANGUAGE InstanceSigs, DeriveFunctor, DeriveFoldable #-}
module Gy07 where

import Control.Monad
import Data.Traversable
import Prelude hiding (NonEmpty(..), Maybe(..), Either(..))

newtype State s a = State { runState :: s -> (a, s) } deriving Functor

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) = ap

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State sa) >>= f = State $ \s -> let (a, s') = sa s in runState (f a) s'



-- Definiáljunk egy olyan áll. vált.-t amely megcímzkézi egy lista összes elemét
labelList :: [a] -> State Integer [(Integer, a)]
labelList = undefined

-- Futtassuk le a fenti állapotváltozást 0 kezedeti label-el!
labelList' :: [a] -> [(Integer, a)]
labelList' = undefined


-- Segédműveletek
execState :: State s a -> s -> s
execState st s = let (a, s') = runState st s in s'

evalState :: State s a -> s -> a
evalState st s = let (a, s') = runState st s in a


-- A fenti séma szerint címkézzünk megy egy fát!
-- Csináljuk meg mind preorder, inorder és postorder bejárással
data Tree a = Leaf a | Node (Tree a) a  (Tree a) deriving (Eq, Show, Functor, Foldable)

labelTreePRE, labelTreeIN, labelTreePOST :: Tree a -> Tree (Integer, a)
labelTreePRE = undefined
labelTreeIN = undefined
labelTreePOST = undefined


---- Írjuk meg az alábbi műveleteket

-- Végezzük el az első paraméterként kapott IO műveletet minden listabeli elemre
ioList :: (a -> IO b) -> [a] -> IO [b]
ioList = undefined

-- Csináljuk megy ugyanezt fára
ioTree :: (a -> IO b) -> Tree a -> IO (Tree b)
ioTree = undefined

-- Csináljuk meg ugyanezeket state-el!
stateList :: (a -> State s b) -> [a] -> State s [b]
stateList = undefined

stateTree :: (a -> State s b) -> Tree a -> State s (Tree b)
stateTree = undefined


-- Ezeke a műveletek tetszőleges applikatív struktúrával elvégezhető, pl:
appList :: Applicative f => (a -> f b) -> [a] -> f [b]
appList f [] = pure []
appList f (x:xs) = (:) <$> f x <*> appList f xs

appTree :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
appTree f (Leaf a) = Leaf <$> f a
appTree f (Node l a r) = Node <$> appTree f l <*> f a <*> appTree f r

-- A függvények hasonlóak => új tulajdonság
{-
        appList :: Applicative f => (a -> f b) -> List a -> f (List b)
        appTree :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
-}

-- Ezt a tulajdonságot a Traversable típusosztály fogja reprezentálni
{-
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  {-# MINIMAL traverse | sequenceA #-}
-}


data List a = Nil | Cons a (List a) deriving (Eq, Show, Foldable, Functor)

instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse = undefined

  sequenceA :: Applicative f => List (f a) -> f (List a)
  sequenceA = undefined

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse = appTree
  sequenceA :: Applicative f => Tree (f a) -> f (Tree a)
  sequenceA = undefined

-- Írjunk Traversable instance-okat!


data Id a = Id a deriving (Eq, Show, Foldable, Functor)
data Dual a = Dual a a deriving (Eq, Show, Foldable, Functor)
data BiList a = BiCons a a (BiList a) | BiNill deriving (Eq, Show, Foldable, Functor)
data NonEmpty a = Last a | NECons a (NonEmpty a) deriving (Eq, Show, Foldable, Functor)
data Maybe a = Just a | Nothing deriving (Eq, Show, Foldable, Functor)
data Either e a = Left e | Right a deriving (Eq, Show, Foldable, Functor)
data SplitList e a = LeftCons e (SplitList e a) | RightCons a (SplitList e a) | SNill deriving (Eq, Show, Functor, Foldable)
data Wrap f a = Wrap (f a) deriving (Eq, Show, Foldable, Functor)
data WrapList f a = WrapCons (f a) (WrapList f a) | WrapNill deriving (Eq, Show, Functor, Foldable)
data Compose f g a = Compose (f (g a)) deriving (Eq, Show, Functor, Foldable)
data Tree2 a = Leaf2 | Node2 (Tree2 a) a (Tree2 a) deriving (Eq, Show, Functor, Foldable)

instance Traversable Id where

instance Traversable Dual where

instance Traversable BiList where

instance Traversable NonEmpty where

instance Traversable Maybe where

instance Traversable (Either q) where

instance Traversable (SplitList q) where

instance Traversable f => Traversable (Wrap f) where

instance Traversable f => Traversable (WrapList f) where

instance (Traversable f, Traversable g) => Traversable (Compose f g) where

instance Traversable Tree2 where
