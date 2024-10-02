module Crazy where

data CrazyType a = C1 a a | C2 a Int | C3  
  (CrazyType a) deriving (Eq, Show)

instance Functor CrazyType where
  fmap :: (a -> b) -> CrazyType a -> CrazyType b
  fmap f (C1 a b) = C1 (f a) (f b)
  fmap f (C2 a i) = C2 (f a) i
  fmap f (C3 rec) = C3 $ fmap f rec

