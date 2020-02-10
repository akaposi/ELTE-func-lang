{-# LANGUAGE InstanceSigs #-}
module Practice06 where

import Control.Monad 

newtype Reader r a = Reader (r -> a)

instance Functor (Reader r) where 
  fmap :: (a -> b) -> Reader r a -> Reader r b 
  fmap f (Reader g) = Reader (f.g)

instance Applicative (Reader r) where 
  pure  = return 
  (<*>) = ap

instance Monad (Reader r) where
  return :: a -> Reader r a 
  return x = Reader (const x)

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b 
  (>>=) (Reader f) k = Reader $ \r -> let (Reader g) = k (f r) in g r