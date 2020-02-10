{-# LANGUAGE InstanceSigs #-}
module Practice06_2 where 

import Control.Monad

newtype Reader e a = Reader { runReader :: e -> a }

instance Functor (Reader e) where 
  fmap :: (a -> b) -> Reader e a -> Reader e b 
  fmap f (Reader g) = Reader $ \e -> f (g e)

instance Applicative (Reader e) where 
  pure  = return
  (<*>) = ap

instance Monad (Reader e) where 
  return :: a -> Reader e a 
  return x = Reader $ \_ -> x 

  (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
  (>>=) (Reader f) k = Reader $ \e -> runReader (k (f e)) e 

ask :: Reader e e 
ask = Reader $ \e -> e

-------

newtype Writer w a = Writer { unWriter :: w -> (a, w) }

instance Functor (Writer w) where 
  fmap :: (a -> b) -> Writer w a -> Writer w b 
  fmap f (Writer g) = Writer $ \w -> let (a, w') = g w in (f a, w')

instance Applicative (Writer w)  where 
  pure  = return
  (<*>) = ap

instance Monad (Writer w) where 
  return :: a -> Writer w a 
  return x = Writer $ \w -> (x, w)

  (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
  (>>=) (Writer f) k = Writer $ \w -> let (x, w') = f w  in 
                                      let Writer g = k x in 
                                          g w'
