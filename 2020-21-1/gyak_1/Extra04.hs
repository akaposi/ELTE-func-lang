{-# language RankNTypes #-}

import Control.Monad

-- Írd meg az összes hiányzó instance-ot!
--------------------------------------------------------------------------------


data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a)   = Pure (f a)
  fmap f (Free ffa) = Free (fmap (fmap f) ffa)

instance Functor f => Applicative (Free f) where
  pure  = return
  (<*>) = ap

instance Functor f => Monad (Free f) where
  return = Pure
  Free ffa >>= f = Free (fmap (>>= f) ffa)

--------------------------------------------------------------------------------

newtype Cont r a = Cont {runCont :: (a -> r) -> r}

instance Functor (Cont r) where
  fmap f (Cont g) = Cont $ \k -> g (k . f)

instance Applicative (Cont r) where
  pure  = return
  (<*>) = ap

instance Monad (Cont r) where
  return a = Cont $ \k -> k a
  Cont g >>= f = Cont $ \k -> g $ \a -> runCont (f a) k

--------------------------------------------------------------------------------

newtype F f a = F {runF :: forall r. (a -> r) -> (f r -> r) -> r}

instance Functor (F f) where
  fmap f (F g) = F $ \pure free -> g (pure . f) free

instance Applicative (F f) where
  pure  = return
  (<*>) = ap

instance Monad (F f) where
  return a = F $ \pure free -> pure a
  F f >>= g = F $ \pure free -> f (\a -> runF (g a) pure free) free
