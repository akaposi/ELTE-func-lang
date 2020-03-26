module Practice5 where

-- Maybe intro
-- Monad
-- Maybe as a monad

-- class Monad (m :: * -> *) where
--   (>>=)  :: m a -> (a -> m b) -> m b
--   return :: a -> m a


-- fmap :: (a -> b) -> f a -> f b

-- data Maybe' a = Nothing' | Just' a
--   deriving (Eq, Ord, Show)

foo :: Eq k => [(k,v)] -> k -> Maybe v
foo [] _ = Nothing
foo ((k,v):rest) toFind
  | k == toFind = Just v
  | otherwise   = foo rest toFind

