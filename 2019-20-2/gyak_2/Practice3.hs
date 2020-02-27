module Practice3 where

import Data.Monoid

-- LAWS: (x <> y) <> z == x <> (y <> z)
-- class Semigroup m where 
--   (<>) :: m -> m -> m 

-- class Semigroup m => Monoid m where 
--   mempty :: m

data List a = Nil | Cons a (List a)
  deriving (Show)

instance Eq (List a) where 
  (==) _ _ = undefined

instance Semigroup (List a) where 
  (<>) _ _ = undefined

instance Monoid (List a) where 
  mempty = _

mconcat :: Monoid m => List m -> m
mconcat = undefined 


data T a = TodoT

lift :: a -> T a 
lift = undefined

-- LAWS: lift (x <> y) == lift x <> lift y
instance Semigroup (T a) where 
  (<>) lhs rhs = undefined

-- LAWS: lift mempty == (mempty :: T a)
instance Monoid (T a) where 
  mempty = undefined