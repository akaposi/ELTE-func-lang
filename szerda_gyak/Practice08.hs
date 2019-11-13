{-# LANGUAGE KindSignatures, InstanceSigs, DeriveFunctor #-}
module Practice08 where

-- import Prelude hiding (Applicative(..))

-- class Functor f => Applicative (f :: * -> *) where
--   pure :: a -> f a 
--   (<*>) :: f (a -> b) -> f a -> f b

data Either2 a b = L a | R b 
  deriving (Eq, Ord, Show, Functor)

-- example for indepenedent computations
instance Monoid a => Applicative (Either2 a) where 
  pure :: b -> Either2 a b 
  pure x = R x

  (<*>) :: Either2 a (b -> c) -> Either2 a b -> Either2 a c
  (<*>) (R f)  (R x)  = R (f x)  
  (<*>) (L e)  (R x)  = L e  
  (<*>) (R f)  (L e)  = L e  
  (<*>) (L e1) (L e2) = L (e1 <> e2)

f1 :: Monad m => (a -> b -> c) -> m a -> m b -> m c  
f1 f m1 m2 = do 
  x <- m1 
  y <- m2 
  pure (f x y)

f1' :: Applicative m => (a -> b -> c) -> m a -> m b -> m c  
f1' f m1 m2 = f <$> m1 <*> m2

-- use only fmap, foldMap, fold
-- sequenceMaybes [Just 5, Just 1, Just 2] == Just [5,1,2]
-- sequenceMaybes [Just 5, Nothing, Just 2] == Nothing
sequenceMaybes :: [Maybe a] -> Maybe [a] 
sequenceMaybes [] = Just [] 
sequenceMaybes (Nothing:_) = Nothing
sequenceMaybes ((Just x):xs) = case sequenceMaybes xs of 
  Nothing -> Nothing 
  Just ys -> Just (x:ys)

twiceM :: Monad m => m a -> m (a, a)
twiceM m = do 
  undefined

gatherEffects :: Monad m => [m a] -> m [a]
gatherEffects [] = pure [] 
gatherEffects (m:ms) = do 
  x  <- m 
  xs <- gatherEffects ms
  pure (x:xs)

h :: Double -> Maybe Double 
h 0 = Nothing
h n = Just (10 / n)

instance Traversable [] where 
  traverse :: (a -> t a) -> [a] -> t [a]
  traverse f [] = pure [] 
  traverse f (x:xs) = (:) <$> f x <*> traverse f xs
  -- fmap f (x:xs) = (:) (f x) (fmap f xs)