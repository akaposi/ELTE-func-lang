{-# LANGUAGE KindSignatures, DeriveFunctor, InstanceSigs #-}
module Practice08 where

-- import Prelude hiding (Applicative(..))

-- Applicative: Functor < Applicative < Monad
-- Traversable: gather effects & results

-- multi-param Functor 
-- "a monad where the computations don't depend on each other"
{-
class Applicative (f :: * -> *) where 
  pure :: a -> f a -- same as return 
  (<*>) :: f (a -> b) -> f a -> f b -- the result context might differ from the input context
  -- fmap :: (a -> b) -> f a -> f b

  2 + 5 -> (+) 2 5
  (+) (Just 5) (Just 2)

  fmap (+2) (Just 5)

  fmap (+) (Just (5 :: Int)) :: Maybe (Int -> Int)
  fmap :: (a -> b) -> f a -> f b 
    ~ (Int -> (Int -> Int)) -> Maybe Int -> Maybe (Int -> Int)
  (+) :: Int -> Int -> Int
  Just 5 :: Maybe Int
  f ~ Maybe
  a ~ Int 
  b ~ Int -> Int

  fmap = <$>
  (+) 2 5 -> (+) <$> Just 5 <*> Just 2
  take 3 [1..5] -> take <$> Just 3 <*> Just [1..5]
  (.) (+1) (*2) 5 -> (.) <$> Just (+1) <*> Just (*2) <*> Just 5

  -- non-determinism with lists
  [(+1), (*2)] <*> [1,2,3]
  [2,3,4,2,4,6]
-}

data Either2 a b = L a | R b 
  deriving (Eq, Ord, Show, Functor)

instance Monoid e => Applicative (Either2 e) where 
  pure :: a -> Either2 e a 
  pure x = R x 

  (<*>) :: Either2 e (a -> b) -> Either2 e a -> Either2 e b 
  (<*>) (R f) (R x) = R (f x)
  (<*>) (R f) (L e) = L e 
  (<*>) (L e) (R x) = L e
  -- until above, same as Either 
  -- (<*>) (Left e1) (Left e2) = (Left e1)  
  (<*>) (L e1) (L e2) = L (e1 <> e2)

  -- can't have Monad instance, 
  -- because it wouldn't be able to agree with the Applicative instance
  {-
    pure  = return
    (<*>) = ap
    (*>)  = (>>) (m1 >> m2   same as   m1 >>= \_ -> m2 )
  -}
  -- (>>=) :: Either2 e a -> (a -> Either2 e b) -> Either2 e b


-- motivation for Traversable

-- sequenceMaybes [Just 1, Just 2, Just 3]  == Just [1,2,3]
-- sequenceMaybes [Just 1, Nothing, Just 3] == Nothing
sequenceMaybes :: [Maybe a] -> Maybe [a]
sequenceMaybes [] = Just [] 
sequenceMaybes (Just x : xs) = case sequenceMaybes xs of 
  Nothing -> Nothing 
  Just ys -> Just (x:ys) 
sequenceMaybes _ = Nothing

-- gatherEffects [Just 1, Just 2, Just 3]  == Just [1,2,3]
-- gatherEffects [Just 1, Nothing, Just 3] == Nothing
gatherEffects :: Monad m => [m a] -> m [a]
gatherEffects [] = return []
gatherEffects (m:ms) = do 
  x  <- m 
  xs <- gatherEffects ms
  return (x:xs)

-- run a monadic computation twice
-- runState (twiceM (modify (+1) >> get)) 0 == ((1,2),2) -- m ~ State Int, a ~ Int
twiceM :: Monad m => m a -> m (a, a)
twiceM m = do 
  x <- m 
  y <- m 
  pure (x,y)