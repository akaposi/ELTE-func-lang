{-# LANGUAGE InstanceSigs #-}
module Practice6 where

import Control.Monad (ap)
import Control.Monad.State

nameIdDB :: [(String, Int)]
nameIdDB = [ ("John",    1)
           , ("Richard", 2)
           , ("Julie",   3)
           ]

data Currency = USD | GBP
  deriving (Eq, Ord, Show)

idMoneyCurDB :: [(Int, (Double, Currency))]
idMoneyCurDB = [ (1, (11, USD))
               , (5, (22, USD))
               , (3, (33, GBP))
               ]

exchangeRateDB :: [(Currency, Double)]
exchangeRateDB = [ (USD, 1)
                 , (GBP, 1.1)
                 ]

-- lookup, foo

lookupMoneyInUSD :: String -> Maybe Double
lookupMoneyInUSD name =
  case lookup name nameIdDB of
    Nothing -> Nothing
    Just id -> case lookup id idMoneyCurDB of
      Nothing -> Nothing
      Just (amount, currency) -> case Just 5 of
        Nothing -> Nothing
        Just x  -> case lookup currency exchangeRateDB of
          Nothing -> Nothing
          Just ratio -> Just $ amount * ratio

lookupMoneyInUSD' :: String -> Maybe Double
lookupMoneyInUSD' name = do
  id                 <- lookup name nameIdDB
  (amount, currency) <- lookup id idMoneyCurDB
  x <- return 5 -- Just 5
  ratio              <- lookup currency exchangeRateDB
  return $ amount * ratio

lookupMoneyInUSD'' :: String -> Maybe Double
lookupMoneyInUSD'' name =
  lookup name nameIdDB >>= \id ->
    lookup id idMoneyCurDB >>= \(amount, currency) ->
      lookup currency exchangeRateDB >>= \ratio ->
        return $ ratio * amount

foo :: Maybe Int
foo = do
  x <- return 1
  _ <- return 2
  return 3

-- ($)   ::   a -> (a ->   b) ->   b
-- (>>=) :: m a -> (a -> m b) -> m b

-- class Functor m => Applicative (m :: * -> *) where

-- class Applicative m => Monad (m :: * -> *) where
--   (>>=)  :: m a -> (a -> m b) -> m b
--   return :: a -> m a

data Maybe' a = Nothing' | Just' a
  deriving (Eq, Show, Ord)

instance Functor Maybe' where
  fmap f Nothing'  = Nothing'
  fmap f (Just' x) = Just' $ f x

instance Applicative Maybe' where
  pure  = return
  (<*>) = ap

instance Monad Maybe' where
  return :: a -> Maybe' a
  return x = Just' x

  (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
  (>>=) Nothing' _  = Nothing'
  (>>=) (Just' x) f = f x

-- (Functor (Applictive (Monad)))
-- (Semigroup (Monoid))

-- newtype State s a = State (s -> (a, s))

-- get :: State s s
-- get = State $ \s -> (s,s)

-- put :: s -> State s ()
-- put s = State $ \_ -> ((), s)

-- runState :: State s a -> s -> (a, s)
-- runState (State f) s0 = f s0

sumElements :: [Int] -> State Int ()
sumElements [] = do
  return ()
sumElements (x:xs) = do
  n <- get
  put (x + n)

sumAndCollect :: [Int] -> State [Int] Int
sumAndCollect [] = do
  ys <- get
  return $ sum ys
sumAndCollect (x:xs) = do
  ys <- get
  put (x:ys)
  sumAndCollect xs

partitionAndDecide :: [Either a b] -> State ([a], [b]) Bool
partitionAndDecide (Left x : ys)

data BinTree l n = Leaf l | Node n (BinTree l n) (BinTree l n)
  deriving (Eq, Ord, Show)

labelLeaves :: BinTree l n -> State Int (BinTree (Int, l) n)
labelLeaves (Node n lhs rhs) = do
  lhs' <- labelLeaves lhs
  rhs' <- labelLeaves rhs
  return $ Node n lhs' rhs'
labelLeaves (Leaf l) = do
  n <- get
  put (n+1)
  return $ Leaf (n,l)

labelNodes :: BinTree l n -> State Int (BinTree l (Int, n))
labelNodes = undefined


