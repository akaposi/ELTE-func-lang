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
      Just (money, currency) -> case lookup currency exchangeRateDB of
        Nothing -> Nothing
        Just exRate -> Just (exRate * money)

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _  = Nothing
bindMaybe (Just x) f = f x

lookupMoneyInUSD' :: String -> Maybe Double
lookupMoneyInUSD' name =
  lookup name nameIdDB `bindMaybe` \id ->
    lookup id idMoneyCurDB `bindMaybe` \(money, currency) ->
      lookup currency exchangeRateDB `bindMaybe` \ratio ->
        Just (ratio * money)

-- do-notation
lookupMoneyInUSD'' :: String -> Maybe Double
lookupMoneyInUSD'' name = do
  id                <- lookup name nameIdDB
  (money, currency) <- lookup id idMoneyCurDB
  ratio             <- lookup currency exchangeRateDB
  return $ ratio * money

-- the baove code desugars into this
lookupMoneyInUSD''' :: String -> Maybe Double
lookupMoneyInUSD''' name =
  lookup name nameIdDB           >>= \id ->
  lookup id idMoneyCurDB         >>= \(money, currency) ->
  lookup currency exchangeRateDB >>= \ratio ->
  return (ratio * money)

-- class Monad m where
--   return :: a -> m a

--   -- read as: "bind"
--   (>>=)  :: m a -> (a -> m a) -> m a

newtype State' s a = State' (s -> (a, s))

runState' :: State' s a -> s -> (a, s)
runState' (State' f) s0 = f s0

get' :: State' s s
get' = State' $ \s -> (s, s)

put' :: s -> State' s ()
put' s' = State' $ \s -> ((), s')

index :: [a] -> (Int -> [(Int, a)])
index []     curIx = []
index (x:xs) curIx = (curIx, x) : index xs (curIx + 1)

index' :: Int -> ([a] -> [(Int, a)])
index' curIx []     = []
index' curIx (x:xs) = (curIx, x) : index' (curIx + 1) xs

indexM :: [a] -> State Int [(Int, a)]
indexM [] = return []
indexM (x:xs) = do
  curIx <- get
  put (curIx + 1)
  ys    <- indexM xs
  return $ (curIx, x) : ys

computation1 :: State Int [(Int, Char)]
computation1 = indexM "Hello world"

example1 :: ([(Int, Char)], Int)
example1 = runState computation1 0

data BinTree l n = Leaf l | Node n (BinTree l n) (BinTree l n)
  deriving (Eq, Ord, Show)

labelLeavesM :: BinTree l n -> State Int (BinTree (Int,l) n)
labelLeavesM (Leaf l) = do
  n <- get
  put (n+1)
  return $ Leaf (n,l)
labelLeavesM (Node n lhs rhs) = do
  lhs' <- labelLeavesM lhs
  rhs' <- labelLeavesM rhs
  return $ Node n lhs' rhs'

computation2 :: State Int (BinTree (Int,String) Char)
computation2 = labelLeavesM $ Node 'a' (Node 'b' (Leaf "asd") (Leaf "qwe")) (Leaf "foobar")

example2 :: (BinTree (Int,String) Char, Int)
example2 = runState computation2 0
