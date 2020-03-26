module Practice5 where

-- Maybe intro
-- Monad
-- Maybe as a monad

-- class Monad (m :: * -> *) where
--   (>>=)  :: m a -> (a -> m b) -> m b
--   return :: a -> m a


-- fmap :: (a -> b) -> f a -> f b

data Maybe' a = Nothing' | Just' a
  deriving (Eq, Ord, Show)

foo :: Eq k => [(k,v)] -> k -> Maybe v
foo [] _ = Nothing
foo ((k,v):rest) toFind
  | k == toFind = Just v
  | otherwise   = foo rest toFind


isJust :: Maybe a -> Bool
isJust = undefined

fromJust :: Maybe a -> a
fromJust = undefined

catMaybes :: [Maybe a] -> [a]
catMaybes = undefined

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe = undefined

safeHead :: [a] -> Maybe a
safeHead = undefined

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe = undefined

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe = undefined

{- tests
bindMaybe Nothing id                  == Nothing
bindMaybe (Just 2) (\x -> Just (2*x)) == Just 4
bindMaybe (Just 2) (\x -> Nothing)    == Nothing

returnMaybe 5 == Just 5
-}








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

lookupMoneyInUSD :: String -> Double
lookupMoneyInUSD = undefined
