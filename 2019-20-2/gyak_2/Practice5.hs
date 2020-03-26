module Practice5 where

-- review
-- Maybe
-- Monad
-- Maybe as Monad

data Maybe' a = Nothing' | Just' a
  deriving (Eq, Ord, Show)

foo :: Eq k => [(k,v)] -> k -> Maybe v
foo [] toFind = Nothing
foo ((k,v) : rest) toFind
  | k == toFind = Just v
  | otherwise   = foo rest toFind

fmapMaybe :: (a -> b) -> Maybe a -> MAybe b
fmapMaybe = undefined

{- tests
fmapMaybe (Just 5) (2*) == Just 10
fmapMaybe Nothing  (2*) == Nothing
-}

-- class Monad (m :: * -> *) where
--   return :: a -> m a
--   (>>=)  :: m a -> (a -> m b) -> m b

isJust :: Maybe a -> Bool
isJust = undefined

fromJust :: Maybe a -> a
fromJust = undefined

safeHead :: [a] -> Maybe a
safeHead = undefined

catMaybes :: [Maybe a] -> [a]
catMaybes = undefined

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe = undefined

{- tests
returnMaybe 5 == Just 5

bindMaybe (Just 5) (\x -> Just (2*x)) == Just 10
bindMaybe Nothing  (\x -> Just (2*x)) == Nothing
bindMaybe (Just 0) (\x -> if (x == 0) then Nothing else 2/x) == Nothing
-}

returnMaybe :: a -> Maybe a
returnMaybe = undefined

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe = undefined



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
    Just id -> undefined
