{-# OPTIONS -Wincomplete-patterns #-}
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
isJust Nothing  = False
isJust (Just x) = True

fromJust :: Maybe a -> a
fromJust Nothing  = error "asd"
fromJust (Just x) = x

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead []    = Nothing

isNothing Nothing = True
isNothing _ = False

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just x : xs) = x : catMaybes xs
catMaybes (Nothing : xs) = catMaybes xs
-- catMaybes (x : xs)
--   | isJust x = fromJust x : catMaybes xs
--   | isNothing x = undefined

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f xs = catMaybes $ map f xs

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe f Nothing  = Nothing
fmapMaybe f (Just x) = Just (f x)

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _  = Nothing
bindMaybe (Just x) f = f x

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

lookupMoneyInUSD :: String -> Maybe Double
lookupMoneyInUSD name =
  case lookup name nameIdDB of
    Nothing -> Nothing
    Just id -> undefined
