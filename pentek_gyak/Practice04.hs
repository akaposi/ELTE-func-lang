{-# LANGUAGE KindSignatures, InstanceSigs #-}
module Practice04 where

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

getMoneyInUSD :: String -> Maybe Double
getMoneyInUSD name = case lookup name nameIdDB of
  Nothing -> Nothing
  Just id -> case lookup id idMoneyCurDB of
    Nothing                 -> Nothing
    Just (amount, currency) -> case lookup currency exchangeRateDB of
      Nothing     -> Nothing
      Just exRate -> Just $ amount * exRate

getMoneyInUSD' :: String -> Maybe Double
getMoneyInUSD' name = do
  id                 <- lookup name nameIdDB
  (amount, currency) <- lookup id idMoneyCurDB
  exRate             <- lookup currency exchangeRateDB
  pure $ amount * exRate

-- desugared version
getMoneyInUSD'' :: String -> Maybe Double
getMoneyInUSD'' name =
  lookup name nameIdDB           >>= \id ->
  lookup id idMoneyCurDB         >>= \(amount, currency) ->
  lookup currency exchangeRateDB >>= \exRate ->
  pure $ amount * exRate

getMoneyOfJulie :: Maybe Double
getMoneyOfJulie = do
  id                 <- lookup "Julie" nameIdDB
  (amount, currency) <- lookup id idMoneyCurDB
  exRate             <- lookup currency exchangeRateDB
  pure $ amount * exRate

{-
class Applicative m => Monad (m :: * -> *) where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

instance Monad' Maybe where
  return :: a -> Maybe a
  return x = Just x

  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>=) m cont = case m of
    Nothing -> Nothing
    Just x  -> cont x

-}

{-
  x <- m
  <cont>
  DESUGARED INTO:
  m >>= (\x -> <cont>)
-}


