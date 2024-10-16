module Drinks where

data Drink = Coffee | Tea | HotChocolate deriving Eq
type Money = Int

removeCredit :: Money -> Maybe Money
removeCredit i | i < 300 = Nothing
removeCredit i = Just $ i - 300

getDrink :: Drink -> [Drink] -> Maybe (Drink, [Drink])
getDrink d ds | d `elem` ds = let (before, after) = break (d /=) ds in Just (d , before ++ after)
getDrink _ _ = Nothing

buyDrink :: Money -> Drink -> [Drink] -> Maybe Drink
buyDrink i d ds = {-removeCredit i >> getDrink d ds >>= \(d , _) -> Just d-} do
  removeCredit i
  (d , _) <- getDrink d ds
  return d