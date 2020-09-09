
-- infrastruktúra
------------------------------------------------------------
-- ghci parancsok, ghc, holes








-- osztályok
------------------------------------------------------------

class Eq' a where
  eq :: a -> a -> Bool

class Eq' a => Ord' a where
  -- less-or-equal
  lte :: a -> a -> Bool

class Show' a where
  show' :: a -> String

data Color = Red | Green | Blue
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- írd meg a következő instance-okat
instance Eq' Color where
  eq = undefined

instance Ord' Color where
  lte = undefined

instance Show' Color where
  show' = undefined

instance Eq' a => Eq' (Maybe a) where
  eq = undefined

instance Ord' a => Ord' (Maybe a) where
  lte = undefined

instance Show' a => Show' (Maybe a) where
  show' = undefined

instance Eq' a => Eq' [a] where
  eq = undefined

instance Ord' a => Ord' [a] where
  lte = undefined

instance Show' a => Show' [a] where
  show' = undefined

instance Eq' a => Eq' (Tree a) where
  eq = undefined

instance Ord' a => Ord' (Tree a) where
  lte = undefined

instance Show' a => Show' (Tree a) where
  show' = undefined
