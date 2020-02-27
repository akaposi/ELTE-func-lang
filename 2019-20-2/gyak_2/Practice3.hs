module Practice3 where

import Data.Monoid

-- LAWS: (x <> y) <> z == x <> (y <> z)
-- class Semigroup m where 
--   (<>) :: m -> m -> m 

-- class Semigroup m => Monoid m where 
--   mempty :: m

-- üres lista: Nil 
-- [1,2,3]: Cons 1 (Cons 2 (Cons 3 Nil))
data List a = Nil | Cons a (List a)
  deriving (Show)

instance Eq (List a) where 
  (==) _ _ = undefined

instance Semigroup (List a) where 
  (<>) _ _ = undefined

instance Monoid (List a) where 
  mempty = _

-- tudjuk: x,y,z típusához létezik Monoid instance
-- mconcatL [x, y, z] == x <> y <> z <> mempty
mconcatL :: Monoid m => List m -> m
mconcatL = undefined 

-- fodlMapL f [x, y, z] == f x <> f y <> f z <> mempty
foldMapL :: Monoid m => (a -> m) -> List a -> m 
foldMapL = undefined 

data BinTree l n = Leaf l | Node n (BinTree l n) (BinTree l n)
  deriving (Eq, Ord, Show)

concatLeaves :: Semigroup l => BinTree l n -> l
concatLeaves = undefined 

concatNodes :: Monoid n => BinTree n l -> n
concatNodes = undefined 

concatMapBoth :: Monoid m => (l -> m) -> (n -> m) -> BinTree n l -> m
concatMapBoth = undefined

data T a = TodoT

lift :: a -> T a 
lift = undefined

-- LAWS: lift (x <> y) == lift x <> lift y
instance Semigroup (T a) where 
  (<>) lhs rhs = undefined

-- LAWS: lift mempty == (mempty :: T a)
instance Monoid (T a) where 
  mempty = undefined