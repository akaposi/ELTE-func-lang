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

instance Eq a => Eq (List a) where 
  (==) Nil Nil = True 
  (==) (Cons x xs) (Cons y ys) = x == y && xs == ys
  (==) _ _ = False

instance Semigroup (List a) where 
  (<>) Nil ys = ys 
  (<>) (Cons x xs) ys = Cons x (xs <> ys)

instance Monoid (List a) where 
  mempty = Nil

-- tudjuk: x,y,z típusához létezik Monoid instance
-- mconcatL [x, y, z] == x <> y <> z <> mempty
-- mconcatL (Cons x (Cons y (Cons z Nil))) == x <> y <> z <> mempty
mconcatL :: Monoid m => List m -> m
mconcatL Nil         = mempty
mconcatL (Cons x xs) = x <> mconcatL xs

-- fodlMapL f [x, y, z] == f x <> f y <> f z <> mempty
foldMapL :: Monoid m => (a -> m) -> List a -> m 
foldMapL f Nil         = mempty 
foldMapL f (Cons x xs) = f x <> foldMapL f xs

data BinTree l n = Leaf l | Node n (BinTree l n) (BinTree l n)
  deriving (Eq, Ord, Show)

concatLeaves :: Semigroup l => BinTree l n -> l
concatLeaves (Leaf x) = x
concatLeaves (Node _ lhs rhs) = concatLeaves lhs <> concatLeaves rhs 

concatNodes :: Monoid n => BinTree l n -> n
concatNodes (Leaf _) = mempty 
concatNodes (Node x lhs rhs) = x <> concatNodes lhs <> concatNodes rhs

concatMapBoth :: Monoid m => (l -> m) -> (n -> m) -> BinTree l n -> m
concatMapBoth f _ (Leaf x) = f x
concatMapBoth f g (Node y lhs rhs) = g y <> 
  concatMapBoth f g lhs <> 
  concatMapBoth f g rhs

-- rávezetés
data T0 a = TodoT0

lift0 :: a -> T0 a 
lift0 = undefined

-- LAWS: lift0 (x <> y) == lift0 x <> lift0 y
instance Semigroup a => Semigroup (T0 a) where 
  (<>) lhs rhs = undefined

-- LAWS: lift0 mempty == (mempty :: T0 a)
instance Semigroup a => Monoid (T0 a) where 
  mempty = undefined


-- "nehezebb"
data T a = TodoT

lift :: a -> T a 
lift = undefined

-- LAWS: lift (x <> y) == lift x <> lift y
instance Semigroup (T a) where 
  (<>) lhs rhs = undefined

-- LAWS: lift mempty == (mempty :: T a)
instance Monoid (T a) where 
  mempty = undefined