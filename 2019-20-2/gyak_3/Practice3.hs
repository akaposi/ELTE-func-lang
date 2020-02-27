module Practice3 where

-- ex1.
-- BinTree amiben a Node-ban vannak az értékek, Leaf-ben nem

data BinTreeNode a = TODO1
  deriving (Eq, Show, Ord)

-- ex2.
-- minde levélben, mind csúcsokban, de eltérő típusú is lehet!

data BinTreeBoth l n = TODO2
  deriving (Eq, Show, Ord)

concatTreeLeaves :: Semigroup l => BinTreeBoth l n -> l 
concatTreeLeaves = undefined 

concatTreeNodes :: Monoid n => BinTreeBoth l n -> n 
concatTreeNodes = undefined 

concatMapTree :: Monoid m => (l -> m) -> (n -> m) -> BinTreeBoth l n -> m 
concatMapTree = undefined

data T a = TodoT

lift :: a -> T a 
lift = undefined

-- LAWS: lift (x <> y) == lift x <> lift y
instance Semigroup (T a) where 
  (<>) lhs rhs = undefined

-- LAWS: lift x <> lift mempty == lift x
--       lift mempty <> lift x == lift x
instance Monoid (T a) where 
  mempty = undefined