module Practice3 where

-- ex1.
-- BinTree amiben a Node-ban vannak az értékek, Leaf-ben nem

data BinTreeNode a = Leaf' | Node' a (BinTreeNode a) (BinTreeNode a)
  deriving (Eq, Show, Ord)

-- ex2.
-- mind levelekben, mind csúcsokban, de eltérő típusú is lehet!

data BinTreeBoth l n = Leaf l | Node n (BinTreeBoth l n) (BinTreeBoth l n)
  deriving (Eq, Show, Ord)

concatTreeLeaves :: Semigroup l => BinTreeBoth l n -> l 
concatTreeLeaves (Leaf x) = x 
concatTreeLeaves (Node _ lhs rhs) = concatTreeLeaves lhs <> concatTreeLeaves rhs 

concatTreeNodes :: Monoid n => BinTreeBoth l n -> n 
concatTreeNodes (Leaf x) = mempty
concatTreeNodes (Node y lhs rhs) = y <> concatTreeNodes lhs <> concatTreeNodes rhs 

concatMapTree :: Monoid m => (l -> m) -> (n -> m) -> BinTreeBoth l n -> m 
concatMapTree f g (Leaf x) = f x
concatMapTree f g (Node y lhs rhs) = g y <> 
  concatMapTree f g lhs <> concatMapTree f g rhs

data T a = TodoT

lift :: a -> T a 
lift = undefined

-- LAWS: lift (x <> y) == lift x <> lift y
instance Semigroup (T a) where 
  (<>) lhs rhs = undefined

-- LAWS: lift mempty == (mempty :: T a)
instance Monoid (T a) where 
  mempty = undefined