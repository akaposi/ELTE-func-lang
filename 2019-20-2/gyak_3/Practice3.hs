module Practice3 where

-- ex1.
-- BinTree amiben a Node-ban vannak az értékek, Leaf-ben nem

data BinTreeNode a = TODO

-- ex2.
-- minde levélben, mind csúcsokban, de eltérő típusú is lehet!

data BinTreeBoth l n = TODO 

concatTreeLeaves :: Semigroup l => BinTreeBoth l n -> l 
concatTreeLeaves = undefined 

concatTreeNodes :: Monoid n => BinTreeBoth l n -> n 
concatTreeNodes = undefined 

concatMapTree :: Monoid m => (l -> m) -> (n -> m) -> BinTreeBoth l n -> m 
concatMapTree = undefined