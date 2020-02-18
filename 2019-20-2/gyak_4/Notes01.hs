{-# LANGUAGE InstanceSigs #-}
-------------------------------------------
-- Type classes and algebraic data types --
-------------------------------------------

module Notes01 where
import Prelude hiding (showList)

-- GHCI commands:
-- > :r                    reload the file
-- > (expression)          evaluate the expression
-- > :t (expression)       give the type of the expression
-- > :i (name)             give information about the name (definition, ...)

eqBool :: Bool -> Bool -> Bool
eqBool True  True  = True
eqBool False False = True
eqBool _     _     = False

eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList eqA []     []     = True
eqList eqA (x:xs) (y:ys) = (eqA x y) && (eqList eqA xs ys)
eqList eqA _      _      = False

eqListBool :: [Bool] -> [Bool] -> Bool
-- eqListBool []     [] = True
-- eqListBool (x:xs) (y:ys) = (eqBool x y) && (eqListBool xs ys)
-- eqListBool _      _ = False
eqListBool = eqList eqBool

eqListListBool :: [[Bool]] -> [[Bool]] -> Bool
eqListListBool = eqList eqListBool

-- Type classes
class Eq' a where
  eq :: a -> a -> Bool

instance Eq' Bool where
  eq = eqBool

instance Eq' a => Eq' [a] where
  eq []     []     = True
  eq (x:xs) (y:ys) = (eq x y) && (eq xs ys)
  eq _      _      = False

eqPair :: (Eq' a, Eq' b) => (a,b) -> (a,b) -> Bool
eqPair (xa,xb) (ya,yb) = (eq xa ya) && (eq xb yb)

instance (Eq' a, Eq' b) => Eq' (a, b) where
  eq (xa,xb) (ya,yb) = (eq xa ya) && (eq xb yb)

-- Standard type classes:
--   class Eq a where
--     (==) :: a -> a -> Bool
--   data Ordering = LT | EQ | GT
--   class Ord a where
--     compare :: a -> a -> Ordering
--   class Show a where
--     show :: Show a => a -> String

-- Algebraic data types (ADTs)
--   The types of booleans and lists are defined as algebraic data types.
--
-- data Bool = True
--           | False
--
-- data List a = Empty
--             | Cons a (List a)

-- Binary trees
data BinTree a = Leaf a
               | Node (BinTree a) (BinTree a)

-- Functions can be defined by pattern matching
-- f :: Bool -> b
-- f True  = ???
-- f False = ???
--
-- g :: BinTree a -> b
-- g (Leaf x)   = ???
-- g (Node l r) = ???


tree :: BinTree Int
tree = Node (Node (Leaf 2) (Leaf 4)) (Leaf 5)

--                o
--               / \
--   tree =     o   5
--             / \
--            2   4

instance Eq' a => Eq' (BinTree a) where
  eq (Leaf x) (Leaf y) = eq x y
  eq (Node xl xr) (Node yl yr) = eq xl yl && eq xr yr
  eq _ _ = False

instance Show a => Show (BinTree a) where
  show :: Show a => BinTree a -> String
  show (Leaf x) = show x
  show (Node a b) = "(" ++ show a ++ " " ++ show b ++ ")"

-- > show tree = "((2 4) 5)"
