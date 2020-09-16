module Notes01 where

-- GHCi commands:
--  :l File.hs    Load a file
--  :r            Reload the loaded file
--  expr          Evaluate the expression `expr`
--  :t expr       Display the type of the expression `expr`
--  :i expr       Display information about the name `expr`

-- Typed holes
--   (In vscode: press F8 to display the expected type of a hole,
--               and the types of the variables in scope)
--   (In GHCi: reload to see the same information)

f0 :: ((a, b) -> c) -> b -> a -> c
-- f0 = undefined
-- f0 g b a = _
-- f0 g b a = g _
-- f0 g b a = g (_ , _)
f0 g b a = g (a, b)

-- f1 :: _

-- Algebraic data types

-- (a, b) in Prelude
data Pair a b = Pair a b

-- Either a b in Prelude
data Either' a b = Left' a
                 | Right' b

-- Maybe a in Prelude
data Maybe' a = Just' a 
              | Nothing'
              deriving (Show)

-- [a] in Prelude
data List a = Empty 
            | Cons a (List a)
            deriving (Show)

data BinTree a = Leaf a
               | Node (BinTree a) (BinTree a)
               deriving (Show)

-- Show, Eq and Ord instances

-- class Show a where
--   show :: a -> String

-- class Eq a where
--   (==) :: a -> a -> Bool

-- class Eq a => Ord a where
--   (<=) :: a -> a -> Bool

instance (Show a, Show b) => Show (Pair a b) where
  show (Pair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"

instance (Eq a, Eq b) => Eq (Pair a b) where
  Pair x1 x2 == Pair y1 y2 = (x1 == y1) && (x2 == y2)

instance (Ord a, Ord b) => Ord (Pair a b) where
  Pair x1 x2 <= Pair y1 y2
    | x1 == y1  = x2 <= y2
    | otherwise = x1 < y1

instance (Show a, Show b) => Show (Either' a b) where
  show (Left' x) = "(Left' " ++ show x ++ ")"
  show (Right' x) = "(Right' " ++ show x ++ ")"

instance (Eq a, Eq b) => Eq (Either' a b) where
  Left' x  == Left' y  = (x == y)
  Right' x == Right' y = (x == y)
  _        == _        = False

instance (Ord a, Ord b) => Ord (Either' a b) where
  Left' x  <= Left' y  = (x <= y)
  Left' x  <= _        = True
  _        <= Left' _  = False
  Right' x <= Right' y = (x <= y)

instance Eq a => Eq (Maybe' a) where
  Nothing' == Nothing' = True
  Just' x  == Just' y  = x == y
  _        == _        = False

instance Eq a => Eq (List a) where
  Empty     == Empty     = True
  Cons x xs == Cons y ys = (x == y) && (xs == ys)
  _         == _         = False

instance Eq a => Eq (BinTree a) where
  Leaf x     == Leaf y     = x == y
  Node x1 x2 == Node y1 y2 = (x1 == y1) && (x2 == y2)
  _          == _          = False
