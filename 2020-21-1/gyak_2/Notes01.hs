module Notes01 where

-- GHCi commands:
--  :l File.hs    Load a file
--  :r            Reload the loaded file
--  expr          Evaluate the expression `expr`
--  :t expr       Display the type of the expression `expr`
--  :i expr       Display information about the name `expr`

-- Typed holes
--   (In vscode: press F8 to display the expected type of a hole, and the types of the variables in scope)
--   (In GHCi: reload to see the same information)

f0 :: ((a, b) -> c) -> b -> a -> c
f0 = _

f1 :: (a -> b) -> a -> (b, a)
f1 = _

-- Algebraic data types

-- (a, b) in Prelude
data Pair a b = Pair a b

-- Either a b in Prelude
data Either' a b = Left' a
                 | Right' b
                 deriving (Show)

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

instance (Show a, Show b) => Show (Pair a b) where
  show x = undefined

instance (Eq a, Eq b) => Eq (Pair a b) where
  x == y = undefined

instance (Eq a, Eq b) => Eq (Either' a b) where
  x == y = undefined

instance Eq a => Eq (Maybe' a) where
  x == y = undefined

instance Eq a => Eq (List a) where
  x == y = undefined

instance Eq a => Eq (BinTree a) where
  x == y = undefined

instance (Show a, Show b) => Show (Pair a b) where
  show x = undefined


-- map in Prelude
mapList :: (a -> b) -> List a -> List b
mapList f Empty       = undefined
mapList f (Cons x xs) = undefined

mapMaybe :: (a -> b) -> Maybe' a -> Maybe' b
mapMaybe x y = undefined

mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree = undefined
