module Lesson03 where

data List a = Nil | Cons a (List a) -- rendes megszokott láncolt lista

{-
t =
              adat1
            /      \
        adat2      adat3
        /  \
    adat4  adat5
-}
data BinaryTree a = Leaf a | Node (BinaryTree a) a (BinaryTree a)
-- ez mindenképpen két irányba ágazik, olyan nincs, hogy csak egy gyereke van. Vagy 0 vagy 2 gyerek.

t :: BinaryTree String
t = Node (Node (Leaf "adat4") "adat2" (Leaf "adat5")) "adat1" (Leaf "adat3")

t2 :: Num a => BinaryTree a
t2 = Node (Node (Leaf 2) 3 (Leaf 5)) 7 (Leaf 11)

height :: Num b => BinaryTree a -> b
height = undefined

treeSum :: Num a => BinaryTree a -> a
treeSum = undefined -- ezt a függvényt érdemes megcsinálni, segít a következő +/- feladatot könnyebben megoldani.

-- írd meg a következő instance-okat
instance Eq a => Eq (BinaryTree a) where
    (==) = undefined

instance Ord a => Ord (BinaryTree a) where -- inorder bejárás
    (<=) = undefined

instance Show a => Show (BinaryTree a) where -- inorder bejárás
    show = undefined

--------------------------------------

instance Foldable List where
    foldr = undefined

instance Foldable BinaryTree where
    foldr = undefined

listSum :: Num a => List a -> a
listSum = undefined

treeSum' :: Num a => BinaryTree a -> a
treeSum' = undefined