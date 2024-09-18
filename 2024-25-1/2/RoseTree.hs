module RTree where

data RoseTree a = Node a [RoseTree a]

instance Show a => Show (RoseTree a) where
    show (Node a []) = "Leaf " ++ show a
    show (Node a xs) = "Node " ++ show a ++ " " ++ show xs

{-
>>> show (Node 1 []) == "Leaf 1"
True
>>> show (Node 1 [(Node 2 [])]) == "Node 1 [Leaf 2]"
True
>>> show (Node 1 [(Node 2 []), (Node 3 [])]) == "Node 1 [Leaf 2,Leaf 3]"
True
-}

instance Eq a => Eq (RoseTree a) where
    (Node a xs) == (Node b ys) = a == b && xs == ys

{-
>>>(Node 1 []) /= (Node 2 [])
>>>(Node 1 []) /= (Node 1 [(Node 2 [])])
>>>(Node 1 [(Node 2 [])]) == (Node 1 [(Node 2 [])])
True
True
True
-}

