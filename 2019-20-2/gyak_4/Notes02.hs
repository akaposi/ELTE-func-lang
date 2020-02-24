{-# LANGUAGE InstanceSigs #-}
module Notes02 where

-- xs = [x1, x2, x3, ...]    x1 <= x2, x2 <= x3, ...
-- ys = [y1, y2, y3, ...]    y1 <= y2, y2 <= y3, ...
--   merge xs ys  is the sorted list with the elements of xs ys
--   merge [1, 4, 9] [2, 3, 8] = [1, 2, 3, 4, 8, 9]
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
merge [] ys = ys
merge xs [] = xs

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = let (ys, zs) = splitAt ((length xs + 1) `div` 2) xs
               in merge (mergeSort ys) (mergeSort zs)


-- group' [x,y,y,x,z,z] = [[x],[y,y],[x],[z,z]]
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [x] = [[x]]
group' (x:xs) = let (y:ys):yss = group' xs
                in if x == y then (x:y:ys):yss
                             else [x]:(y:ys):yss

group'' :: Eq a => [a] -> [[a]]
group'' [] = []
group'' (x:xs) = (x : takeWhile (==x) xs) : group'' (dropWhile (==x) xs)


data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

-- sum all elements in a tree
sumTree :: Num a => Tree a -> a
sumTree (Leaf x) = x
sumTree (Node l r) = sumTree l + sumTree r

-- list all elements in a tree
flattenTree :: Tree a -> [a]
flattenTree (Leaf x) = [x]
flattenTree (Node l r) = flattenTree l ++ flattenTree r
