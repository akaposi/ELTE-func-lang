module Notes02 where

mapList :: (a -> b) -> [a] -> [b]
mapList = undefined

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe = undefined

mapPair :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapPair = undefined

mapEither :: (a -> b) -> (c -> d) -> Either a c -> Either b d
mapEither = undefined

-- Define f and g using mapList, mapMaybe, mapPair, mapEither, ...
f :: (a -> b) -> [[a]] -> [[b]]
f = undefined

g :: (a -> b) -> [(a, a)] -> [(a, b)]
g = undefined

data Tree1 a = Leaf1 a
             | Node1 (Tree1 a) (Tree1 a)

mapTree1 :: (a -> b) -> Tree1 a -> Tree1 b
mapTree1 = undefined

data Tree2 a = Leaf2 a
             | Node2 [Tree2 a]

mapTree2 :: (a -> b) -> Tree2 a -> Tree2 b
mapTree2 = undefined

-- Only look below this line after you have defined all the functions above

mapFun :: (a -> b) -> (Int -> a) -> (Int -> b)
mapFun = undefined

data Tree3 a = Leaf3 a
             | Node3 (Int -> [Tree3 a])
mapTree3 :: (a -> b) -> Tree3 a -> Tree3 b
mapTree3 = undefined


bindList :: (a -> [b]) -> [a] -> [b]
bindList = undefined
-- example: bindList (\x -> [x, x+1]) [1, 2] == [1, 2, 2, 3]

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe = undefined
-- example:
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) Nothing      == Nothing
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) (Just True)  == Nothing
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) (Just False) == Just True

bindTree1 :: (a -> Tree1 b) -> Tree1 a -> Tree1 b
bindTree1 = undefined
-- example:
--  bindTree1 (\x -> if x then Leaf1 0 else Node1 (Leaf1 0) (Leaf1 1))
--            (Node1 (Leaf1 True) (Leaf1 False)))
--  == Node1 (Leaf1 0) (Node1 (Leaf1 0) (Leaf1 1))
