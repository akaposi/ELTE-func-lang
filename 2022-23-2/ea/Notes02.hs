{-# language Strict #-}

-- Hajtogatások
------------------------------------------------------------

-- lista hajtogatások
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f b []     = b
-- foldr f b (a:as) = f a (foldr f b as)

-- foldr f   b [x1, x2, x3, x4] = f x1 (f x2 (f x3 (f x4 b)))
-- foldr (+) 0 [x1, x2, x3, x4] = x1 + (x2 + (x3 + (x4 + 0)))

any' :: (a -> Bool) -> [a] -> Bool
any' f as = foldr (\a b -> f a || b) False as

sum' :: [Int] -> Int
sum' = foldr (+) 0

-- sum :: [Int] -> Int
-- sum []     = 0
-- sum (x:xs) = x + sum xs

-- sum :: [Int] -> Int
-- sum xs = go 0 xs where
--   go acc []     = acc
--   go acc (x:xs) = go (acc + x) xs     -- vég-rekurzív, konstans stack helyet
--                                       -- foglal

-- foldr vs foldl : *nem* a left vs. right a lényeg:
--    foldr : potenciálisan lusta bal-jobb bejárás
--    foldl : vég-rekurzív (akkumuláló) bejárás

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f b []     = b
-- foldl f b (a:as) = foldl f (f b a) as   -- vég-rekurzív

sum'' :: [Int] -> Int
sum'' = foldl (+) 0

-- let foo = [f x, f y]  -- "thunk" : lusta érték
--                          tárolva van benne: kód pointer +
--                          változók értékei

f :: Int -> Int -> (Int, Int)
f x y = (x + y, x * y)

      -- struct ((\x y -> x + y), x értéke, y értéke)

      -- minden mintaillesztéshez van egy extra eset
      -- amikor az érték thunk

      -- case _ of
      --   0       -> ...
      --   <thunk> ->

      -- case xs of
      --   x:xs' ->


-- case b of
--   True -> ...
--   False -> ..

--   <thunk> -> ..

-- strict adatmező:
-- szigorú lista

data List a = Nil | Cons !a !(List a)

l1 :: List (Int, Int)
l1 = Cons (10 + 10, 10 * 10) Nil

-- f :: Bool -> Bool
-- f ~x = .......

-- szigorú Haskell hogy kéne, hogy kinézzen:
--    el tudjam érni, hogy 1. soha ne hozzak létre thunkot
--                         2. soha ne kelljen thunkot kiértékelnem

-- foldl (+) 0 [x1, x2, x3] == ((0 + x1) + x2) + x3

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f !b []     = b
foldl' f  b (a:as) = foldl' f (f b a) as   -- vég-rekurzív

-- import Data.Foldable
-- foldl

num :: Int
num = foldl (+) 0 [0..1000000]

-- map = foldr ..
-- foldl = foldr ...
-- concatMap = foldr ...

-- let x = index bigarray i
main = print num


-- foldr más adattípusokra (nem csak listákra)
--------------------------------------------------------------------------------

-- data List a = Nil | Cons a (List a)

foldrList nil cons Nil         = nil
foldrList nil cons (Cons a as) = cons a (foldrList nil cons)


data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show)

foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree leaf node (Leaf a)     = leaf a
foldTree leaf node (Node t1 t2) = node (foldTree leaf node t1)
                                       (foldTree leaf node t2)

sumTree :: Tree Int -> Int
sumTree = foldTree id (+)

-- vég-rekurzív fa összegzés:

sumTree' :: Tree Int -> Int
sumTree' t = go 0 t where
  go acc (Leaf n)   = n + acc
  go acc (Node l r) = go (go acc l) r

-- sumTree' definíció 0 stack frame allokálással
--   (ciklikus doubly linked listák Haskell-ben)
-- data List a = Cons (List a) a (List a)
-- foldr függvényt általánosítani más adatszerkezetkre
