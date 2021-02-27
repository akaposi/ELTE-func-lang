{-# options_ghc -Wincomplete-patterns #-}

module Lesson03 where

{- Usual ADTs -}

-- List
data List a = Nil | Cons a (List a)

infixr 5 -+-
(-+-) :: a -> List a -> List a
(-+-) a as = Cons a as

instance Show a => Show (List a) where
  show Nil = ""
  show (Cons x Nil) = show x
  show (Cons x xs) = show x ++ ", " ++ (show xs)

l :: List Int
l = 3 -+- 8 -+- 15 -+- Nil

l' :: List String
l' = "this" -+- "is" -+- "a" -+- "test" -+- Nil

-- Tree
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

t :: Tree Int
t = Node (Node (Leaf 7) (Leaf 10)) (Node (Leaf 3) (Leaf 4))

t' :: Tree (List Bool)
t' =
  Node
    (Node
      (Leaf (True -+- True -+- Nil))
      (Leaf (False -+- False -+- Nil)))
    (Leaf (True -+- Nil))

t'' :: Tree String
t'' =
  Node
    (Node
      (Leaf "lorem")
      (Node
        (Leaf "ipsum")
        (Leaf "dolor")))
    (Node
      (Leaf "sit")
      (Leaf "amet"))


{- Functor -}

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just a) = (Just (f a))

mapList :: (a -> b) -> List a -> List b
mapList f Nil = Nil
mapList f (Cons a as) = Cons (f a) (mapList f as)

-- Cons :: a -> (List a -> List a)

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b
--
--     -- fmap id valami = valami

instance Functor List where
  fmap = mapList

instance Functor Tree where
--fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)


{- Foldable -}

-- e.g. "composeAll [f, g, h] x == f (g (h x))"

-- composeAll [] x == (x)
-- composeAll [h] x == (h (x))
-- composeAll [g, h] x == g (h (x))
-- composeAll [f, g, h] x == f (g (h (x)))

-- [f, g, h]
-- 0. \x -> x == (id)
-- 1. \x -> h x == (h . id)
-- 2. \x -> g (h x) == (g . h . id)
-- 3. \x -> f (g (h x)) == (f . g . h . id)

composeAll :: [a -> a] -> a -> a
composeAll [] a = a
composeAll (f:fs) a = f (composeAll fs a)
-- composeAll fs a = head fs (composeAll (tail fs) a)

composeAll' :: [a -> a] -> a -> a
-- composeAll' = foldr (\f acc -> f . acc) id
composeAll' = foldr (.) id

funs :: [Float -> Float]
funs = [(/2), (+6), (*4)]

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

fold'' :: (a -> b -> b) -> b -> [a] -> b
fold'' f b [] = b
fold'' f b (x:xs) = f x (fold'' f b xs)

sum'' = fold'' (+) 0
length'' = fold'' (\x acc -> 1 + acc) 0

instance Foldable List where
  foldr f acc Nil = acc
  foldr f acc (Cons x xs) = f x (foldr f acc xs)

instance Foldable Tree where
  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f acc (Leaf x) = f x acc
  foldr f acc (Node l r) = foldr f (foldr f acc r) l
  -- foldr f acc (Node l r) = let
  --                            fr = (foldr f acc r)
  --                          in
  --                            (foldr f fr l)

treeSum :: Tree Int -> Int
treeSum = foldr (+) 0


{-
   # Semigroup and Monoid laws #

   class Semigroup a where
     (<>) :: a -> a -> a -- infix: (<>)

   - Associativity: a <> (b <> c) = (a <> b) <> c

   class Semigroup a => Monoid a where
     mempty :: a

   - Left  Identity: mempty <> a = a
   - Right Identity: a <> mempty = a
-}

-- Int (addition)
instance Semigroup Int where
  (<>) = undefined
instance Monoid Int where
  mempty = undefined

-- Int (multiplication)
-- instance Semigroup Int where
--   (<>) = undefined
-- instance Monoid Int where
--   mempty = undefined

-- Int (maximum)
-- instance Semigroup Int where
--   (<>) = undefined
-- instance Monoid Int where
--   mempty = undefined

-- Int (minimum)
-- instance Semigroup Int where
--   (<>) = undefined
-- instance Monoid Int where
--   mempty = undefined

-- List
instance Semigroup (List a) where
  xs <> ys = undefined
instance Monoid (List a) where
  mempty = undefined

autoFold :: Monoid m => Foldable f => f m -> m
autoFold = undefined

-- EndoFunction
data EndoFunction a = MkEndoFun (a -> a)

runEndoFun :: EndoFunction a -> a -> a
runEndoFun (MkEndoFun f) = f

funs' :: [EndoFunction Float]
funs' = map MkEndoFun funs

instance Semigroup (EndoFunction a) where
  (<>) = undefined

instance Monoid (EndoFunction a) where
  mempty = undefined

test :: Float -> Float
test = runEndoFun (autoFold funs')


-- Extra discussion:

f2 :: (a -> b) -> (a -> b)
f2 = id

f3 :: (b -> c) -> (a -> b) -> (a -> c)
f3 = (.)

f4 :: (a -> b -> c) -> (b -> a -> c)
f4 = flip

f5 :: ((a, b) -> c) -> (a -> (b -> c))
f5 = curry

-- f10 :: Either (a, b) (a, c) -> (a, Either b c)
-- f10 = undefined

-- f11 :: (a, Either b c) -> Either (a, b) (a, c)
-- f11 = undefined

f12 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f12 f g = f (g (\a -> (f a a))) (g (\a -> (f a a)))

-- Further practice:
-- https://github.com/AndrasKovacs/ELTE-func-lang/blob/ccf1619a81d446a45ca113e5c25382f86fd7f52e/2020-21-2/gyak_1/feladatok01.hs
