{-# options_ghc -Wincomplete-patterns #-}

module Lesson03 where

f2 :: (a -> b) -> (a -> b)
f2 = id

f3 :: (b -> c) -> (a -> b) -> (a -> c)
f3 = (.)

f4 :: (a -> b -> c) -> (b -> a -> c)
f4 = flip

f5 :: ((a, b) -> c) -> (a -> b -> c)
f5 = curry

f10 :: Either (a, b) (a, c) -> (a, Either b c)
f10 (Left (a, b)) = (a, Left b)
f10 (Right (a, c)) = (a, Right c)

f11 :: (a, Either b c) -> Either (a, b) (a, c)
f11 (a, Left b) = Left (a, b)
f11 (a, Right c) = Right (a, c)

f12 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f12 f g = f (g (\a -> f a a)) (g (\a -> f a a))

-- e.g. "composeAll [f, g, h] x == f (g (h x))"
composeAll :: [a -> a] -> a -> a
composeAll [] a = a
composeAll (f:fs) a = f (composeAll fs a)

-- [f, g, h]
-- 0. \x -> x == (id)
-- 1. \x -> h x == (h . id)
-- 2. \x -> g (h x) == (g . h . id)
-- 3. \x -> f (g (h x)) == (f . g . h . id)

composeAll' :: [a -> a] -> (a -> a)
composeAll' fs = foldr (.) id fs

funs :: [Float -> Float]
funs = [(/2), (+6), (*4)]

-- Further practice:
-- https://github.com/AndrasKovacs/ELTE-func-lang/blob/ccf1619a81d446a45ca113e5c25382f86fd7f52e/2020-21-2/gyak_1/feladatok01.hs


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

-- Tree
data Tree a = Leaf a | Node (Tree a) (Tree a)

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


{- Foldable -}

instance Foldable List where
  foldr f acc Nil = acc
  foldr f acc (Cons x xs) = f x (foldr f acc xs)

instance Foldable Tree where
  foldr f acc (Leaf x)   = f x acc
  foldr f acc (Node l r) = (foldr f (foldr f acc r) l)
    -- let acc' = (foldr f acc r) in (foldr f acc' l)

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
  (<>) = (+)
instance Monoid Int where
  mempty = 0

-- Int (multiplication)
-- instance Semigroup Int where
--   (<>) = (*)
-- instance Monoid Int where
--   mempty = 1

-- Int (maximum)
-- instance Semigroup Int where
--   (<>) = max
-- instance Monoid Int where
--   mempty = minBound

-- Int (minimum)
-- instance Semigroup Int where
--   (<>) = min
-- instance Monoid Int where
--   mempty = maxBound

-- List
instance Semigroup (List a) where
  -- Nil <> ys = ys
  -- (Cons x xs) <> ys = Cons x (xs <> ys)
  xs <> ys = foldr Cons ys xs
instance Monoid (List a) where
  mempty = Nil

autoFold :: Monoid m => Foldable f => f m -> m
autoFold fm = foldr mappend mempty fm
-- autoFold fm = foldr (<>) mempty fm

-- EndoFunction
data EndoFunction a = MkEndoFun (a -> a)

runEndoFun :: EndoFunction a -> a -> a
runEndoFun (MkEndoFun f) = f

funs' :: [EndoFunction Float]
funs' = map MkEndoFun funs

instance Semigroup (EndoFunction a) where
  (MkEndoFun f) <> g = MkEndoFun (f . (runEndoFun g))

instance Monoid (EndoFunction a) where
  mempty = MkEndoFun id

test :: Float
test = runEndoFun (autoFold funs') 5
