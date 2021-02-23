{-# options_ghc -Wincomplete-patterns #-}

module Lesson03 where

f2 :: (a -> b) -> a -> b
f2 f a = f a

f3 :: (b -> c) -> ((a -> b) -> (a -> c))
f3 f g a = f (g a)

f4 :: (a -> b -> c) -> (b -> a -> c)
f4 f b a = f a b

f5 :: ((a, b) -> c) -> a -> b -> c
f5 f a b = f (a, b)

f10 :: Either (a, b) (a, c) -> (a, Either b c)
f10 = undefined

f11 :: (a, Either b c) -> Either (a, b) (a, c)
f11 = undefined

f12 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f12 = undefined

-- e.g. "composeAll [f, g, h] x == f (g (h x))"
composeAll :: [a -> a] -> a -> a
composeAll fs a = undefined

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
  foldr f acc l = undefined

instance Foldable Tree where
  foldr f acc t = undefined

treeSum :: Tree Int -> Int
treeSum t = undefined


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

test :: Float
test = runEndoFun (autoFold funs') 5
