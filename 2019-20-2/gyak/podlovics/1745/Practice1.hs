{-# OPTIONS -Wincomplete-patterns #-}
module Practice1 where

data List a = Nil 
            | Cons a (List a)

ex1 :: [Int] 
ex1 = [1,2,3]
-- [1..3]
-- 1:2:3:[]

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

ex1List :: List Int
ex1List = 1 `Cons` (2 `Cons` (3 `Cons` Nil))
  -- Cons 1 (Cons 2 (Cons 3 Nil))
  -- Cons 1 $ Cons 2 $ Cons 3 $ Nil

lengthL :: List a -> Int
lengthL Nil = 0 
lengthL (Cons x xs) = 1 + lengthL xs

foo :: Int -> Bool 
foo = error "asd"

error' :: [Char] -> a
error' x = error' x

data Nat = Zero
         | Suc Nat 

zero :: Nat
zero = Zero

three :: Nat 
three = Suc $ Suc $ Suc Zero

