{-# OPTIONS -Wincomplete-patterns #-}
module Practice1 where 

data Nat = Zero | Suc Nat
  deriving Show

zero :: Nat 
zero = Zero 

two :: Nat 
two = Suc (Suc Zero)

three :: Nat 
three = Suc (Suc (Suc Zero))

isZero :: Nat -> Bool 
isZero Zero = True 
isZero _    = False

eqNat :: Nat -> Nat -> Bool 
eqNat Zero Zero = True 
eqNat (Suc n) (Suc m) = eqNat n m
eqNat _ _ = False

inc :: Nat -> Nat
inc = Suc

addNat :: Nat -> Nat -> Nat 
addNat Zero n    = n 
addNat (Suc m) n = Suc (addNat m n) 

mulNat :: Nat -> Nat -> Nat 
mulNat Zero _    = Zero 
mulNat (Suc m) n = addNat n (mulNat m n) 
-- (m+1) * n = n + m*n

ltNat :: Nat -> Nat -> Bool 
ltNat _ Zero          = False 
ltNat Zero _          = True 
ltNat (Suc n) (Suc m) = ltNat n m

-- listák ...

data List a = Nil             -- []
            | Cons a (List a) -- (:)
  deriving Show

lengthL :: List a -> Int
lengthL Nil = 0
lengthL (Cons x xs) = 1 + lengthL xs

eqL :: Eq a => List a -> List a -> Bool
eqL Nil Nil = True 
eqL (Cons x xs) (Cons y ys) = x == y && eqL xs ys
eqL _ _ = False