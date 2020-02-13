{-# OPTIONS -Wincomplete-patterns #-}
module Practice1 where 

data Nat = Zero | Suc Nat
  deriving Show

zero :: Nat 
zero = Zero 

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
inc _ = undefined 

addNat :: Nat -> Nat -> Nat 
addNat _ _ = undefined 

mulNat :: Nat -> Nat -> Nat 
mulNat _ _ = undefined


