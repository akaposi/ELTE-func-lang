import Prelude

-- Kaposi Ambrus
-- https://github.com/akaposi/ELTE-func-lang

-- ring = gyűrű = (C,+,0,*,1,-)+csomo egyenloseg pl. (x+y)+z = x+(y+z),
--                                                    x*(y+z) = x*y + x*z
--                       Abel csoport = kommutativ egysegelemes felcsoport
-- commutative exponential rig = commutative exponential semiring
{-
(C,+,0,*,1,^)
(x+y)+z = x+(y+z)     (x*y)*z = x*(y*z)
0+x = x               1*x = x           
x+y = y+x             x*y = y*x         
       x*(y+z) = x*y + x*z
(a^b)^c = a^(b*c)
a^(b+c) = (a^b)*(a^c) 
(a*b)^c = (a^c)*(b^c)
a^0 = 1
a^1 = a
1^a = 1            
-}

data Empty

-- +   Either
-- 0   Empty
-- *   (,)
-- 1   ()
-- ^   flip (->)
{-
Either (Either a b) c ≅ Either a (Either b c)  -- (x+y)+z = x+(y+z)
(Bool,a) ≅ Either a a  -- 2*a = (1+1)*a = 1*a + 1*a = a+a
((Bool,String),Int) ≅ (Bool,(String,Int)) -- (x*y)*z = x*(y*z)
(Bool,Either Int String) ≅ Either (Bool,Int) (Bool,String) -- x*(y+z) = x*y + x*z
Maybe         -- _+1
uncurry :: (c->b->a) ≅ ((b,c)->a) :: curry -- (a^b)^c = a^(b*c)
(Either b c) -> a  ≅  (b->a,c->a)  -- a^(b+c) = (a^b)*(a^c) 
c -> (a,b)  ≅  (c->a,c->b)  -- (a*b)^c = (a^c)*(b^c)
Empty -> a  ≅  ()  -- a^0 = 1
() -> a  ≅  a -- a^1 = a
a -> ()  ≅  ()  -- 1^a = 1     
-}
data Nat = Zero | Suc Nat

instance Num Nat where
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  -- fromInteger 0     = Zero
  -- fromInteger (1+n) = Suc (fromInteger n)
  fromInteger n | n == 0 = Zero
                | n > 0  = Suc (fromInteger (n-1))
  (-) = error "sorry, we were lying"

instance Show Nat where
  show n = show (conv n)
     where conv Zero = 0
           conv (Suc n) = 1 + conv n

-- posdiff a b = Just (Left  x)  --- a = b+x+1
-- posdiff a b = Just (Right x)  --- b = a+x+1
posdiff :: Nat -> Nat -> Maybe (Either Nat Nat)
posdiff Zero Zero = Nothing
posdiff (Suc n) (Suc m) = posdiff n m
posdiff Zero (Suc m) = Just (Right m)
posdiff (Suc n) Zero = Just (Left n)

-- Hilbert <-> Brouwer

-- leq a b = Nothing, ha a > b
-- leq a b = Just b, ha a <= b
leq :: Nat -> Nat -> Maybe Nat
leq Zero b = Just b
leq (Suc a) Zero = Nothing
-- leq (Suc a) (Suc b) = case (leq a b) of
--   Nothing -> Nothing
--   Just n  -> Just (Suc n)
-- leq (Suc a) (Suc b) = help (leq a b)
--   where
--     help Nothing = Nothing
--     help (Just n) = Just (Suc n)
leq (Suc a) (Suc b) =
  let help = \w -> case w of
                   Nothing -> Nothing
                   Just n  -> Just (Suc n)
  in
      help (leq a b)

not' :: Bool -> Bool
not' b = case b of
  True -> False
  False -> True

x :: Int
-- x = let a = b in let b = 3 in a -- error
x = let a = b ; b = 3 in a -- nincs error

{-
a = p1
b = p2
c = p3
main :: IO ()
main = p4

let a = p1 in
let b = p2 in
let c = p3 in
p4
-}
