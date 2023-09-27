{-
http://strictlypositive.org/winging-jpgs/
-}

data Four = Four0 | Four1 | Four2 | Four3
-- data Either a b = Left a | Right b
data Product a b = Pair a b -- (a,b)

-- | Either a b | = | a | + | b |
-- | Either Bool Four | = | Bool | + | Four | = 2 + 4 = 6
-- | Product a b | = | a | * | b |

iso1 :: (Bool,Bool) -> Either Bool Bool
iso1 (True, b)  = Left b
iso1 (False,b)  = Right b

iso2 :: Either Bool Bool -> (Bool,Bool)
iso2 (Left b) = (True,b)
iso2 (Right b) = (False,b)

-- "Either Bool Bool" es "(Bool,Bool)" izomorf tipusok (bijekcioban allnak, ugyanannyi informaciot reprezentalnak)
-- iso1 (iso2 (Left b)) = iso1 (True,b) = Left b
-- iso1 (iso2 (Right b)) = ... = Right b
-- iso2 (iso1 x) = x
-- (egyenlosegi erveles)
-- Either Bool Bool ≅ (Bool,Bool)
-- Bool+Bool ≅ (Bool*Bool)
-- (A+B)*C ≅ A*C + B*C
-- ((Either a b),c) ≅ (Either (a,c) (b,c))
-- (A+B)^2 ≅ A^2 + 2*A*B + B^2
-- A^(B*C) ≅ (A^B)^C     ((c,b) -> a) -> (c -> (b -> a))
-- A^(B+C) ≅ A^B * A^C   (Either b c -> a) -> (b -> a , c -> a)
--                       (b -> a) -> (c -> a) -> (Either b c -> a)
-- A^B = (B -> A)
-- 2^3 = 8
-- 3^2 = 9
-- | Bool -> Maybe Bool | = 3^2
f :: Bool -> Maybe Bool 
f True = Nothing
f False = Nothing

-- | Maybe Bool -> Bool | = 2^3
{-
g :: Maybe Bool -> Bool
g Nothing = ?
g (Just True) = ?
g (Just False) = ?
-}

t1,t2,t3 :: Bool
t1 = True
t2 = False
t3 = error "baj van"

-- | Bool * Bool | = 3 * 3 + 1
-- | Bool + Bool | = 3 + 3 + 1

-- call by name                          (lusta)        (\x->x+x+x) (1+1) ~~> (1+1)+(1+1)+(1+1) ~~~~~~~~~> 6                   (\x -> 3) (oriasi szamitas) ~> 3
-- call by value(OCaml, ML)              moho,szigoru   (\x->x+x+x) (1+1) ~> (\x->x+x+x) 2 ~> 2+2+2 ~~~> 6                     (\x -> 3) (oriasi szamitas) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~> 3
-- call by need(Miranda,Clean,Haskell)                  (\x->x+x+x) (1+1) ~> let u:=(1+1) in u+u+u ~> let u:=2 in u+u+u ~~~~> 6
-- Simon Peyton-Jones
-- Miranda: David Turner -- "strong functional programming"
-- Conor McBride: Turing completeness totally free


{-
alap tipusosztalyokat vegignezni
negativ peldak (Eq)

Int :: *
List :: * -> *
data Pair f g a = Pair (f a) (g a)
Pair :: (* -> *) -> (* -> *) -> * -> *
Pair List (Int,) Bool = Pair (List Bool) (Int,Bool)

challenge: ? :: ((* -> *) -> *) -> *

(,)  :: * -> * -> *
(->) :: * -> * -> *
-}
