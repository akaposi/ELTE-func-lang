-- Ismétlés, parametrikus ADTk ((,) ,Either, ->), case, let kötések, Eq, Show, Ord

data Nat = Zero | Suc Nat

-- t : A

-- monoidok elmelete: c :: *, op :: c->c->c, e::c, (x`op`y)`op`z = x`op`(y`op`z),
--                    e `op` x = x, x `op` e = x
class Monoid c where
  op :: c->c->c
  e::c

-- egyenlosegek nelkuli monoidok (pontozott halmaz binaris
-- operatorral) inicialis algebraja:
data IM = Op IM IM | E
{-
IM-ben az Op-nak E nem identitas eleme, mert:

  Op                Op        
  /\  ≠  x          /\  ≠  x  
 x  E              E  x       

IM-ben az Op nem asszociativ, mert:

 /\  ≠  /\
/\       /\

Op (Op E E) E ≠ Op E (Op E E)
-}

-- igazi monoidik inicialis algebraja: ()

-- a feletti monoid:
-- c :: *, op::c->c->c, e:c, incl::a->c, ass,idl,idr
-- inicialis algebraja: [a]


-- f :: (Monoid c, Monoid d) => c -> d
-- f c.e = d.e
-- f (x `c.op` y) = f x `d.op` f y


-- algebrai_1: Nat-algebrak elmelete:
-- (pontozott halmaz endofuggvennyel)
class NatAlgebra c where
  zero :: c
  suc  :: c -> c

-- Nat az inicialis NatAlgebra
foldNat :: NatAlgebra c => Nat -> c
foldNat Zero = zero
foldNat (Suc n) = suc (foldNat n)

-- [a] az az inicialis a-lista algebra
class ListAlgebra c where
  nil :: c
  cons :: a -> c -> c

foldList :: ListAlgebra c => [a] -> c
foldList [] = nil
foldList (x:xs) = cons x (foldList xs)

append :: [a] -> [a] -> [a]
-- xs `append` ys = foldList (nil:=ys; cons:=(:)) xs
xs `append` ys = foldr (:) ys xs
-- append []     ys = ys
-- append (x:xs) ys = x : append xs ys

-- foldList = foldr

-- a-knak a rendezetlen parja -- olyan algebrai strutktura, aminek Haskellben nincs inicialis algebraja (Agdaban van)

-- (a,a)

-- 16.53-kor folytatjuk

-- tipusok "exp commutative rig"
-- exponential rig
-- rig = ring without negation
-- alaphalmaz = *
-- matek: 0,1,+,*,^
-- ^,*,+,1,0
-- a ^ b := b -> a
-- a * b := (a,b)
-- a + b := Either a b
-- 1     := Unit
-- 0     := Empty
data Empty
-- tipusok azok exp comm rig-et alkotnak, ahol az egyenloseg izomorfizmus
-- (a,b) = (b,a)
-- f :: (a,b) -> (b,a)
-- f (x,y) = (y,x)
-- g :: (b,a) -> (a,b)
-- g (x,y) = (y,x)
-- f . g = id,  g . f = id
-- Either a (Either b c) ≅ Either (Either a b) c
f :: Either a (Either b c) -> Either (Either a b) c
f (Left a) = Left (Left a)
f (Right (Left b)) = Left (Right b)
f (Right (Right c)) = Right c
g :: Either (Either a b) c -> Either a (Either b c)
g (Left (Left a)) = Left a
g (Left (Right b)) = Right (Left b)
g (Right c) = Right (Right c)
{-
f . g = id
id x = x
(f . g) x = x = id x :: Either (Either a b) c -> Either (Either a b) c
(f . g) (Left (Left a)) = 
f (g (Left (Left a))) = 
f (Left a) = 
Left (Left a)
(f . g) (Left (Right b)) =  .... = Left (Right b)
(f . g) (Right c) =  .... = Right c
es a (g . f) iranyt is, akkor megadtuk a + asszoc "egyenloseget"

Either Empty a ≅ a
Either a Empty ≅ a
Either a b ≅ Either b a

((a,b),c) ≅ (a,(b,c))   -- (a,b,c)
((),a) ≅ a
(a,()) ≅ a
(a,b) ≅ (b,a)

(a,Either b c) ≅ (Either a b,Either a c)

a^(b*c) = (a^b)^c       curry :: (b,c) -> a   ≅   b->c->a :: uncurry
a^(b+c) = a^b * a^c
a^0 = 1
a^1 = a
1^a = 1

-}
