-- parametrikus polimorfizmus, parametricity

f :: forall a . a -> a
f x = x -- NEM LATJUK

data Empty

g :: Empty -> Empty
g e = e

-- f-nek minden tipusra egyseges modon kell mukodnie

{-
ILYEN NINCS:

f @Bool b = not b
f @Int i  = i + 2
f @(a -> b) g = g
-}

-- de helyette van ad-hoc polimorfizmus

class Fun a where
  f' :: a -> a

instance Fun Bool   where  f' b = not b
instance Fun Int    where  f' i = i + 2
instance Fun (a->b) where  f' g = g

-- f @Bool : Bool -> Bool


-- expression problem.
-- data Day = Monday | Tuesday | Wednesday
-- f :: Day -> Int
-- f Monday = 0
-- f Tuesday = 3
-- class Day { Int f }, class Monday extends Day {f = 0}, class Tuesday extends Day { f = 3 }, ...
-- Int f(Day d) { if (d.instanceOf(Monday)) return 0; ... }

-- hogyan bizonyitjuk be, hogy f = id?

-- 1. kell tudnunk, hogy mi az a Haskell?
--    nyelv = logikai rendszer
--    nyelv = masodrendu algebrai elmelet
-- pl. algebrai elmelet: monoid(A halmaz, u∈A, f:A×A→A fgv., f(u,x) = x, f(x,u)=x, f(f(x,y)z) = f(x,f(y,z))), csoport, gyuru, vektorter, graf
-- lambda kalkulus: Tm halmaz, lam : (Tm → Tm) → Tm, app : Tm×Tm → Tm, β : app(lam(t),u) = t(u)
-- Haskell: kicsit bonyolult (nyelvek tipusrendszere targy)
-- Haskell = System F = Ty halmaz, Tm(A) egy halmaz minden A∈Ty-ra,
--   (⇒) : Ty×Ty→Ty, lam : (Tm(A)→Tm(B))→Tm(A⇒B), app : Tm(A⇒B)×Tm(A) → Tm(B), β : app(lam(t),u) = t(u)
--   (∀) : (Ty→Ty)→Ty, Lam : (Π_{X∈Ty}.Tm(B(X))) → Tm(∀(B)), App_A : Tm(∀(B)) → Tm(B(A)) ahol A∈Ty, β : App_A(Lam(t)) = t(A)
-- System F-re epulo nyelvek: ML, OCaml, Haskell, Clean, Miranda

-- ⇒     ->
-- lam   \
-- app   (space)
-- ∀     forall
-- Lam   nincs jeloles   id :: forall a . a -> a       id = \\a . \ x . x
-- App_A @A              id @Bool :: Bool -> Bool

-- meg lehet adni a Haskellnek egy olyan modelljet, amiben
--   Ty := (A halmaz) es egy P⊂A.
--   Tm(A,P) := ∀x∈A.P(x)
--   (A,P) ⇒ (B,Q) := (A→B, f-re igaz a predikatum, ha P(x)-ből Q(f(x)) következik)
--   lam,app
--   ∀ F := ((A halmaz) es P⊂A → F(A,P).₁, ...)
--     F : ((A halmaz) es egy P⊂A) → ((A halmaz) es egy P⊂A)
--   Lam,App
-- ebben a modellben be tudom latni, hogy f @a x = x

-- alapotlet: Haskell programok megorzik a predikatumokat.
-- f :: forall a . a -> a
-- van egy A tipusom, es egy P⊂A predikatumom, akkor ha x∈P, akkor f @A x ∈ P
-- tfh. hogy van egy A tipus es x :: A. P(y):=(x=y)   P := { y∈A | x = y }
-- ha P(x), akkor P(f @A x)
-- ha x=x, akkor x=f @A x

-- formalis szemantika, theory of programming languages, ITT: Fothizmus(programozaselmelet), formalis szemantika, nyelvek tipusrendszere

-- Haskellben mindent meg lehet csinalni data nelkul, csak forall-t es ->-t hasznalva
-- Church kodolas

-- data () = ()
type Unit = forall a. a -> a
unit :: Unit
unit = \ x -> x

-- data Bool = True | False
type Bool' = forall a. a -> a -> a
true :: Bool'
true = \ x y -> x
false :: Bool'
false = \ x y -> y
ite :: Bool' -> a -> a -> a
ite b x y = b x y
-- ite true x y = true x y = (\x y->x) x y = x
-- ite false x y = false x y = (\x y->y) x y = y

-- data Three = One | Two | Three
type Three = forall a.a->a->a->a

-- data Nat = Zero | Suc Nat
type Nat = forall a.a->(a->a)->a
zero, one, two, three :: Nat
zero  = \z s -> z
one   = \z s -> s z
two   = \z s -> s (s z)
three = \z s -> s (s (s z))
suc :: Nat -> Nat
suc n = \z s -> s (n z s)
-- n :: a -> (a->a)->a
-- Nat iteratora
iteNat :: forall a.a -> (a -> a) -> Nat -> a
iteNat = \z s n -> n z s
-- iteNat z s zero = z
-- iteNatf z s (suc n) = s (iteNat z s n)

-- free theorem: Nat minden eleme "\z s -> s^n z" alaku
-- n :: Nat
-- A tipus, P⊂A, z∈A, P(z), s:A→A, ∀x.P(x)→P(s(x))
--------------------------------------------------
-- P(n @A z s) = (∃m∈ℕ. n @A z s = s^m z)  ???????
-- A := Nat
-- P(x) := (∃m∈ℕ. x = (\z s -> s^m z))
-- z := zero
-- P(z) teljesul
-- s := suc
-- s megorzi P-t
---------------------------------------------------
-- P(n @A z s) = (∃m∈ℕ. n @A z s = (\z s -> s^m z))

-- minden n :: Nat-ra és A típusra és z :: A-ra és s :: A -> A -ra
-- ∃ m∈ℕ . n z s = \x y -> y^m z

-- instance Show Nat where -- nincs olyan Haskell kiterjesztes, ami megengedi polimorf tipusokra instance megadast?

toInt :: Nat -> Int
toInt = iteNat 0 (+1)

-- data List a = Nil | Cons a (List a)
type List a = forall b.b->(a->b->b)->b
-- HF: nil, cons, iteList=foldr

-- koinduktiv Church kodolas:
-- record Stream a = MkStream { head :: a , tail :: Stream a }

-- type Stream a = exists b.((b -> (a,b)),b)

-- exists b . f = forall u. (forall b . f -> u) -> u
-- exists b . b = forall u. (forall b . b -> u) -> u

type PointedType = forall u. (forall b . b -> u) -> u

type Stream a = forall u. (forall b . ((b -> (a,b)),b) -> u) -> u
head :: forall a.Stream a -> a
head s = s f
  where
    f :: forall b . ((b -> (a,b)),b) -> a
    f (g,x) = fst (g x)

tailStr :: forall a.Stream a -> Stream a
tailStr = tailStr

-- genStream :: (b -> a) -> (b -> b) -> b -> Stream a
-- genStream h t seed = 

-- data Stream a = Cons a (Stream a)
