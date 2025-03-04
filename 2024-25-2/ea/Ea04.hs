-- algebrai adattipusok algebraiak ket ok miatt:
-- 1. inicialis algebrak (pl. Nat az inicialis "pontozott halmaz endofuggvennyel")
-- 2. a tipusok exponential commutative rig-et alkotnak
{-
Either a (Either b c) ≅ Either (Either a b) c
Either Empty a ≅ a
Either a Empty ≅ a
Either a b ≅ Either b a

((a,b),c) ≅ (a,(b,c))   -- (a,b,c)
((),a) ≅ a
(a,()) ≅ a
(a,b) ≅ (b,a)

(a,Either b c) ≅ (Either a b,Either a c)

(b,c) -> a ≅ b->c->a                 a^(b*c) = (a^b)^c
Either b c -> a ≅ (b->a,c->a)        a^(b+c) = a^b * a^c
Empty ->      a ≅ Unit               a^0 = 1
Unit  ->      a ≅ a                  a^1 = a
a     -> Unit ≅ Unit                 1^a = 1
-}

data Empty

fromEmpty :: Empty -> a
fromEmpty e = case e of

type Empty' = forall a.a

fromEmpty' :: Empty' -> Int
fromEmpty' x = x @Int

fromEmpty'' :: Empty' -> a
fromEmpty'' x = x

-- Haskell = System F + fixpont-kombinator
{-
prog.nyelv = olyan algebrai elmelet, aminek van valamilyen szamitogep algebraj
szintaxis = inicialis algebra

-- legegyszerubb prog.nyelv:
Tm halmaz
:*: binaris operator Tm -> Tm -> Tm
S,K ∈ Tm
ket egyenloseg:
K :*: u :*: v = u
S :*: f :*: g :*: w = f :*: w :*: (g :*: w)

-}
data Tm = Tm :*: Tm | S | K
infixl 9 :*:
-- K :*: u :*: v = u
-- S :*: f :*: g :*: w = f :*: w :*: (g :*: w)

-- altalanositott fgv.kompozicio:
-- (.) :: (c->b->a)->(c->b)->c->a
-- (.) f g w = f w (g w)
{-
System F =  Ty halmaz
            minden A ∈ Ty-ra Tm(A) halmaz
            _⇒_ : Ty × Ty → Ty
            ∀   : (Ty → Ty) → Ty
            identitas fgv. tipusa: ∀ (A ↦ A ⇒ A)
            ures tipus Church kodolasa: ∀ (A ↦ A)
            app : Tm(A⇒B)×Tm(A) → Tm(B)
            App : Tm(∀(F)) →  ⊓ Tm(F(A))
                             A∈Ty
            lam : (Tm(A)→Tm(B)) → Tm(A⇒B)
            Lam :  ⊓ Tm(F(A)) → Tm(∀(F))
                  A∈Ty
            egyenloseg1 : app(lam(f),a) = f(a)
            egyenloseg2 : App(Lam(F),A) = F(A)

Haskell =   System F +
            fix : (Tm(A)→Tm(A)) → Tm(A)
            egyenloseg3 : fix(f) = f(fix(f))

id ∈ Tm(∀(A↦A))
Nat ∈ Ty
App(id) ∈  ⊓ Tm(F(A))
         A∈Ty
App(id,Nat) ∈ Tm(Nat)


X halmaz, es egy Y(x), x∈X halmazcsalad         -- A ⊎ B = {(0,a) | a∈A} ∪ {(1,b) | b∈B}

 ⊔ Y(x) = {(x,y) | x ∈ X, y∈Y(x)} = Y(x₀) ⊎ Y(x₁) ⊎ Y(x₂) ⊎ ...     -- Σ tipus
x∈X

 ⊓ Y(x) = {f : X → ∪ Y(x) | ∀x-re f(x)∈Y(x)} = Y(x₀) × Y(x₁) × Y(x₂) × ...  -- Π tipus
x∈X               x∈X

X:=ℕ,  Y(n) = n×n-es matrixok

 ⊔ (n×n-es matrix)  = negyzetes matrixok halmaza
n∈ℕ

 ⊓ (n×n-es matrix)  = minden n-hez egy n×n-es matrix
n∈ℕ

-}

-- Fast and loose reasoning is morally correct

-- 17.00-kor folytatjuk

-- Eq, Show, Ord folytatás, Magasabbrendű polimorfizmus, Functor, Magasabbrendű megkötések
-- Hajtogatás, Foldable, Semigroup, Monoid
-- Monad, IO

-- Eq :: * -> *
-- Eq a = a -> a -> Bool
-- class Eq a where
--   (==) :: a->a->Bool

-- Num :: * -> *
-- Num a = (a->a->a,a->a,a->a->a,...)

f :: Num a => a -> a
f x = x + x * 3
-- f :: (a->a->a,a->a,a->a->a,Integer->a,...) -> a -> a
-- f ((+),neg,(*),fromInteger,...) x = x + x * fromInteger 3

-- Functor :: (* -> *) -> *
-- Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
-- fmap id = id
-- fmap (f . g) = fmap f . fmap g

-- map :: (a -> b) -> [a] -> [b]
-- map f [] = []
-- map f (x:xs) = f x : map f xs

{-
ezt akarjuk bebizonyitani: map id xs = xs  <->  map id = id
ezt bebizonyitjuk a bemeneti lista (xs) szerinti indukcioval
1. alapeset: map id [] = [] = id []
2. induktiv eset: ha az allitas igaz xs-re, akkor minden x-re igaz
   x:xs-re is
  e : map id xs = xs (indukcios hipotezis)
  e-bol kovetkezik map id (x:xs) = x:xs

  map id (x:xs) =(map def) id x : map id xs =(id def.) x : map id xs =(e i.h.) x:xs
-}

map' :: (a->b)->[a]->[b]
map' f = foldr (\a bs -> f a : bs) []

-- instance Functor [] where
--   fmap = map

-- Tree :: * -> *
data Tree a = Node a (Tree a) (Tree a) | Leaf

instance Functor Tree where
  fmap f (Node a t1 t2) = Node (f a) (fmap f t1) (fmap f t2)
  fmap _ Leaf = Leaf

-- Tree :: *
-- data Tree = Node Tree Tree | Leaf

-- type Id a = a
data Id a = Id a
instance Functor Id where
  fmap :: (a -> b) -> Id a -> Id b
  fmap f (Id a) = Id (f a)

data Const c a = Const c
instance Functor (Const c) where
  fmap :: (a -> b) -> Const c a -> Const c b
  fmap _ (Const c) = Const c
  -- ez nem az fmap _ x = x fuggveny

-- instance Functor (c,) where
-- instance Functor (,c) where
-- instance Functor (Either c) where
-- instance Functor (\a->Either a c)  where

-- instance Functor ((->) c) where
--   fmap :: (a -> b) -> ((->) c a) -> ((->) c b)
--   fmap :: (a -> b) -> (c -> a) -> (c -> b)
--   fmap = (.)

-- instance Functor (->c) where?

-- [Int] = [] Int
egyLista :: [] Int
egyLista = [1,2,3,5]

-- Int -> Int, itt a (->) ket tipusbol csinal egy tipust
-- * -> *, itt a (->) az ket fajtabol (kind-bol) csinal egy kind-ot

-- koinduktiv tipusok potencialisan vegtelen melysegu fak, ezek is Church kodolhatok

-- kov. ora 14 perccel rovidebb
