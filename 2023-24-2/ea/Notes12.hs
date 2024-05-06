{-# LANGUAGE DataKinds, TypeFamilies #-}

-- NatModel = a :: *, a, (a -> a)

-- (c -> (a,b)) ≅ ((c -> a),(c -> b))           binaris szorzat
-- (c -> ()) ≅ ()                               nullaris szorzat
-- eddig cartesian category
-- (c -> (a -> b)) ≅ ((c,a) -> b)               fgv
-- eddig cartesian closed category
-- (Either a b -> c) ≅ ((a -> c),(b -> c))      binaris osszeg
-- (Empty -> c) ≅ ()                            nullaris osszeg
-- eddig bicartesian closed category

-- (Nat -> c) ≅ (a,(a->a))                      hulyeseg

-- (f :: Nat -> c), (z::c), (s::c->c), f Zero = z, f (Suc n) = s (f n)
-- f = fold z s = iteNat z s

-- az inicialis Nat-algebrabol pontosan egy homomorfizmus megy tetsz. masik Nat-algebraba

-- (C:Set)(z:C)(s:C→C), isContr (Σ(f:Nat→C).f zero = z × (∀ n . f (suc n) = s (f n)))
-- <->
-- (P:Nat→Set)(z:P zero)(s:∀ n → P n → P(suc n)) →
--     Σ(f : (n : ℕ) → P n).f zero = z × ∀ n → f (suc n) = s n (f n)

-- isContr A = Σ(a:A).(a' : A) → a' = a
-- pontrahuzhato

--   (id,ite) : NatModel ≅ (M:NatModel)×NatMorphism ℕ M : fst

{-
induktiv tipus:

             1 + fold f
      1 + Nat -----> 1 + c      1 + d
        |              |          |  
zero,suc|              |f         |g
        |              |          |  
        v    fold f    v          v  
       Nat ----------> c          d  

minden c::*-ra es f::1+c->c-re pontosan egy olyan fold f van, melyre a fenti diagram kommutal

f=z,s
fold f zero = z
fold f (suc n) = s (fold f n)

-}

-- BinTree :: *, Leaf :: Int -> BinTree, Node : BinTree->BinTree->BinTree
-- (c::*)(l::Int->c)(n::c->c->c). ∃!((f :: BinTree->c). f(Leaf i) = l i × f(Node l r) = n (f l) (f r))

-- koinduktiv
-- Stream :: *, hd :: Stream -> Int, tl :: Stream -> Stream
-- (c::*)(h::c->Int)(t::c->c). ∃!((f :: c->Stream). hd(f x) = h x × tl(f x) = f(t x))
{-
  Stream                      Colist
     |                           |
hd,tl|                        des|
     |                           |
     v                           v
  Int×Stream                 Unit+Int×Colist
-}

data Pair a :: * where
  Con :: a -> a -> Pair a

-- data Pair a = Con a a

data RoseTree = Node [RoseTree]

data Nest a = Nil | Cons a (Nest (a,a))

xs :: Nest Int
xs = Cons 3 (Cons (1,2) (Cons ((1,2),(3,4)) Nil))

data Bush a = NilB | ConsB a (Bush (Bush a))

b1,b2 :: Bush Int
b1 = ConsB 3 NilB
b2 = ConsB 3      (ConsB (ConsB 3 NilB) NilB)
--         ^Int   ^Bush(Bush Int)
--                       ^Bush Int   ^Bush (Bush (Bush Int))

-- data Tm = Var String | App Tm Tm | Lam String Tm

-- λx.x    Lam "x" (Var "x")
-- λy.y    Lam "y" (Var "y")

-- refl : (λx.x) ≡ (λy.y)

-- forall a . a -> a
-- forall c . c -> c

-- De Bruijn:

-- data Tm = Var Int | App Tm Tm | Lam Tm

-- λx.x       Lam (Var 0)
-- λx.λy.x    Lam (Lam (Var 1))
-- λx.λy.y    Lam (Lam (Var 0))

-- Lam (Lam (Var 2)) nem letezo valtozora mutat

data Empty
-- well-scoped terms
data Tm a = Var a | App (Tm a) (Tm a) | Lam (Tm (Maybe a))
-- zart term: Tm Empty

idTm :: Tm Empty
idTm = Lam (Var Nothing)

kTm :: Tm Empty -- λx y.x
kTm = Lam (Lam (Var (Just Nothing)))

sndTm :: Tm Empty -- λx y.y
sndTm = Lam (Lam (Var Nothing))

data Nat = Zero | Succ Nat

data Fin :: Nat -> * where
  ZeroFin :: Fin (Succ a)
  SuccFin :: Fin a -> Fin (Succ a)

x :: Fin (Succ Zero)
x = ZeroFin
y :: Fin (Succ (Succ Zero))
y = ZeroFin

data Tm' :: Nat -> * where
  Var' :: Fin a -> Tm' a
  App' :: Tm' a -> Tm' a -> Tm' a
  Lam' :: Tm' (Succ a) -> Tm' a

-- f :: (i :: Nat) -> Tm' i
f :: forall i . Tm' i
f = undefined

data Ty = INT | Ty :->: Ty
data Con = NIL | Con :*: Ty

data Var''' :: Con -> Ty -> * where
  Zero'' :: Var''' (g :*: a) a
  Suc''  :: Var''' g a -> Var''' (g :*: b) a

data Tm'' :: Con -> Ty -> * where
  Const :: Int -> Tm'' g INT
  Var'' :: Var''' g a -> Tm'' g a
  App'' :: Tm'' g (a :->: b) -> Tm'' g a -> Tm'' g b
  Lam'' :: Tm'' (g :*: a) b -> Tm'' g (a :->: b)

id'' :: Tm'' NIL (INT :->: INT)
id'' = Lam'' (Var'' Zero'')

k'' :: Tm'' NIL (a :->: (b :->: a))
k'' = Lam'' $ Lam'' $ Var'' (Suc'' Zero'')

type family EvalTy (a :: Ty) :: * where
  EvalTy INT = Int
  EvalTy (a :->: b) = EvalTy a -> EvalTy b

type family EvalCon (a :: Con) :: * where
  EvalCon NIL = ()
  EvalCon (g :*: a) = (EvalCon g,EvalTy a)

eval :: Tm'' g a -> EvalCon g -> EvalTy a
eval (Var'' Zero'')    (_,i) = i
eval (Var'' (Suc'' x)) (is,_) = eval (Var'' x) is
eval (App'' f a) is = eval f is (eval a is)
eval (Lam'' f) is = \i -> eval f (is,i)
eval (Const n) _ = n

test = eval (App'' id'' (Const 13)) ()
test' = eval (App'' (App'' k'' (Const 13)) id'') ()

-- jovo ora 39+18 perccel rovidebb (ez mar kumulalt ertek)
