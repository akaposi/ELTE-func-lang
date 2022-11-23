{-# language GADTs, ScopedTypeVariables, RankNTypes,
    StandaloneDeriving, DataKinds, PolyKinds, TypeFamilies,
    ConstraintKinds #-}
{-# options_ghc -Wincomplete-patterns #-}

import Data.Kind

-- Típusszintű prog, GADTk, type family-k, TypeInType, DataKinds, stb...

-- polimorf, magasabrrendű függvények
-- paraméteres ADT-k
-- egy paraméteres típusosztályok, "higher-kinded"
--   fmap :: Functor f => (a -> b) -> f a -> f b
--   fmap :: forall (f :: * -> *)(a :: *)(b :: *). Functor f =>
--               (a -> b) -> f a -> f b

-- ADT-k --> GADT-k (GADTs)
-- osztályok --> többparaméteres osztályok,
--                "functional dependencies" a parméterek fölött
--                (típusszintű logikai programozás)
--               (MultiParamTypeClasses)
--               (FunctionalDependencies)

-- típusok nyelve:
--    kind-ok nyelve:
--       *, * -> *, * -> * -> *, (* -> *) -> *, stb...
--    bővítés: kind-ok és típusok egységesítése
--       (DataKind, PolyKinds)

-- GADT példák
------------------------------------------------------------

-- hasonló, mint Agda/Coq inductive def
--  megszorítás:

-- data Exp = IntLit Int | BoolLit Bool | And Exp Exp | Mul Exp Exp
--          | Eq Exp Exp

-- eval :: Exp -> Maybe (Either Bool Int)
-- eval = undefined

{-

data Exp :: * -> * where
  IntLit  :: Int -> Exp Int
  BoolLit :: Bool -> Exp Bool
  And     :: Exp Bool -> Exp Bool -> Exp Bool
  Mul     :: Exp Int -> Exp Int -> Exp Int
  Eq      :: Eq a => Exp a -> Exp a -> Exp Bool

deriving instance Show (Exp a)

eval :: Exp a -> a
eval (IntLit n) = n
eval (BoolLit b) = b
eval (And e1 e2) = eval e1 && eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq e1 e2)  = eval e1 == eval e2

e1 :: Exp Bool
e1 = Eq (Eq (IntLit 10) (IntLit 20)) (And (BoolLit True) (BoolLit False))

-}

-- DataKinds, PolyKinds
-- --> Minden ADT deklaráció két másolatban létezik:
--      1. Értékek szintjén
--      2. Típusok szintjén

-- data MyBool = MyTrue | MyFalse
--   deriving (Eq, Show)

myNot :: Bool -> Bool
myNot = undefined

data IsTrue :: Bool -> * where -- bizonyítás, hogy valamilyen b ~ True
                               -- a típusszinten
  ItIsTrue :: IsTrue True

-- p :: IsTrue True
-- p = ItIsTrue
-- f :: IsTrue b -> ...

-- data PropEq :: forall a. a -> a -> * where
--   Refl :: PropEq x x

-- leszűkítem a lehetséges Exp típusokat csak Bool/Int-re
data ExpType = BoolTy | IntTy deriving (Eq, Show)

data Exp :: ExpType -> * where
  IntLit  :: Int -> Exp IntTy
  BoolLit :: Bool -> Exp BoolTy
  And     :: Exp BoolTy -> Exp BoolTy -> Exp BoolTy
  Mul     :: Exp IntTy -> Exp IntTy -> Exp IntTy
  Eq      :: SExpType a -> Exp a -> Exp a -> Exp BoolTy

data Val :: ExpType -> * where
  IntVal :: Int -> Val IntTy
  BoolVal :: Bool -> Val BoolTy

eval1 :: Exp a -> Val a
eval1 (IntLit n)  = IntVal n
eval1 (BoolLit b) = BoolVal b
eval1 (And e1 e2) = case (eval1 e1, eval1 e2) of
                      (BoolVal b1, BoolVal b2) -> BoolVal (b1 && b2)
eval1 (Mul e1 e2) = case (eval1 e1, eval1 e2) of
                      (IntVal b1, IntVal b2) -> IntVal (b1 * b2)
eval1 (Eq _ e1 e2) = case (eval1 e1, eval1 e2) of
                      (IntVal n1 , IntVal n2)  -> BoolVal (n1 == n2)
                      (BoolVal b1, BoolVal b2) -> BoolVal (b1 == b2)

-- típusszintű függvény:
type family EvalExpType (a :: ExpType) :: * where
  EvalExpType IntTy  = Int
  EvalExpType BoolTy = Bool


-- ExpTy futásidejű reprezentációja
--  library ehhez ("singletons")
data SExpType :: ExpType -> * where
  SIntTy  :: SExpType IntTy
  SBoolTy :: SExpType BoolTy

eval2 :: Exp a -> EvalExpType a
eval2 (IntLit n)  = n
eval2 (BoolLit b) = b
eval2 (And e1 e2) = eval2 e1 && eval2 e2
eval2 (Mul e1 e2) = eval2 e1 * eval2 e2
eval2 (Eq SIntTy e1 e2) = eval2 e1 == eval2 e2
eval2 (Eq SBoolTy e1 e2) = eval2 e2 == eval2 e2

-- definiláljuk a függvényt, ami
--  ExpType futásidejű rep-jéből visszaadegy Eq instance-ot

-- ConstraintKinds

-- valamilyen instance
data Inst (c :: Constraint) :: * where
  Inst :: c => Inst c

foo :: Inst (Functor [])
foo = Inst

expTypeEq :: SExpType a -> Inst (Eq (EvalExpType a))
expTypeEq SIntTy  = Inst
expTypeEq SBoolTy = Inst

eval3 :: Exp a -> EvalExpType a
eval3 (IntLit n)  = n
eval3 (BoolLit b) = b
eval3 (And e1 e2) = eval3 e1 && eval3 e2
eval3 (Mul e1 e2) = eval3 e1 * eval3 e2
eval3 (Eq tyrep e1 e2) = case expTypeEq tyrep of
                           Inst -> eval3 e1 == eval3 e2

-- "Nested ADT" : GADT-k nélkül is lehet varázsolni
--  "Nested": nem ugyanazt a paraméterezést használjuk rekurzívan
--  mint amit az ADT deklaráció kap.

-- alternating list
data AltList a b = ANil | ACons a (AltList b a)

-- teljes bináris fák típus

data Tree a = Leaf a | Node (Tree (a, a))

-- t1 :: Tree Int
-- t1 = Leaf 0

-- t2 :: Tree Int

-- t2 = Node (Leaf (0, 0))
-- t3 = Node (Node (Leaf ((0, 1), (2, 3))))
-- t4 = Node (Node (Leaf (((0, 1), (2, 3)), ((0, 1), (2, 3)))))


data List a = Nil | Cons a (List (List a))

t1, t2 :: List Int
t1 = Nil
t2 = Cons 0 Nil
t3 = Cons 0 (Cons (Cons 0 Nil) Nil)
t4 = Cons 0 (Cons (Cons 0 Nil) (Cons (Cons (Cons 0 Nil) Nil) Nil))

-- List típus "értelme": referencia: "generic trie data structures"

----------------------------------------------------------------------

data Nat = Z | S Nat      -- a + (b + c) = (a + b) + c

data Vec :: * -> Nat -> * where
  VNil   :: Vec a Z
  VCons  :: a -> Vec a n -> Vec a (S n)

vmap :: (a -> b) -> Vec a n -> Vec b n
vmap f VNil         = VNil
vmap f (VCons a as) = VCons (f a) (vmap f as)


-- HList [Int, Int, Bool] -- generikus rekord/tuple mint GADT

data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: a -> HList types -> HList (a ': types)

hlist1 :: HList [Int, Int, Bool, String]
hlist1 = HCons 0 $ HCons 10 $ HCons True $ HCons "foo" HNil

-- type family Elem a as = ....
--   HList as -> a
