-- {-# options_ghc -fwarn-incomplete-patterns #-}
{-# LANGUAGE ImpredicativeTypes #-}

import Prelude hiding (and)

data Nat = Zero | Suc Nat

instance Show Nat where
  show n = show (f n)
    where
      f :: Nat -> Int
      f Zero = 0
      f (Suc n) = 1+f n

haromE :: Nat -> Bool
haromE (Suc (Suc (Suc Zero))) = True
haromE _ = False

plus :: Nat -> Nat -> Nat
plus Zero y = y
plus (Suc n) y = Suc (plus n y)

-- tipososztaly torvenyek: Num instance-nak gyurunek kell lennie
-- instance Num Nat where
-- ilyet nem tudunk csinalni

-- Either a b = vagy a, vagy b

data Diff a = Less a | Equal | Greater a
  deriving (Show)

-- leq 3 7 = Less 3
-- leq 3 4 = Less 0
-- leq 3 3 = Equal
-- leq 4 3 = Greater 0
-- leq 5 3 = Greater 1
leq :: Nat -> Nat -> Diff Nat
leq Zero Zero = Equal
leq Zero (Suc n) = Less n
leq (Suc m) Zero = Greater m
leq (Suc m) (Suc n) = leq m n

instance Eq Nat where
  m == n = case leq m n of
    Equal -> True
    _     -> False

instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  m <= n = case leq m n of
    Greater _ -> False
    _         -> True

-- tipus vs. halmaz
-- tipus az elvaszthatatlan a termtol

-- x_int :: Int -- Simonyi eredeti
-- a :: A
-- a :: a
-- (3,2) :: (Int,Int)
const :: forall a b . a -> b -> a
const a b = a

-- Hindley-Milner tipusrendszer: ebben csak legkulso forall van.
-- forall a . a -> forall b . b  ≅ forall a b . a -> b

-- adattipusok elkodolhatok Church modszerevel
-- "Haskellben overkill a data"
-- nemdata: ->, forall

ifthenelse :: Bool -> a -> a -> a
-- ifthenelse :: forall a . (forall b . b -> b -> b) -> a -> a -> a
-- ≠             forall a . forall b . (b -> b -> b) -> a -> a -> a
ifthenelse True  x y = x
ifthenelse False x y = y

type Unit = forall a . a -> a
tt :: Unit
tt a = a

type BOOL = forall a. a -> a -> a
true, false :: BOOL
true  x y = x
false x y = y

ite :: BOOL -> a -> a -> a
ite b x y = b x y

-- instance Show BOOL where
--   show b = ite b "TRUE" "FALSE"
sB :: BOOL -> String
sB b = ite b "TRUE" "FALSE"

-- ketelemu tipus Church kodolasa

and :: BOOL -> BOOL -> BOOL
and b b' = ite b b' false

-- Nat Church kodolasa

type NAT = forall a . a -> (a -> a) -> a
zero,one,two :: NAT
zero z s = z
one z s = s z
two z s = s (s z)
suc :: NAT -> NAT
suc n = \z s -> s (n z s)
sN :: NAT -> String
sN n = n "" ('I':)
toInt :: NAT -> Int
toInt n = n 0 (+1)
-- plus' :: NAT -> NAT -> NAT
-- plus' = undefined

-- foldBOOL :: a -> a -> BOOL -> a
foldNAT :: forall a . a -> (a -> a) -> NAT -> a
foldNAT z s n = n z s
-- n :: forall b . b -> (b -> b) -> b
-- z :: a
-- s :: a -> a
-- ??? :: a

plus' :: NAT -> NAT -> NAT
plus' m n = foldNAT @NAT n suc m

data Empty
type EMPTY = forall a . a
-- vanE :: forall a . a
-- vanE = undefined

-- fold = iterator = recursor = catamorphism = eliminator = destruktor = case

foldEMPTY :: forall a . EMPTY -> a
foldEMPTY e = e

foldEMPTY' :: EMPTY -> Int
foldEMPTY' e = e @Int

-- id :: forall a . a -> a
-- forall a . (forall b . b) -> a

-- Lisp alapja tipus nelkuli lambda kalkulus
-- Haskell alapja System F / polimorf tipusos lambda kalkulus

fromInt :: Int -> NAT
fromInt n | n == 0 = zero
          | n > 0  = suc (fromInt (n-1))

data Tree = Node Tree Tree | Leaf
{-
 /\
  /\
-}
t1 :: Tree
t1 = Node Leaf (Node Leaf Leaf)
{-
 /\
/\
-}
t2 :: Tree
t2 = Node (Node Leaf Leaf) Leaf

-- type TREE = forall a . ((Bool -> a) -> a) -> a -> a
type TREE = forall a . (a -> a -> a) -> a -> a
-- HF: konstrukotrok, fold

type LIST b = forall a . (b -> a -> a) -> a -> a
-- (:) = cons = suc, de fel van diszitve egy b-vel
-- []  = nil  = zero

type InfTREE = forall a . ((NAT -> a) -> a) -> a -> a
-- HF: konstr, fold

-- induktiv tipusok (veges melysegu fak) Church kodolasa

-- kov. ora 6 perccel rovidebb
