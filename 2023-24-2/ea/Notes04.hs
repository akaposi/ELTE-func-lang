-- {-# LANGUAGE ExplicitForAll #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

import Data.Functor.Contravariant

-- Monad, State, IO

-- data F a = Mk a a a a... a ...
data F a = Mk { un :: (a -> Int) -> Bool }
  deriving Functor
{-
fmap (f.g) = fmap f.fmap g
fmap id = id

 (f :: a -> b) -> (a -> Int) -> (b -> Int)
 
      ?
  b ----> Int
  |       ||
  v       ||
  a ----> Int

 (f :: a -> b) -> ((a->Int)->Bool) -> ((b->Int)->Bool)

           α
  (a->Int)--->Bool

  a
  |f                   α (g.f) :: Bool
  v
  b--->Int
    g
-}

fmapF :: (a -> b) -> F a -> F b
fmapF f (Mk α) = Mk (\g -> α (g.f))

-- un (fmapF (+3) (Mk (\g->g 3 > 5))) (*2)
-- un (fmapF (+3) (Mk (\g->g 3 < 5))) (*2)

-- un (fmap (+3) (Mk (\g->g 3 > 5))) (*2)
-- un (fmap (+3) (Mk (\g->g 3 < 5))) (*2)

-- F a = Int -> a                     Functor
-- F a = (a -> Int)                   Contravariant
-- F a = ((a -> Int) -> Int)          Functor
-- F a = (((a -> Int) -> Int) -> Int) Contravariant

newtype Arr f g a = Arr (f a -> g a)

{- instance (Contravariant f, Functor g) =>
      Functor (Λ a . f a -> g a)
              ^ :: * -> *
       (forall a. f a -> g a) :: *   -}
instance (Contravariant f, Functor g) => Functor (Arr f g) where
  fmap :: (a -> b) -> Arr f g a -> Arr f g b
  fmap h (Arr i) = Arr (fmap h . i . contramap h)
{-
   h           contramap h       map h
a -----> b    f b ----> f a    g a ----> g b

                 ?
            f b ----> g b
             |        ^
  contramap h|        |map h
             v   i    |
            f a ---> g a                -}  

instance (Functor f, Contravariant g) => Contravariant (Arr f g) where
  contramap :: (a -> b) -> Arr f g b -> Arr f g a
  contramap h (Arr i) = Arr (contramap h . i . fmap h)

newtype Id a = Id a
  deriving Functor

newtype Const b a = Const b
  deriving (Functor)

instance Contravariant (Const c) where
  contramap :: (a -> b) -> Const c b -> Const c a
  contramap _ (Const c) = Const c

-- Λ a . (a -> Int)   ==   Arr Id (Const Int)
--                         
-- map1 :: (a -> b) -> Arr Id (Const Int)
-- map1 = contramap

-- System F, polymorphic lambda calculus
-- Girard  , Reynolds

-- Bool = (forall a . a -> a -> a)

type BOOL = forall a . a -> a -> a
true, false :: BOOL
true  = \t f -> t
false = \t f -> f
iteBOOL :: forall a. BOOL -> a -> a -> a
iteBOOL b = b

showBOOL :: BOOL -> String
showBOOL b = b "TRUE" "FALSE"

neg :: BOOL -> BOOL
neg b = iteBOOL b false true

-- showBOOL $ neg true
-- showBOOL $ neg false

-- data ListBOOL = Nil | Cons BOOL (ListBOOL)
type ListBOOL = forall a. a -> (BOOL -> a -> a) -> a
nil :: ListBOOL
nil = \n c -> n
cons :: BOOL -> ListBOOL -> ListBOOL
cons x xs = \n c -> c x (xs n c)
iteListBOOL :: ListBOOL -> a -> (BOOL -> a -> a) -> a
iteListBOOL xs = xs

iteListBOOL' :: ListBOOL -> ListBOOL -> (BOOL -> ListBOOL -> ListBOOL) -> ListBOOL
iteListBOOL' xs = xs

showListBOOL :: ListBOOL -> String
showListBOOL xs = xs "[]" (\b s -> showBOOL b ++ ":" ++ s)

infixr 6 `cons`

-- forall a . a -> a
-- System F-ben is OK:     forall a . (forall b . b -> b-> b) -> a -> a -> a
-- Hindley-Milner t.r. OK: forall a b . (b -> b-> b) -> a -> a -> a

plList :: ListBOOL
plList = true `cons` false `cons` true `cons` nil

id' :: ListBOOL -> ListBOOL
id' xs = iteListBOOL xs nil cons

-- mapnot :: ListBOOL -> ListBOOL
-- mapnot xs = iteListBOOL xs nil (\b bs -> neg b `cons` bs)

-- map' :: (BOOL -> BOOL) -> ListBOOL -> ListBOOL
-- map' f xs = iteListBOOL xs nil (\b bs -> f b `cons` bs)

-- showListBOOL $ id' plList

-- data-val megadott tipus "szep" esetben megfelel egy algebrai elmeletnek
-- (amiben nincsenek egyenlosegek)
{-
Bool :: *, True :: *, False :: *
Tree :: *, Leaf :: Tree, Node :: Tree -> Tree -> Tree
Nat :: *,  Zero :: Nat, Suc :: Nat -> Nat
List a :: *, Nil :: List a, Cons :: a -> List a -> List a
Ring :: * , Null :: Ring, (+) :: Ring -> Ring -> Ring, (-) :: Ring -> Ring , Id :: Ring, (*) :: Ring -> Ring -> Ring
Empty :: *

  +
 / \
 - -
 | |
 0 -
   |
   *
   /\
  1  0

T :: *, Weird :: T -> T

Church-kodolas

type Bool   = forall a . a -> a -> a
type Tree   = forall a . a -> (a -> a-> a) -> a
type Nat    = forall a . a -> (a -> a) -> a
type List a = forall b . b -> (a -> b -> b) -> b
type Ring   = forall a . a -> (a -> a-> a) -> (a -> a) -> a -> (a -> a-> a) -> a
type T      = forall a . (a -> a) -> a
type Empty  = forall a . a
-}

-- a kovetkezo ora 2 perccel rovidebb

