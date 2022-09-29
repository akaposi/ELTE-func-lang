{-# language InstanceSigs, EmptyCase #-}

-- Functor, kind rendszer, példák, Functor törvények, Monad
--------------------------------------------------------------------------------

-- map :: (a -> b) -> [a] -> [b]

-- mapMaybe :: (a -> b) -> Maybe a -> Maybe b   -- "0 vagy 1 elemű lista"
-- mapMaybe f (Just a) = Just (f a)
-- mapMaybe f Nothing  = Nothing

-- data Tree a = Leaf a | Node (Tree a) (Tree a)

-- mapTree :: (a -> b) -> Tree a -> Tree b
-- mapTree f (Leaf a)   = Leaf (f a)
-- mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

-- Functor: osztály, aminek a "map" a metódusa

-- standard definíció:
-- class Functor f where              "f" típusparaméter (1-paraméteres típus!)
--  fmap :: (a -> b) -> f a -> f b

-- instance Functor Maybe where
--   fmap :: (a -> b) -> Maybe a -> Maybe b      -- (f-et "Maybe"-re helyettesítem)
--   fmap = mapMaybe

-- instance Functor [] where
--   fmap :: (a -> b) -> [a] -> [b]   -- (a -> b) -> [] a -> [] b
--   fmap = map

-- a típus, akkor [a] típus
-- [] : lista mint 1-paraméteres típuskonstruktor
--      (prefix formában írt lista típus)

map' :: (a -> b) -> [] a -> [] b  -- [] prefix formában!
map' = undefined

-- mi a helyzet típusokkal vs. paraméteres típusokkal?
-- "kind" rendszer :
--   értéknek típusa van
--   típusnak kind-ja van

-- konkrét típus:
--    Int :: *        * : típusok kind-ja
-- ghci-ben:
-- > :k Int
-- Int :: *

-- 1-paraméteres típus kind-ja:
-- > :k Maybe
-- > Maybe :: * -> *
-- > :k Maybe Int
-- > Maybe Int :: *

-- 2-paraméteres:
-- > :k Either
-- > Either :: * -> * -> *
-- > :k Either Int
-- > Either Int :: * -> *
-- > :k Either Int Bool
-- > Either Int Bool :: *


-- típusszintű típushiba:

-- data Foo = Foo

-- instance Functor Foo where  -- Foo rossz típusú, * -> * kéne, viszont Foo :: *
--   fmap = undefined

-- explicit annotáció típusparaméteren:
class Functor' (f :: * -> *) where
  fmap' :: (a -> b) -> f a -> f b

data Either' (a :: *) (b :: *) = Left' a | Right' b

-- Ettől különböző van-e: * -> * -> ... -> *
-- példa: (* -> *) -> *
--        ((* -> *) -> *) -> *

data FEither f g a = FLeft (f a) | FRight (g a)
  deriving Show

foo :: FEither Maybe [] Int
foo = FLeft (Just 100)

foo2 :: FEither Maybe [] Int
foo2 = FRight [10, 20, 30]

-- > :k FEither
-- > FEither :: (* -> *) -> (* -> *) -> * -> *

instance (Functor f, Functor g) => Functor (FEither f g) where
  fmap :: (a -> b) -> FEither f g a -> FEither f g b
  fmap f (FLeft  fa) = FLeft  (fmap f fa)
  fmap f (FRight ga) = FRight (fmap f ga)
     -- f  :: a -> b
     -- ga :: g a        tudom: Functor g, tehát (fmap f :: g a -> g b)
     -- fmap f ga :: g b
     -- FRight (fmap f ga) :: FEither f g b

-- 1-paraméteres hasonló eset:
-- instance (Eq a, Eq b) => Eq (Either a b)

-- data FMul f g a = FPair (f a) (g a)
-- data Id a = Id a
-- data Const a b = Const a

-- téma: "datatype-generic programming"
--        GHC.Generics modul

--  megírunk egy pretty printert az "elemi" típus-konstruktorokra
--    automatikusan kapunk egy pretty printer-t nagyon sok konkrét típusra

foo3 :: FEither Maybe [] Int
foo3 = fmap (+100) foo2

--------------------------------------------------------------------------------

data Three a = Three a a a

instance Functor Three where
  fmap f (Three x y z) = Three (f x) (f y) (f z)

data Bar a = Bar Int Int a

instance Functor Bar where
  fmap :: (a -> b) -> Bar a -> Bar b
  fmap f (Bar x y a) = Bar x y (f a)

-- több paraméter: az *utolsó* paraméter fölött tudunk fmap-elni
data Baz a b c = Baz Int a b c

instance Functor (Baz x y) where
  fmap :: (a -> b) -> Baz x y a -> Baz x y b
  fmap f (Baz n x y a) = Baz n x y (f a)

  -- írjuk le a fenti típust teljes zárójelezéssel!
  -- fmap :: (a -> b) -> ((((Baz x) y) a) -> (((Baz x) y) b))

  -- curry-zés!
  -- f :: a -> b -> c -> d
  -- f :: a -> (b -> (c -> d))

-- data Either a b = Left a | Right b

-- instance Functor (Either x) where
--   fmap :: (a -> b) -> Either x a -> Either x b
--   fmap f (Left x) = Left x
--   fmap f (Right a) = Right (f a)

-- fmap (+100) (Right 0) == Right 100
-- fmap (+100) (Left 0)  == Left 0

-- szintén standard instance: függvény

-- ha a :: * és b :: * akkor (a -> b :: *)
-- (->) :: * -> * -> *

{-
instance Functor ((->) x) where
  -- fmap :: (a -> b) -> (->) x a -> (->) x b
  fmap :: (a -> b) -> (x -> a) -> (x -> b)
  fmap f g = \x -> f (g x)
  -- fmap = (.)
-}

-- (Bool -> a) ~ (a, a)
-- (x    -> a) ~ (a, a, a, ...., a)     (x-szer)
-- fmap f :: (a, a, a, ...., a) -> (b, b, b, ...., b)

-- általánosan: Functor f: f a "tárol" "a" típusú értékeket

------------------------------------------------------------

-- tegyük föl (f :: * -> *)
--   milyen f-re *nincs* Functor instance?

data EmptyF a

instance Functor EmptyF where
  fmap :: (a -> b) -> EmptyF a -> EmptyF b
  fmap f fa = case fa of -- language EmptyCase opció
    -- üres függvény domain-ből bárhova van triviális függvény ("vacuous")

 -- "a"-n predikátumok ("feltételek") típusa
data Predicate a = Predicate (a -> Bool)

-- NEM működik:
-- instance Functor Predicate where
--   fmap :: (a -> b) -> Predicate a -> Predicate b
--   fmap f (Predicate p) = Predicate (\b -> _)
     -- f :: a -> b
     -- p :: a -> Bool
     -- _ :: b -> Bool

  -- Predicate: *kontravariáns* az "a" paraméterben
  --  Ha Functor f       : pontosan azt jelenti, hogy f kovariáns a paraméterben
  --     Contravariant f : pontosan azt jelenti, hogy f kontravariáns a paraméterben

-- (nem anyag, csak kitenkintés)

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

instance Contravariant Predicate where
  contramap f (Predicate p) = Predicate (p . f)


-- Functor törvények:

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

--   1. ∀ x. fmap id x = x          -- fmap nem változtat struktúrát, csak
--                                  -- a tárolt "a" értékeket módosítja
--
--   2. ∀ f g x. fmap f (fmap g x) = fmap (f . g) x
--
--                                  -- optimalizáció: "fúzió"
--                                  -- elég kevesebbszer bejárni

-- kérdés: következik-e az 1.-ből a 2.?
--   Haskell-ben: igen

-- kérdés: hány teljes definíciója van a köv típusú függvénynek?
-- Haskell-ben pontosan 1 db.
-- f :: a -> a

-- (a -> b) -> a -> b
-- (a -> a) -> a -> a    -- megszámlálhatóan sok definíció
-- f :: (a -> a) -> a -> a
-- f g x = x vagy g x vagy g (g x) vagy g (g (g x))

-- függő típusos nyelvekben (Agda, Idris, Coq)
--   osztály deklarációba a törvényeket is be lehet tenni
--   csak helyes instance-ot lehet definiálni

-- pszeudo-Idris szintaxis:
{-
class Functor f where
  fmap     : (a -> b) -> f a -> f b
  fmapId   : fmap id x = x
  fmapComp : fmap (f . g) x = fmap f (fmap g x)
-}
  -- metódusok: egy függvény + 2 logikai állítás
  -- instance: függvényt definiálni kell, az állítást bizonyítani
