{-# LANGUAGE InstanceSigs, QuantifiedConstraints, StandaloneDeriving, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Gy02 where

import Prelude hiding (Either (..), Maybe (..))

-- Vegyük az alábbi adattípusokat
data Single a = Single a deriving (Eq, Show)
data Tuple a = Tuple a a deriving (Eq, Show)
data Quintuple a = Quintuple a a a a a deriving (Eq, Show)
data List a = Nil | Cons a (List a) deriving (Eq, Show)
data Maybe a = Just a | Nothing deriving (Eq, Show)
data NonEmpty a = Last a | NECons a (NonEmpty a) deriving (Eq, Show)
data NonEmpty2 a = NECons2 a (List a) deriving (Eq, Show)
data Either e a = Left e | Right a deriving (Eq, Show)
data BiTuple e a = BiTuple e a deriving (Eq, Show)
data TriEither e1 e2 a = LeftT e1 | MiddleT e2 | RightT a deriving (Eq, Show)
data BiList a b = ACons a (BiList a b) | BCons b (BiList a b) | ABNill deriving (Eq, Show)

-- Próbáljunk meg olyan függvényeket írni, ami a fent említett típusoknak a típusparaméterét megváltoztatja
-- Pl.: Single a -> Single b vagy List a -> List b
-- Mivel a fenti típusok mind valamilyen szintent tárolnak magukban 'a' típusú elemet ezért szükséges lesz egy (a -> b) függvényre

mapSingle :: (a -> b) -> Single a -> Single b
mapSingle f (Single a) = Single (f a)

mapTuple :: (a -> b) -> Tuple a -> Tuple b
mapTuple f (Tuple a a') = Tuple (f a) (f a')

mapQuintuple :: (a -> b) -> Quintuple a -> Quintuple b
mapQuintuple f (Quintuple a a' a'' a''' a'''') = Quintuple (f a) (f a') (f a'') (f a''') (f a'''') 

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just a) = Just (f a)

mapList :: (a -> b) -> List a -> List b
mapList f Nil = Nil
mapList f (Cons a as) = Cons (f a) (mapList f as)

-- Ezt a mappolhatósági tulajdonságot le tudjuk írni a magasabbrendú polimorfizmus segítségével
-- Emeljük ki a Single, Tuple stb-t a típusból (ezt hívják magasabbrendű polimorfizmusnak, mert a polimorfizmus típusfüggvényekre alkalmazzuk):
{-

        mapSingle    :: (a -> b) -> Single    a -> Single    b
        mapTuple     :: (a -> b) -> Tuple     a -> Tuple     b
        mapQuintuple :: (a -> b) -> Quintuple a -> Quintuple b

        map          :: (a -> b) ->     f     a ->     f     b

-}

-- Ezt fogjuk Functornak hívni
{-
:i Functor
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
        -- Defined in ‘GHC.Base’
-}

-- A Functornak szabálya konyhanyelven: megtartja az adat struktúráját
-- Tehát a konstruktorok sorrendjét, helyét és számát nem változtatja.

instance Functor Single where
  fmap :: (a -> b) -> Single a -> Single b
  fmap = mapSingle

instance Functor Tuple where
  fmap :: (a -> b) -> Tuple a -> Tuple b
  fmap = mapTuple

instance Functor Quintuple where
  fmap :: (a -> b) -> Quintuple a -> Quintuple b
  fmap = mapQuintuple

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap = mapMaybe

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap = mapList

-- Írjuk meg a többi típusra is a Functor instance-ot!

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (Last a) = Last (f a)
  fmap f (NECons a ns) = NECons (f a) (fmap f ns)

instance Functor NonEmpty2 where
  fmap :: (a -> b) -> NonEmpty2 a -> NonEmpty2 b
  fmap f (NECons2 a ns) = NECons2 (f a) (fmap f ns)

-- Ugye a Functor egy Type -> Type kindú kifejezést vár, viszont pl az Either egy Type -> Type -> Type kindú valami, ezért le kell fixálni az első paramétert

instance Functor (Either fixed) where
  fmap :: (a -> b) -> Either fixed a -> Either fixed b
  fmap f (Left fixed) = Left fixed
  fmap f (Right a) = Right (f a)

instance Functor (BiTuple fixed) where
  fmap :: (a -> b) -> BiTuple fixed a -> BiTuple fixed b
  fmap f (BiTuple e a) = BiTuple e (f a)

instance Functor (TriEither fixed1 fixed2) where
  fmap :: (a -> b) -> TriEither fixed1 fixed2 a -> TriEither fixed1 fixed2 b
  fmap f (RightT a) = RightT (f a)
  fmap f (LeftT e1) = LeftT e1
  fmap f (MiddleT e2) = MiddleT e2

instance Functor (BiList fixed) where
  fmap :: (a -> b) -> BiList fixed a -> BiList fixed b
  fmap f ABNill = ABNill
  fmap f (ACons fixed abls) = ACons fixed (fmap f abls)
  fmap f (BCons a abls) = BCons (f a) (fmap f abls)

-- fmap id x == x
-- fmap (f . g) x == fmap f (fmap g (x)) 

-- "nagyon" magasabbrendú polimorfizmus. Ha egy Type -> Type kindú valamit és egy típust adunk meg, csak akkor lesz teljes

-- Speciális Kind annotáció, hogy minden kind-ját megadjuk, ritkán szükséges
-- Én csak az egyszerűség kedvéért kommentbe majd odaírom
type Lift :: (* -> *) -> * -> *
data Lift f a = Lift (f a) deriving (Eq, Show)

-- data BiTuple e a = BiTuple e a deriving (Eq, Show)
-- data Lift    f a = Lift   (f a) deriving (Eq, Show)
-- Van különbség

-- Példa:
listOfInts :: Lift List Int
listOfInts = Lift (Cons 1 (Cons 2 Nil))

maybeABool :: Lift Maybe Bool
maybeABool = Lift Nothing -- pont nincs bool :(

-- Le kell az első paramétert fixálnunk, hogy tudjunk rá Functor-t írni
-- Viszont a fix típusra kell Functor kikötés, hogy az a-t kicserélhessük benne
instance (Functor f) => Functor (Lift f) where
  fmap :: (Functor f) => (a -> b) -> Lift f a -> Lift f b
  fmap g (Lift fa) = Lift (fmap g fa)

-- f az vmi funktor
-- g : a -> b
-- fa : Functor f => f a

-- Pár hasonló típus
data Sum f g a = SumLeft (f a) | SumRight (g a) deriving (Eq, Show)
data Product f g a = Product (f a) (g a) deriving (Eq, Show)
data Compose f g a = Compose (f (g a)) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Sum f g a -> Sum f g b
  fmap h (SumLeft fa) = SumLeft (fmap h fa)
  fmap h (SumRight ga) = SumRight (fmap h ga)

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Product f g a -> Product f g b
  fmap = undefined

-- Nehéz

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Compose f g a -> Compose f g b
  fmap = undefined

-- A függvény funktor?
data Fun a b = Fun (a -> b)

instance Functor (Fun q) where
  -- fmap :: (a -> b) -> (q -> a) -> (q -> b)
  -- Hint: mi a (.) típusa?
  fmap :: (a -> b) -> Fun q a -> Fun q b
  fmap = undefined

-- Egyéb érdekesség:
data UselessF f a = Mk1 (f Int) a
--                       ^ f nincs olyan pozícióban, hogy fmap-olni kéne rajta, tehát a Functor f megkötés felesleges

-- Gyakorlás:

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show)
data RoseTree a = RoseLeaf a | RoseNode [RoseTree a] deriving (Eq, Show)
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a) deriving (Eq, Show)
data SkipList a = Skip (SkipList a) | SCons a (SkipList a) | SNill deriving (Eq, Show)
data CrazyType a = C1 a a | C2 a Int | C3 (CrazyType a) deriving (Eq, Show)
data Either3 a b c = Left3 a | Middle3 b | Right3 c deriving (Eq, Show)
data Triplet a b c = Triplet a b c deriving (Eq, Show)
data SplitTree a b = SplitTree (Tree a) a b (Tree b) deriving (Eq, Show)
data TriCompose f g h a = TriCompose (f (g (h a))) deriving (Eq, Show)
data Free f a = Pure a | Free (f (Free f a))
type Fix :: (* -> *) -> * -> *
data Fix f a = Fix (f (Fix f a))
data Join a b = Join (a -> a -> b)
data CrazyType2 a b = SingleA a | SingleB b | Translate (a -> b)

--- Írjunk rájuk Functor instance-ot!

-- Dont mind this

deriving instance (Eq a, forall q. (Eq q) => Eq (f q)) => Eq (Free f a)

deriving instance (Show a, forall q. (Show q) => Show (f q)) => Show (Free f a)

deriving instance (Eq a, forall q. (Eq q) => Eq (f q)) => Eq (Fix f a)

deriving instance (Show a, forall q. (Show q) => Show (f q)) => Show (Fix f a)

deriving instance (Show a, Show (f Int)) => Show (UselessF f a)

deriving instance (Eq a, Eq (f Int)) => Eq (UselessF f a)