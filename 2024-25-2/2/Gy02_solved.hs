{-# LANGUAGE InstanceSigs, QuantifiedConstraints, StandaloneDeriving, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Gy02_pre where

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
mapQuintuple f (Quintuple a b c d e) = (Quintuple (f a) (f b) (f c) (f d) (f e))

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f (Just a) = Just $ f a
mapMaybe f Nothing = Nothing

mapList :: (a -> b) -> List a -> List b
mapList f Nil = Nil
mapList f (Cons x xs) = Cons (f x) (mapList f xs)


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
  fmap f (Last a) = Last $ f a
  fmap f (NECons a b) = NECons (f a) (fmap f b)

instance Functor NonEmpty2 where
  fmap :: (a -> b) -> NonEmpty2 a -> NonEmpty2 b
  fmap f (NECons2 a l) = NECons2 (f a) (fmap f l)

-- Ugye a Functor egy Type -> Type kindú kifejezést vár, viszont pl az Either egy Type -> Type -> Type kindú valami, ezért le kell fixálni az első paramétert

instance Functor (Either fixed) where
  fmap :: (a -> b) -> Either fixed a -> Either fixed b
  fmap _ (Left fi) = Left fi
  fmap f (Right a) = Right $ f a

instance Functor (BiTuple fixed) where
  fmap :: (a -> b) -> BiTuple fixed a -> BiTuple fixed b
  fmap f (BiTuple e a) = BiTuple e $ f a

instance Functor (TriEither fixed1 fixed2) where
  fmap :: (a -> b) -> TriEither fixed1 fixed2 a -> TriEither fixed1 fixed2 b
  fmap f (LeftT   b) = LeftT b
  fmap f (MiddleT b) = MiddleT b
  fmap f (RightT  a) = RightT $ f a

instance Functor (BiList fixed) where
  fmap :: (a -> b) -> BiList fixed a -> BiList fixed b
  fmap f  ABNill      = ABNill
  fmap f (ACons a xs) = ACons a (f <$> xs)
  fmap f (BCons b xs) = BCons (f b) (f <$> xs)

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
  fmap g (Lift x) = Lift (fmap g x)

-- f az vmi funktor
-- g : a -> b
-- fa : Functor f => f a

-- Pár hasonló típus
data Sum f g a = SumLeft (f a) | SumRight (g a) deriving (Eq, Show)
data Product f g a = Product (f a) (g a) deriving (Eq, Show)
data Compose f g a = Compose (f (g a)) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Sum f g a -> Sum f g b
  fmap fun (SumLeft  fa) = SumLeft  $ fun <$> fa
  fmap fun (SumRight ga) = SumRight $ fun <$> ga


instance (Functor f, Functor g) => Functor (Product f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Product f g a -> Product f g b
  fmap fun (Product fa ga) = Product (fun <$> fa) (fun <$> ga)

-- Nehéz

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Compose f g a -> Compose f g b
  fmap fun (Compose fga) = Compose $ fmap (\ga -> fmap fun ga) fga

-- A függvény funktor?
data Fun a b = Fun (a -> b)
-- fmap'  :: (a -> b) -> Fun a q -> Fun b q
-- fmap'' :: (a -> b) -> Fun q a -> Fun q b

-- fmap'  :: (a -> b) -> (a -> q) -> b -> q
-- fmap'' :: (a -> b) -> (q -> a) -> q -> b

{-
  fmap'
  indulok/van : b
  kell : q

  a -> b
  |
  V
  q

-----------

fmap''
  indulok/van : q
  kell : b

  a -> b
  ^
  |
  q
-}

instance Functor (Fun q) where
  -- Hint: mi a (.) típusa?
  -- fmap :: (a -> b) -> (q -> a) -> (q -> b)
  fmap :: (a -> b) -> Fun q a -> Fun q b
  fmap f (Fun g) = Fun (f . g)

-- Egyéb érdekesség:
data UselessF f a = Mk1 (f Int) a
--                       ^ f nincs olyan pozícióban, hogy fmap-olni kéne rajta, tehát a Functor f megkötés felesleges

-- Gyakorlás:

--- Írjunk rájuk Functor instance-ot!

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor (Tree) where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Leaf = Leaf
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

data RoseTree a = RoseLeaf a | RoseNode [RoseTree a] deriving (Eq, Show)

instance Functor (RoseTree) where
  fmap :: (a -> b) -> RoseTree a -> RoseTree b
  fmap f (RoseLeaf a) = RoseLeaf $ f a
  -- A RoseTree-ket tartalmazó listákat úgy lehet mappolni hogy minden elemre egy map-ot nyomunk
  -- Ugyanaza mint amikor egy [[a]]-ra akarunk mappolni 
  fmap f (RoseNode ts) = RoseNode $ fmap (fmap f) ts

data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a) deriving (Eq, Show)

instance Functor (Tree2) where
  fmap :: (a -> b) -> Tree2 a -> Tree2 b
  fmap f (Leaf2 a) = Leaf2 $ f a
  fmap f (Node2 l r) = Node2 (fmap f l) (fmap f r)

data SkipList a = Skip (SkipList a) | SCons a (SkipList a) | SNill deriving (Eq, Show)

instance Functor (SkipList) where
  fmap :: (a -> b) -> SkipList a -> SkipList b
  fmap f (Skip as) = Skip $ fmap f as
  fmap f (SCons a as) = SCons (f a) (fmap f as)
  fmap f SNill = SNill

data CrazyType a = C1 a a | C2 a Int | C3 (CrazyType a) deriving (Eq, Show)

instance Functor (CrazyType) where
  fmap :: (a -> b) -> CrazyType a -> CrazyType b
  fmap f (C1 a a') = C1 (f a) (f a')
  fmap f (C2 a i)  = C2 (f a) i
  fmap f (C3 ts)   = C3 (fmap f ts)

data Either3 a b c = Left3 a | Middle3 b | Right3 c deriving (Eq, Show)

-- Csak a c-ben funcktor így csak azt tudjuk lecserélni
instance Functor (Either3 fixed fixed') where
  fmap :: (a -> b) -> Either3 fixed fixed' a -> Either3 fixed fixed' b
  fmap f (Left3 fx) = (Left3 fx)
  fmap f (Middle3 fx) = (Middle3 fx)
  fmap f (Right3 a) = Right3 $ f a

data Triplet a b c = Triplet a b c deriving (Eq, Show)

-- Csak a c-ben funcktor így csak azt tudjuk lecserélni
instance Functor (Triplet fixed fixed') where
  fmap :: (a -> b) -> Triplet fixed fixed' a -> Triplet fixed fixed' b
  fmap f (Triplet fx fx' c) = Triplet fx fx' (f c)

data SplitTree a b = SplitTree (Tree a) a b (Tree b) deriving (Eq, Show)

-- Csak a b-ben funcktor így csak azt tudjuk lecserélni
instance Functor (SplitTree fixed) where
  fmap :: (a -> b) -> SplitTree fixed a -> SplitTree fixed b
  fmap f (SplitTree fxs fx b r) = SplitTree fxs fx (f b) (fmap f r)

data TriCompose f g h a = TriCompose (f (g (h a))) deriving (Eq, Show)

-- Meg kell kötni hogy f g h is funktor mert csak így férünk hozzá a benti a-hoz
-- MIvel 3 funcktor mögött van az a így 3 fmap kell
instance (Functor f, Functor g, Functor h) => Functor (TriCompose f g h) where
  fmap :: (a -> b) -> TriCompose f g h a -> TriCompose f g h b
  fmap f (TriCompose fgha) = TriCompose (fmap (fmap (fmap f)) fgha)

data Free f a = Pure a | Free (f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap :: (a -> b) -> Free f a -> Free f b
  fmap f (Pure a) = Pure $ f a
  fmap f (Free fFreefa) = Free $ fmap (fmap f) fFreefa
  -- Free $ _ --ban _ :: f (Free f b)
  -- Free $ fmap _ fFreefa --ban _ :: Free f a -> Free f b, de ez pont az fmap vége Free-re

type Fix :: (* -> *) -> * -> *
data Fix f a = Fix (f (Fix f a))

instance (Functor f) => Functor (Fix f) where
  fmap :: (a -> b) -> Fix f a -> Fix f b
  fmap f (Fix fFreefa) = Fix $ fmap (fmap f) fFreefa

data Join a b = Join (a -> a -> b)

instance Functor (Join fixed) where
  fmap :: (a -> b) -> Join fixed a -> Join fixed b
  fmap g (Join ffa) = Join $ \f f' -> g (ffa f f')

data CrazyType2 a b = SingleA a | SingleB b | Translate (a -> b)


instance Functor (CrazyType2 fixed) where
  fmap :: (a -> b) -> CrazyType2 fixed a -> CrazyType2 fixed b
  fmap g (SingleA f)    = SingleA f
  fmap g (SingleB a)    = SingleB $ g a
  fmap g (Translate fa) = Translate $ \a -> g (fa a)

-- Dont mind this

deriving instance (Eq a, forall q. (Eq q) => Eq (f q)) => Eq (Free f a)

deriving instance (Show a, forall q. (Show q) => Show (f q)) => Show (Free f a)

deriving instance (Eq a, forall q. (Eq q) => Eq (f q)) => Eq (Fix f a)

deriving instance (Show a, forall q. (Show q) => Show (f q)) => Show (Fix f a)

deriving instance (Show a, Show (f Int)) => Show (UselessF f a)

deriving instance (Eq a, Eq (f Int)) => Eq (UselessF f a)
