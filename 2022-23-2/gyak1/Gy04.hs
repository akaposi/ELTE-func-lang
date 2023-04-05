{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor, StandaloneDeriving, QuantifiedConstraints #-}

module Gy04 where

import Control.Applicative hiding (ZipList(..))
import Prelude hiding (Maybe(..), Either(..))

-- Vegyük az alábbi típust!
data Gofri f a = Mix a (f (Gofri f a))

-- Írj rá Functor instance-ot! (2 pont)
-- Deriving semmilyen formában nem használható!
instance Functor f => Functor (Gofri f) where
  fmap :: Functor f => (a -> b) -> Gofri f a -> Gofri f b
  fmap f (Mix a b) = Mix (f a) (fmap (fmap f) b)


-- Tesztek:
{-
fmap (+1) (Mix 10 Nothing) == Mix 11 Nothing
fmap length (Mix "alma" $ Just $ Mix "banán" Nothing) == Mix 4 (Just $ Mix 5 Nothing)
fmap length (Mix [1..10] $ [Mix [2,3] [], Mix [4,5,6] [Mix [1] [], Mix [2] []]]) == Mix 10 [Mix 2 [], Mix 3 [Mix 1 [], Mix 1 []]]
-}


-- Ezeket ignoráld, a kiírás és egyenlőség kedvéért vannak itt

deriving instance (Show a, forall a. Show a => Show (f a)) => Show (Gofri f a)
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (Gofri f a)

-- Ami előző órán kimaradt:

-- Az fmap-nak van egy operátor verziója is:
-- <$>
-- tehát f <$> xs == fmap f xs

-- A Functor instance írási is algoritmizálható, van rá DeriveFunctor nyelvi kiegészítő.

data List a = Nil | Cons a (List a) deriving (Eq, Show, Functor)
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show, Functor)
data Maybe a = Just a | Nothing deriving (Eq, Show, Functor)
data Either e a = Left e | Right a deriving (Eq, Show, Functor)
data Tuple e a = Tuple e a deriving (Eq, Show, Functor)
data Id a = Id a deriving (Eq, Show, Functor)
data Wrap f a = Wrap (f a) deriving (Eq, Show, Functor)
data Fun a b = Fun (a -> b) deriving Functor
data Compose f g a = Compose (f (g a)) deriving Functor
data NonEmpty a = Last a | NECons a (NonEmpty a) deriving (Eq, Show, Functor)


-- A Functor az szép és jó, viszont nem tud például több f-féle "konténert" kombinálni
-- Ilyen függvény pl nem létezik:
fmap2 :: Functor f => (a -> b -> c) -> f a -> f b -> f c
fmap2 = error "Does not exist"

-- Ahhoz, hogy ilyet le tudjunk írni, erősebb megkötés kell => Applikatív
-- Ha a Functor volt az 1-és-sok érték (1 függvény és sok érték) kombinációja
-- az applikatív a sok-és-sok érték (sok függvény és sok érték) kombinációja

-- Applikatív törvények (nem kell tudni, de jó instance guidelineok):
{-
        Identitás:         pure (\x -> x) <*> v = v
        Kompozíció:        pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
        Homomorfizmus:     pure f <*> pure x = pure (f x)
        Felcserélhetőség:  u <*> pure y = pure ($ y) <*> u
-}

-- Egy jó guideline:
-- Pl <*> esetén ha az f (a -> b) Q darab függvényt tartalmaz és az f a X darab a-t, akkor az f b Q * X darab b-t fog tartalmazni
-- Ez nem mindig igaz, de átlalában

(+++) :: List a -> List a -> List a
Nil +++ as = as
(Cons a as) +++ bs = Cons a (as +++ bs)

infixr 5 +++

instance Applicative List where

  pure :: a -> List a -- Tiszta értéket berak applikatív környezetbe
  pure a = Cons a Nil

  liftA2 :: (a -> b -> c) -> List a -> List b -> List c -- Az előbb említett fmap2
  liftA2 f Nil _ = Nil
  liftA2 f _ Nil = Nil
  liftA2 f (Cons a as) bs = fmap (f a) bs +++ liftA2 f as bs

  (<*>) :: List (a -> b) -> List a -> List b -- Alternatív verziója a liftA2-nek, csak az egyiket kell megírni (liftA2 f a b = f <$> a <*> b, (<*>) = liftA2 ($))
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) as = fmap f as +++ (fs <*> as)

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf
  liftA2 :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
  liftA2 f (Leaf a) (Leaf b) = Leaf (f a b)
  liftA2 f (Node l r) (Leaf b) = Node (fmap (flip f b) l) (fmap (flip f b) r)
  liftA2 f (Leaf a) (Node l r) = Node (fmap (f a) l) (fmap (f a) r)
  liftA2 f (Node l r) (Node l' r') = Node (Node (liftA2 f l l') (liftA2 f r r')) (Node (liftA2 f l r') (liftA2 f r l'))

instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = Just
  liftA2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
  liftA2 f Nothing _ = Nothing
  liftA2 f _ Nothing = Nothing
  liftA2 f (Just x) (Just y) = Just (f x y)
  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (<*>) Nothing _ = Nothing
  (<*>) _ Nothing = Nothing
  (<*>) (Just f) (Just x) = Just (f x)

-- Fixált paraméteres eseteknél a fix paraméter típusú elemeket nem tudjuk "kombinálni", ezért általában az elsőt kiválasztjuk
instance Applicative (Either e) where
  pure :: a -> Either e a
  pure = Right
  (<*>) :: Either e (a -> b) -> Either e a -> Either e b
  (<*>) (Left e) _ = Left e
  (<*>) (Right f) x = fmap f x
  liftA2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
  liftA2 f (Left e) _ = Left e
  liftA2 f _ (Left e) = Left e
  liftA2 f (Right x) (Right y) = Right (f x y)

instance Applicative Id where
  pure :: a -> Id a
  pure = Id
  (<*>) :: Id (a -> b) -> Id a -> Id b
  (<*>) (Id f) (Id a) = Id (f a)
  liftA2 :: (a -> b -> c) -> Id a -> Id b -> Id c
  liftA2 f (Id a) (Id b) = Id (f a b)

instance Applicative f => Applicative (Wrap f) where
  pure :: Applicative f => a -> Wrap f a
  pure = Wrap . pure
  liftA2 :: Applicative f => (a -> b -> c) -> Wrap f a -> Wrap f b -> Wrap f c
  liftA2 f (Wrap a) (Wrap b) = Wrap (liftA2 f a b)
  (<*>) :: Applicative f => Wrap f (a -> b) -> Wrap f a -> Wrap f b
  (<*>) (Wrap ff) (Wrap fa) = Wrap (ff <*> fa)

instance Applicative (Fun q) where
  pure :: a -> Fun q a
  pure a = Fun (const a)
  liftA2 :: (a -> b -> c) -> Fun q a -> Fun q b -> Fun q c
  liftA2 f (Fun qa) (Fun qb) = Fun $ \q -> f (qa q) (qb q)


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: (Applicative f, Applicative g) => a -> Compose f g a
  pure = Compose . pure . pure
  liftA2 :: (Applicative f, Applicative g) => (a -> b -> c) -> Compose f g a -> Compose f g b -> Compose f g c
  liftA2 f (Compose fga) (Compose fgb) = Compose (liftA2 (liftA2 f) fga fgb)

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure = Last
  liftA2 :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
  liftA2 f (Last a) xs = fmap (f a) xs
  liftA2 f (NECons a as) xs = fmap (f a) xs +++ liftA2 f as xs
    where
      Last a +++ xs = NECons a xs
      NECons a as +++ xs = NECons a (as +++ xs)


-- Egy típusnak több szabályos applikatív instance-a is lehet
data ZipList a = ZipList [a] deriving (Eq, Show, Functor)

instance Applicative ZipList where
  pure :: a -> ZipList a
  pure = ZipList . repeat
  liftA2 :: (a -> b -> c) -> ZipList a -> ZipList b -> ZipList c
  liftA2 f (ZipList a) (ZipList b) = ZipList (zipWith f a b)

-- Az Applikatívot "statikus mellékhatás"-nak is szokás hívni, mivel az eredmény struktúrája eldönthető a paraméterek megvizsgálásával
-- Ez annyi megkötést ad nekünk, hogy a konténer struktúrája nem függ valami függvény eredményétől!
-- Jövő heti +/- egy applikatív instance írása

-- Gyakorlás: Írj az alábbi típusokra Applicative instance-ot!

data Product f g a = Product (f a) (g a) deriving (Eq, Show, Functor)
-- data Sum f g a = SumL (f a) | SumR (g a) deriving (Eq, Show, Functor) ambiguous
data Kleisli f a b = Kleisli (a -> f b) deriving Functor
-- data BiRecurse a = LeftRec a (BiRecurse a) | RightRec a (BiRecurse a) | BNill deriving (Eq, Show, Functor) ambiguous

instance (Applicative f, Applicative g) => Applicative (Product f g) where
  pure :: (Applicative f, Applicative g) => a -> Product f g a
  pure x = Product (pure x) (pure x)
  liftA2 :: (Applicative f, Applicative g) => (a -> b -> c) -> Product f g a -> Product f g b -> Product f g c
  liftA2 f (Product ax ay) (Product bx by) = Product (liftA2 f ax bx) (liftA2 f ay by)

instance Applicative f => Applicative (Kleisli f q) where
  pure :: Applicative f => a -> Kleisli f q a
  pure a = Kleisli $ const (pure a)
  (<*>) :: Applicative f => Kleisli f q (a -> b) -> Kleisli f q a -> Kleisli f q b
  (Kleisli f) <*> (Kleisli a) = Kleisli $ \q -> f q <*> a q
