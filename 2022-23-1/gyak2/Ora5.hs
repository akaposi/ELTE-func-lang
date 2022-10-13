{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE DeriveFunctor, InstanceSigs #-}

module Ora5 where
    
import Prelude hiding (Maybe(..), Either(..))
import Control.Applicative

sumM :: (Num a, Monad m) => [m a] -> m a
sumM [] = return 0
sumM (x:xs) = do
    y <- x -- x >>= \y ->
    z <- sumM xs -- sumM xs >>= \z ->
    return (y + z) -- return (y + z)
    -- x >>= \y -> sumM xs >>= \z -> return (y + z)

sumM' [] = return 0
sumM' (x:xs) = x >>= \y -> sumM' xs >>= \z -> return (y + z)

data List a     = Cons a (List a) | Nil deriving (Show, Functor)
data One a      = One a                 deriving (Show, Functor)
data Maybe a    = Just a | Nothing      deriving (Show, Functor)
data Either e a = Left e | Right a      deriving (Show, Functor)

-- Hogyan is nézett ki az fmap?
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- Mi a következő logikai lépés innen?
{-
    Több paraméter:
        liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
        fmap   :: Functor     f => (a      -> b) -> f a        -> f b
    A függvény is be van csomagolva
        (<*>) :: Applicative f => f (a -> b) -> f a -> f b
        fmap  :: Functor     f =>   (a -> b) -> f a -> f b
    Mellékhatás avagy a függvény eredménye lehet mellékhatásos (ez volt a Monad)
        flip (>>=) :: Monad   f => (a -> f b) -> f a -> f b
        fmap       :: Functor f => (a ->   b) -> f a -> f b

    Akit érdekel bónusz (nem tananyag): Monád fordítottja
        extend :: Comonad f => (f a -> b) -> f a -> f b
        fmap   :: Functor f => (a   -> b) -> f a -> f b
-}

{-
:i Applicative
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    (*>) :: f a -> f b -> f b
    (<*) :: f a -> f b -> f a
    {-# MINIMAL pure, ((<*>) | liftA2) #-}
-}

-- Vannak törvények amiket a műveleteknek be kell tartani, de ezek nem érdekesek:
{-
Identitás:
    pure id <*> v ≡ v
Kompozíció:
    pure (.) <*> u <*> v <*> w ≡ u <*> (v <*> w)
Homomorfizmus:
    pure f <*> pure x ≡ pure (f x)
Felcserélhetőség:
    u <*> pure y ≡ pure ($ y) <*> u
-}

-- Minimális szükséges definíció: pure és ((<*>) vagy liftA2)
-- pure ugyanaz mint a return

instance Applicative One where
    pure :: a -> One a
    pure a = One a
    (<*>) :: One (a -> b) -> One a -> One b
    (One f) <*> (One a) = One (f a)
    liftA2 :: (a -> b -> c) -> One a -> One b -> One c
    liftA2 f (One a) (One b) = One (f a b)
    -- gyakorlás kedvéért mindet megírjuk

instance Applicative Maybe where
    pure :: a -> Maybe a
    pure a = Just a
    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    (Just f) <*> (Just a) = Just (f a)
    _ <*> _ = Nothing
    liftA2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
    liftA2 f (Just a) (Just b) = Just (f a b)
    liftA2 _ _ _ = Nothing

instance Applicative (Either e) where -- hibakomponens típusa fix
    pure :: a -> Either e a
    pure a = Right a
    (<*>) :: Either e (a -> b) -> Either e a -> Either e b
    (Left e) <*> _ = Left e
    _ <*> (Left e) = Left e
    (Right f) <*> (Right a) = Right (f a)
    liftA2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
    liftA2 f (Left e) (Left e') = Left e
    liftA2 f (Left e) (Right b) = Left e
    liftA2 f (Right a) (Left e) = Left e
    liftA2 f (Right a) (Right b) = Right (f a b)

(+++) :: List a -> List a -> List a
Nil +++ xs = xs
(Cons x xs) +++ ys = Cons x (xs +++ ys)
infixr 5 +++

instance Applicative List where
    pure :: a -> List a
    pure a = Cons a Nil
    (<*>) :: List (a -> b) -> List a -> List b
    Nil <*> Nil = Nil
    (Cons _ _) <*> Nil = Nil
    Nil <*> (Cons _ _) = Nil
    (Cons f fs) <*> as = fmap f as +++ (fs <*> as)
    liftA2 :: (a -> b -> c) -> List a -> List b -> List c
    liftA2 f Nil _ = Nil
    liftA2 f _ Nil = Nil
    liftA2 f (Cons a as) bs = fmap (f a) bs +++ liftA2 f as bs

-- Applikatív instance nem egyértelmű, nincs rá kódgenerátor.
data List' a = Cons' a (List' a) | Nil' deriving (Functor, Show)

instance Applicative List' where
    -- pure a = Cons a (pure a)
    pure a = Cons' a Nil'
    Nil' <*> _ = Nil'
    _ <*> Nil' = Nil'
    (Cons' f fs) <*> (Cons' a as) = Cons' (f a) (fs <*> as)
    liftA2 f Nil' _ = Nil'
    liftA2 f _ Nil' = Nil'
    liftA2 f (Cons' a as) (Cons' b bs) = Cons' (f a b) (liftA2 f as bs)

-- Mi jár pluszba az applikatívval?
-- (*>) és (<*). Az egyik elemet eldobja, de a mellékhatását alkalmazza

-- egyéb lift műveletek pl liftA3, liftA4 stb

liftA3' :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3' f fa fb fc = liftA2 f fa fb <*> fc
-- f (c -> d)
-- f c
-- f d

liftA4' :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4' f fa fb fc fd = liftA3' f fa fb fc <*> fd

liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' f fa fb = f <$> fa <*> fb

ap :: Applicative f => f (a -> b) -> f a -> f b
ap = liftA2 ($)

-- Applikatív folytatása a Monád
{-
:i Monad
class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    return :: a -> m a
    {-# MINIMAL (>>=) #-}
-}

-- return-t meg se kell írni, alap defníciója return = pure


-- Amit a monád tud de az applikatív nem: Belső érték kihat a külső struktúrára, applikatívnál ilyet nem lehet
-- Monádnak is vannak törvényeik:
{-
Bal identitás:
    return a >>= k ≡ k a
Jobb identitás:
    m >>= returk ≡ m
Asszociativitás:
    m >>= (\x -> k x >>= k) ≡ (m >>= k) >>= h
-}

instance Monad One where
    (>>=) :: One a -> (a -> One b) -> One b
    (One a) >>= f = f a

instance Monad Maybe where
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    (Just x) >>= f = f x
    Nothing  >>= _ = Nothing

instance Monad (Either e) where
    (>>=) :: Either e a -> (a -> Either e b) -> Either e b
    (Right a) >>= f = f a
    (Left e) >>= _ = Left e

instance Monad List where
    (>>=) :: List a -> (a -> List b) -> List b
    Nil >>= _ = Nil
    (Cons a as) >>= f = f a +++ (as >>= f)

-- Egyéb monád műveletek:
-- Kompozíció:
{-
    flip (.) ::            (a ->   b) -> (b -> c)   -> a ->   c
    (>=>)    :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-}

-- Megtalálható a Control.Monad könyvtárban
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g a = f a >>= g

-- Join művelet
join :: Monad m => m (m a) -> m a
join mma = mma >>= \ma -> ma


data Type1 a = Const1 a | Const2 Int deriving (Functor, Show)
data Type2 a = Last a   | ConsT a (Type2 a) deriving (Functor, Show)
data Type3 a = Branch (Type3 a) (Type3 a) | Leaf a deriving (Functor, Show)

instance Applicative Type1 where
    pure a = Const1 a
    (Const1 f) <*> a = fmap f a
    (Const2 a) <*> _ = Const2 a

instance Monad Type1 where
    (Const1 x) >>= f = f x
    (Const2 a) >>= _ = Const2 a

concatT2 (Last a) xs = ConsT a xs
concatT2 (ConsT x xs) ys = ConsT x (concatT2 xs ys)

instance Applicative Type2 where
    pure = Last
    (Last f) <*> as = fmap f as
    (ConsT f fs) <*> as = fmap f as `concatT2` (fs <*> as)

instance Monad Type2 where
    (ConsT a as) >>= f = concatT2 (f a) (as >>= f)
    (Last a) >>= f = f a

instance Applicative Type3 where
    pure = Leaf
    (Leaf f) <*> as = fmap f as
    (Branch f1 f2) <*> as = Branch (f1 <*> as) (f2 <*> as)

instance Monad Type3 where
    (Leaf a) >>= f = f a
    (Branch a1 a2) >>= f = Branch (a1 >>= f) (a2 >>= f)