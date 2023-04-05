
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Gy04
import Prelude hiding (Maybe(..), Either(..))
import Control.Applicative


--------------------------------------------------------------------------------

-- Vegyük az alábbi típust
data EitherWithF f a b = FLeft (f a) | FRight (f b) deriving (Eq, Show, Functor)

-- Írj rá Applicative instance-ot! (2 pont)
-- (pure = 0.5 pont, liftA2/<*> = 1.5 pont)

-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b

instance Applicative f => Applicative (EitherWithF f c) where
  pure :: a -> EitherWithF f c a
  pure a = FRight (pure a)

  (<*>) :: EitherWithF f c (a -> b) -> EitherWithF f c a -> EitherWithF f c b
  (<*>) (FLeft fc)  _           = FLeft fc
  (<*>) (FRight ff) (FLeft fc)  = FLeft fc
  (<*>) (FRight ff) (FRight fa) = FRight (ff <*> fa)

--------------------------------------------------------------------------------

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) (Tree' a) deriving (Eq, Show)

-- Definiáld a függvényeket >>= használatával!
-- Ha bármelyik Maybe-t adó függvény Nothing-ot ad,
-- akkor a végeredmény is legyen Nothing.

-- Ugyanúgy viselkedik, mint a "filter" függvény, viszont
-- ha a feltétel függvény bárhol Nothing-ot ad, akkor
-- a végeredmény legyen Nothing, egyébként pedig
-- legyen a végeredmény Just-ban szűrt lista.
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f []     = Just []    -- filter f [] == []
filterMaybe f (a:as) = case f a of
  Nothing -> Nothing
  Just b  -> case b of
    True  -> case filterMaybe f as of
               Nothing  -> Nothing
               Just as' -> Just (a : as')
    False -> filterMaybe f as

-- filter f (a:as) = case f a of
--   True  -> a : filter f as
--   False -> filter f as

-- hasonlkóképpen, ha bárhol Nothing-ot ad a függvény, Nothing eredmény
mapMaybeTree' :: (a -> Maybe b) -> Tree' a -> Maybe (Tree' b)
mapMaybeTree' f (Leaf' a) = case f a of
  Nothing -> Nothing
  Just b  -> Just (Leaf' b)
mapMaybeTree' f (Node' t1 t2 t3) = case mapMaybeTree' f t1 of
  Nothing -> Nothing
  Just t1' -> case mapMaybeTree' f t2 of
    Nothing -> Nothing
    Just t2' -> case mapMaybeTree' f t3 of
      Nothing -> Nothing
      Just t3' -> Just (Node' t1' t2' t3')

-- fmap f (Leaf' a) = Leaf' (f a)
-- fmap f (Node' t1 t2 t3) = Node' (fmap f t1) (fmap f t2) (fmap f t3)

-- Ugyanaz, viszont Just és Nothing nélkül, csak Applicative
-- metódusokkal
mapMaybeTree'' :: (a -> Maybe b) -> Tree' a -> Maybe (Tree' b)
mapMaybeTree'' f (Leaf' a) =
  Leaf' <$> f a
                        -- f a :: Maybe b
                        -- Leaf' :: b -> Tree' b
                        -- Leaf' <$> f a :: Maybe (Tree' b)    OK
mapMaybeTree'' f (Node' t1 t2 t3) =
  Node' <$> mapMaybeTree'' f t1 <*> mapMaybeTree'' f t2 <*> mapMaybeTree'' f t3

-- ((Node' <$> mapMaybeTree'' f t1) <*> mapMaybeTree'' f t2) <*> mapMaybeTree'' f t3

-- Applicative fő felhasználása:
--    Functor f     : 1 paraméteres függvénnyel map-elünk
--    Applicative f : akárhány paraméteres függvénnyel map-elünk

-- pure + (<*>) ből akárhány paraméter map-elés definiálható

-- pure   :: a -> f a
-- fmap   :: (a -> b) -> f a -> f b

-- általánosan: N aritású függvénnyel map-elés:

-- f :: a -> b -> ... -> z
-- f <$> _ <*> _ <*> _ ... <*> _

-- 3-as aritás:

-- f :: a -> b -> c -> d
-- x1 :: f a
-- x2 :: f b
-- x3 :: f c

-- f <$> x1 <*> x2 <*> x3 :: f d


-- ugyanaz, viszont Nothing és Just használata nélkül
filterMaybe' :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe' f []     = pure []
filterMaybe' f (a:as) = (\b as -> if b then a:as else as) <$> f a <*> filterMaybe' f as

 -- Két paraméteres függvénnyel kombináljuk a Maybe értékeket.
 --   (\b as -> if b then a:as else as)

 -- <$>, pure, <*>

-- filterMaybe f (a:as) = case f a of
--   Nothing -> Nothing
--   Just b  -> case b of
--     True  -> case filterMaybe f as of
--                Nothing  -> Nothing
--                Just as' -> Just (a : as')
--     False -> filterMaybe f as

-- Mechanikus megoldás: vesszük a Maybe-mentes definíciót, mindenhol
-- mintát illesztünk a Maybe értékekre.

-- hasonlóképpen
zipWithMaybe' :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe' f (a:as) (b:bs) = (:) <$> f a b <*> zipWithMaybe' f as bs
zipWithMaybe' f _      _      = pure []


-- emlékeztető:
--   zipWith f (a:as) (b:bs) = f a b : zipWith f as

--------------------------------------------------------------------------------

-- Előző óra: Applikatív
-- Képzeljünk el egy "f a" applikatívot mint egy f számítás amelynek eredménye "a" típusú
-- Az applikatív műveletek "statikusak", mert egy számítás nem függ egy másik számítás eredményétől

-- liftA2 :: (a -> b -> c) -> f a -> f b -> f c | Ha egy a-t és egy b-t ki tudunk számolni, akkor c-t is
-- (<*>)  :: f (a -> b) -> f a -> f b           | Ha (a -> b)-t és a-t ki tudunk számolni, akkor b-t is
-- Itt a "ki tudunk számolni" jelentheti azt hogy a számítás sikertelen
-- Például Maybe esetében, ami nem biztos hogy olyan értéket tartalmaz

-- Monád: "dinamikus" számítás

{-
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  {-# MINIMAL (>>=) #-}
-}

-- (>>=) neve "bind"

-- Hasonló a bash féle pipeoláshoz
-- echo "hello world" | valami <=> pure "hello world" >>= valami

-- Írjunk Monád instance-okat
-- Típusok Gy04.hs-ben

-- Funktornál & Applikatívnál szabály volt, hogy ha egy konstruktor az = bal oldalán megjelenik
-- akkor a jobb oldalán is meg fog.

-- Ez Monádnál NEM igaz, az eredmény 100%-ban a fv-től függ

instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b -- hint: concatMap
  (>>=) = undefined

instance Monad Tree where
  (>>=) :: Tree a -> (a -> Tree b) -> Tree b
  (>>=) = undefined

-- Monádnak is vannak törvényei
{-
        Bal Identitás:   pure a >>= k = k a
        Jobb Identitás:  m >>= pure = m
        Asszociativitás: m >>= (\x -> k x >>= h) = (m >>= k) >>= h
-}

-- Innentől gyakorlás

instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>=) = undefined

instance Monad Id where
  (>>=) :: Id a -> (a -> Id b) -> Id b
  (>>=) = undefined

instance Monad (Either e) where
  (>>=) :: Either e a -> (a -> Either e b) -> Either e b
  (>>=) = undefined

instance Monad m => Monad (Wrap m) where
  (>>=) :: Monad m => Wrap m a -> (a -> Wrap m b) -> Wrap m b
  (>>=) = undefined

instance Monad (Fun q) where
  -- (>>=) :: (q -> a) -> (a -> (q -> b)) -> (q -> b)
  -- hint: mi volt a <*> típusa fv-ek esetén?
  (>>=) :: Fun q a -> (a -> Fun q b) -> Fun q b
  (>>=) = undefined

instance Monad NonEmpty where
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (>>=) = undefined

-- instance (Monad m, Monad w) => Monad (Product m w) where
--   (>>=) :: (Monad m, Monad w) => Product m w a -> (a -> Product m w b) -> Product m w b
--   (>>=) = undefined

-- instance Monad m => Monad (Kleisli m q) where
--   (>>=) :: Monad m => Kleisli m q a -> (a -> Kleisli m q b) -> Kleisli m q b
--   (>>=) = undefined

-- Milyen műveletek járnak a Monáddal/Applikatívval?

-- Konstans Bind / Konstans Ap
-- (>>) :: Monad m => m a -> m b -> m b
-- vagy
-- (*>) :: Applicative f => f a -> f b -> f b
-- (<*) :: Applicative f => f a -> f b -> f a

-- Ha csak a "számítás" fontos, az eredmény nem

-- Mondának van "monadikus kompozíciója"
(>=>) :: Monad m => (a -> m b) -> (c -> m a) -> c -> m b
(>=>) = undefined

-- liftAX függvények >>= vagy <*> segístégével
liftA3' :: Monad m {- próbáld meg csak Applicative kikötéssel! -}
  => (a -> b -> c -> d)
  -> m a
  -> m b
  -> m c
  -> m d
liftA3' = undefined

-- +/- Monad instance írása lesz

-- Plusz feladatok
-- Írj ezekre Applikatív és Monád instance-okat!

data Free f a = Pure a | Free (f (Free f a)) deriving Functor
data Tuple a b = Tuple a b deriving Functor
data Join a b = Join (a -> a -> b) deriving Functor
