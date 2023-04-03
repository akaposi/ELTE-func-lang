{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Gy04
import Prelude hiding (Maybe(..), Either(..))

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

instance (Monad m, Monad w) => Monad (Product m w) where
  (>>=) :: (Monad m, Monad w) => Product m w a -> (a -> Product m w b) -> Product m w b
  (>>=) = undefined

instance Monad m => Monad (Kleisli m q) where
  (>>=) :: Monad m => Kleisli m q a -> (a -> Kleisli m q b) -> Kleisli m q b
  (>>=) = undefined

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
