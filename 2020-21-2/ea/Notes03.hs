{-# language KindSignatures #-}
{-# language InstanceSigs #-}

-- kind-ok, InstanceSigs, KindSignatures, higher kind, Functor (Either a), Functor ((->) a)
-- Functor példák, ellenpéldák
-- Esetleg: elemi funktorok
-- Monad: Maybe, osztály, IO, do, generikus függvények (mapM, filterM, etc)

--------------------------------------------------------------------------------

-- Functor osztály

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--   törvények:
--     fmap id fa = fa
--     fmap f (fmap g fa) = fmap (f . g) fa


-- class Functor' f where
--   fmap' :: (a -> b) -> f -> f a     -- használtam f-et mint típus + paraméteres típus
--                                     -- kapok: típushibát (típusszintű)

-- típusok típusrendszere: N-paraméteres típusok vizsgálata
--    "kind" rendszer

-- GHC terminológia: érték :: típus :: típus
-- üzenet: "kind" a típus típusa


-- kind-ok:
--    *         : konkrét típusok típusa, pl: Int :: *, Bool :: *,
--  * -> *      : 1-paraméteres típus ("típuskonstruktor"), pl: Maybe :: * -> *, [] :: * -> *
--  * -> * -> * : 2-paraméteres, pl: Either :: * -> * -> *, (->) :: * -> * -> *

-- ghci-ben: :k <típus kifejezés>

-- parciális applikáció típus konstruktorokra:
--   :k Either
--   Either :: * -> * -> *
--   :k Either Int
--   Either Int :: * -> *

-- {-# language KindSignatures #-}

class Functor' (f :: * -> *) where
  fmap' :: (a -> b) -> f a -> f b

class Show' (a :: *) where
  show' :: a -> String

-- típus aminek nem * a kind-ja : "higher-kinded" type

-- magasabbrendű típuskonstruktor?

-- (* -> *) -> * -> *

-- Funktorok kombinálása (szimpla * esetén: (a, b), Either a b, (a -> b), etc.)

-- Funktorok összege:

data FSum (f :: * -> *) (g :: * -> *) (a :: *) = FLeft (f a) | FRight (g a)
  deriving (Show)

-- FSum     :: (* -> *) -> (* -> *) -> * -> *
-- FSum f g :: * -> *      (kind-ja OK Functor-hoz)
instance (Functor f, Functor g) => Functor (FSum f g) where
  fmap f (FLeft fa)  = FLeft  (fmap f fa)
  fmap f (FRight ga) = FRight (fmap f ga)

data FProd f g a = FPair (f a) (g a)
  deriving (Show)
  -- házi feladat: instance (Functor f, Functor g) => Functor (FProd f g)

fs1 :: FSum Maybe Maybe Int
fs1 = FLeft (Just 10)

fp1 :: FProd Maybe [] Bool
fp1 = FPair (Just False) [True, True]

-- Funktor példák:
--   Either   :: * -> * -> *
--   Either a :: * -> *

-- utolsó típusparaméter fölött map-elünk!       {-# language InstanceSigs #-}
-- instance Functor (Either c) where
--   fmap :: (a -> b) -> Either c a -> Either c b
--   fmap f (Left c)  = Left c
--   fmap f (Right a) = Right (f a)

data Foo a b c d = Foo a a b c d Int

instance Functor (Foo c d e) where
  fmap :: (a -> b) -> Foo c d e a -> Foo c d e b
  fmap f (Foo a1 a2 b c d n) = Foo a1 a2 b c (f d) n

-- instance Functor ((->) c) where
--   fmap :: (a -> b) -> (c -> a) -> (c -> b)
--   fmap = (.)

data Foo2 a = Foo2 (Maybe (Maybe (Maybe [a])))  -- egymásba ágyazott funktorok szintén funktor

instance Functor Foo2 where
  fmap f (Foo2 mmmas) = Foo2 (fmap (fmap (fmap (fmap f))) mmmas)


-- (potenciálisan) végtelenül elágazó fa:
data Tree a = Leaf a | Node (Int -> Tree a)

t1 :: Tree Bool
t1 = Leaf False

t2 :: Tree Bool
t2 = Node $ \_ -> Leaf False

t3 :: Tree Int
t3 = Node $ \i -> Leaf i

   --                   Node
   --   Leaf 0   Leaf 1   Leaf 2   Leaf 3   ......

t4 :: Tree Int
t4 = Node $ \i -> Node $ \j -> Leaf (i + j)


   --            Node
   --     0               1
   --   Node             Node
   --     0               0
   --   Leaf (0 + 0)    Leaf (0 + 1)

instance Functor Tree where
  fmap f (Leaf a)  = Leaf (f a)
  fmap f (Node ts) = Node (fmap (fmap f) ts)
  -- fmap f (Node ts) = Node (fmap f . ts)
  -- fmap f (Node ts) = Node (\i -> fmap f (ts i))    -- i-edik részfát map-eljük rekurzívan


--------------------------------------------------------------------------------

-- Kérdés: van olyan (f :: * -> *), hogy nincsen Functor instance?

data NotF a

-- f :: NotF a -> b
-- f = undefined             --

instance Functor NotF where
  fmap f = undefined    -- legális!

data Predicate a = Predicate (a -> Bool)   -- a paraméter mint *bemenet* jelenik, nem pedig mint kimenet!

instance Functor Predicate where
  fmap :: (a -> b) -> Predicate a -> Predicate b
  fmap f (Predicate g) = Predicate $ \b -> undefined
    -- házi feladat: lássuk be, hogy (Predicate $ \b -> True és Predicate $ \b -> False)-re a Functor törvények hamisak.


-- kontravariáns funktor     (lásd: variance OOP altípusozás)
class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

instance Contravariant Predicate where
  contramap f (Predicate g) = Predicate (g . f)


-- Monad osztály
--------------------------------------------------------------------------------

-- osztály hierarchia:
-- class Functor f => Applicative f  where ...
-- class Applicative m => Monad m    where ...

-- Functor:     map-elhető típusok                                          (1 metódus )
-- Applicative: általánosabban map-elhető típusok                           (+2 metódus)
-- Monad:       mellékhatást implementáló típus (kivétel, mutáció, stb...)  (+1 metódus)


-- imperatív pszeudokód:
--   állítások, közötte ; "szekvenciálás" (egymás után való végrehajtás)

-- tisztán funkc kód: definíciók sorrendje mindegy

-- foo x y z = ... where
--    d1 = ...
--    d2 = ...
--    ("d" sorrend mindegy)

-- foo() = {
--    var x = f(10, 20);
--    var y = g("foo", x);
--    return exp;
-- }

-- Monad instance:
--   ";" (szekvenciálás) a metódus, mint magasabbrendű függvény
--   Monad: túlterheli a pontosvesszőt


-- instance Monad Maybe
--------------------------------------------------------------------------------

-- mi a use case?
--   Nothing-ot úgy értelmezzük, mint kivétel
--   Just                      , mint sikeres végrehajtás
--   szeretnénk: rövidre záró szemantika

-- minden listaelemre alkalmazzuk a függvényt, és
-- ha bármilyen elemre Nothing-ot kapunk a végeredmény Nothing (validálás)
--   (egyszerű változat: (a -> Bool) -> [a] -> Bool, standard : "all" nevű függvény)
f1 :: (a -> Maybe b) -> [a] -> Maybe [b]
f1 f []     = Just []
f1 f (a:as) = case f a of
  Nothing -> Nothing
  Just b  -> case f1 f as of
    Nothing -> Nothing
    Just bs -> Just (b:bs)

f2 :: Maybe a -> Maybe b -> Maybe c -> Maybe (a, b, c)
f2 (Just a) (Just b) (Just c) = Just (a, b, c)
f2 _ _ _ = Nothing

f2' :: Maybe a -> Maybe b -> Maybe c -> Maybe (a, b, c)
f2' ma mb mc = case ma of
  Nothing -> Nothing
  Just a -> case mb of
    Nothing -> Nothing
    Just b -> case mc of
      Nothing -> Nothing
      Just c -> Just (a, b, c)

-- C-ben : hibakezelés: hibakód + switch a végeredményeken
--  Monad Maybe : hibakód helyett automatikus propagálás

-- bind ~ ";"

--    bal utasítás      jobb utasítás (függhet a bal végeredményétől)       (szekvenciált művelet)
bind :: Maybe a     ->            (a -> Maybe b)                        ->        Maybe b
bind Nothing _  = Nothing
bind (Just a) f = f a


f1' :: (a -> Maybe b) -> [a] -> Maybe [b]
f1' f []     = Just []
f1' f (a:as) =
  bind (f a)      $ \b  ->
  bind (f1' f as) $ \bs ->
  Just (b:bs)

  -- bind (f a) (\b ->
  -- bind (f1' f as) (\bs ->
  -- Just (b:bs)))

--   var b = f a;
--   var bs = f1' f as;
--   return (b:bs)

f2'' :: Maybe a -> Maybe b -> Maybe c -> Maybe (a, b, c)
f2'' ma mb mc =
  bind ma $ \a ->
  bind mb $ \b ->
  bind mc $ \c ->
  Just (a, b, c)

-- Just megfelel az imperatív "return"

-- class Applicative m => Monad m where
--   return :: a -> m a                      -- Just :: a -> Maybe a    (hatás nélküli utasítás, ami rögtön visszatér egy értékkel)
--   (>>=)  :: m a -> (a -> m b) -> m b      -- ";"  egymás utáni mellékhatásos végrehajtás

-- instance Monad Maybe where
--   return = Just
--   (>>=) = bind

f2''' :: Maybe a -> Maybe b -> Maybe c -> Maybe (a, b, c)
f2''' ma mb mc =
  ma >>= \a ->
  mb >>= \b ->
  mc >>= \c ->
  return (a, b, c)


-- szinaktikus cukorka: do notáció
f2'''' :: Maybe a -> Maybe b -> Maybe c -> Maybe (a, b, c)
f2'''' ma mb mc = do
  a <- ma
  b <- mb
  c <- mc
  return (a, b, c)
