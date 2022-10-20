{-# language DeriveFunctor, ApplicativeDo, InstanceSigs #-}

import Data.Traversable
import Data.Foldable
import Control.Monad

-- ApplicativeDo : do-notációt használjunk Applicative kódban

-- Applicative, Foldable, Traversable, Parser
------------------------------------------------------------

-- Functor => Applicative => Monad

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- Monad m =>

-- definiáljuk a Monad metódusok felhasználásával
fmap' :: Monad m => (a -> b) -> m a -> m b
fmap' f ma = do
  a <- ma
  return $ f a

-- Monad szigorúan "többet tud" mint a Functor
-- Applicative: "többet tud" mint Functor, kevesebbet mint Monad

-- Applicative: N-áris fmap-et támogatja

-- standard def:
-- class Functor f => Applicative f where
--   -- pure megegyezik a return
--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b -- "ap"

-- (return helyett pure függvényt használjuk)
-- (return metódus létezik, az csak historikus okok miatt van)

--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b -- "ap"

amap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
amap2 f fa fb = fmap f fa <*> fb

    -- fa               :: f a
    -- fmap f fa        :: f (b -> c)
    -- fmap f fa <*> fb :: f c

-- fmap infix operátor formában: (<$>)
-- n-áris map:   f <$> fa <*> fb <*> fc <*> ... <*> fz

amap3 :: Applicative f => (a -> b -> c -> d)
                       -> f a -> f b -> f c -> f d
amap3 f fa fb fc = f <$> fa <*> fb <*> fc

   -- (<*>) balra asszociál!
   -- ((f <$> fa) <*> fb) <*> fc

amap4 :: Applicative f => (a -> b -> c -> d -> e)
                       -> f a -> f b -> f c -> f d -> f e
amap4 f fa fb fc fd = f <$> fa <*> fb <*> fc <*> fd

-- IO példa
p1 :: IO String
p1 = do
  l1 <- getLine
  l2 <- getLine
  pure $ l1 ++ l2

p1' :: IO String
p1' = (++) <$> getLine <*> getLine

-- imperatív pszeudokód:
-- String p1(){
--   return getLine() ++ getLine();
-- }

-- Control.Monad-ban elérhető:
--   (<*>) az újradefiniálása Monad metódusokkal:
-- ap :: Monad m => m (a -> b) -> m a -> m b
-- ap mf ma = do
--   f <- mf
--   a <- ma
--   return $ f a

-- fmap definiálása Applicative metódusokkal

--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b -- "ap"

fmapFromApplicative :: Applicative f => (a -> b) -> f a -> f b
fmapFromApplicative f fa = pure f <*> fa

-- State monad
------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return :: a -> State s a
  return a = State $ \s -> (a, s)

  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) (State f) g = State $ \s -> case f s of
    (a, s) -> runState (g a) s

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma


------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show, Functor)

label :: Tree a -> Tree (a, Int)
label t = evalState (go t) 0 where

  -- go :: Tree a -> State Int (Tree (a, Int))
  -- go (Leaf a) = do
  --   n <- get
  --   put (n + 1)
  --   return (Leaf (a, n))
  -- go (Node l r) = do
  --   l <- go l
  --   r <- go r
  --   return (Node l r)

  go :: Tree a -> State Int (Tree (a, Int))
  go (Leaf a) = do
    n <- get
    put (n + 1)
    pure (Leaf (a, n))
  go (Node l r) = Node <$> go l <*> go r

-- Milyen (mellékhatásos) kódot lehet csak Applicative metódusokkal
-- írni, Monad metódusok nélkül?
------------------------------------------------------------

-- mellékhatásos map megírható bind nélkül
mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f []     = pure []
mapA f (a:as) = (:) <$> f a <*> mapA f as

-- (opcionális házi) hasonlóképpen:
--  zipWithA :: Applicative f => (a -> b -> f c) -> [a] -> [b] -> f [c]

filterA :: Applicative f => (a -> f Bool) -> [a] -> f [a]
filterA f []     = pure []
filterA f (a:as) =
  (\b as -> if b then a:as else as) <$> f a <*> filterA f as

  -- -- Monád (ApplicativeDo-val működik)
    -- do b  <- f a
    --    as <- filterA f as
    --    return $ if b then a:as else as

-- különbség Applicative és Monad között:
--    Monad: interaktív programokat lehet írni
--           (későbbi műveletek függenek a korábbi eredményektől)
--    Applicative: statikusan (végrehajtás előtt) tudjuk,
--           hogy milyen mellékhatások jönnek létre

-- Applicative: jobban optimalizálható és elemezhető
--   Haxl library:
--     - beágyazott nyev adatbázis lekérdezésekre:
--     - ApplicativeDo: GHC minél inkábbmegpróbálja
--       Applicative formára hozni a programokat,
--       Applicative instance végrehajtás előtt optimalizál

-- Foldable, Traversable osztályok
------------------------------------------------------------

-- Foldable: foldr túlterhelésére osztály

-- foldr  :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- sum    :: (Foldable t, Num a) => t a -> a
-- length :: Foldable t => t a -> Int

instance Foldable Tree where
  -- kombináljuk az összes tárolt értéket a függvénnyel
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f b (Leaf a)   = f a b
  foldr f b (Node l r) = foldr f (foldr f b r) l

-- alternatív foldr, szebben néz ki:
-- foldr' :: (a -> (b -> b)) -> Tree a -> (b -> b)
-- foldr' f (Leaf a)   = f a
-- foldr' f (Node l r) = foldr' f l . foldr' f r

-- foldMap metódus
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf a)   = f a
  foldMap f (Node l r) = foldMap f l <> foldMap f r

--  opcionális házi: írjuk meg a köv függvényeket
--    csak foldr felhasználásával
--  sum, product, length, null, *foldl*, head, last


-- Traversable
------------------------------------------------------------

-- mellékhatásos map-elést túlterheljük

-- class (Functor t, Foldable t) => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf a)   = Leaf <$> f a
  traverse f (Node l r) = Node <$> traverse f l <*> traverse f r

  -- általánosan:
  --   1. vesszük a sima fmap definíciót
  --   2. konstruktorokat átírjuk adott aritású Applicative
  --      alkalmazásra

  -- traverse = fmap + Applicative mellékhatás

-- Foldable, Traversable derive-olható!
--   {-# language DeriveFoldable, DeriveTraversable #-}

t1 :: Tree Int
t1 = Node (Node (Leaf 0) (Leaf 1)) (Leaf 10)

-- traverse (\_ -> getLine) t1
-- bejárjuk a fát, beolvasunk egy sort minden levélnél,
--  visszaadjuk a String-ek fáját

-- Traverse másik metódusa: sequenceA

-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
-- sequenceA = traverse id

-- IO-ban, listákra
-- sequenceA :: [IO a] -> IO [a]

p2 :: IO [String]
p2 = sequenceA [getLine, getLine, getLine]

-- (extra anyag : list monád)
------------------------------------------------------------

-- instance Monad [] where
--   return :: a -> [a]
--   return a = [a]

--   (>>=) :: [a] -> (a -> [b]) -> [b]  -- concatMap
--   (>>=) = flip concatMap

list1 :: [(Int, Int)]
list1 = do
  x <- [0..10]
  y <- [0..x]
  pure (x + y, x)

list1' :: [(Int, Int)]
list1' = [(x + y, x) | x <- [0..10], y <- [0..x]]

list1'' :: [(Int, Int)]
list1'' =
  concatMap (\x -> concatMap (\y -> [(x + y, x)]) [0..x]) [0..10]

-- adjuk vissza az összes részlisták listáját
-- opcionális házi: hogy működik az alábbi definíció
sublists :: [a] -> [[a]]
sublists = filterM (\_ -> [True, False])

-- filterM' f [] = [[]]
-- filterM' f (a:as) = do
--   b  <- [True, False]
--   as <- filterM' f as
--   if b then [a:as] else [as]

-- f()
--   if ...
--        return
--   else _

-- do
--   l <- getLabel

--
--   goto l

-- do
--   res <- callcc $ \exit ->
--      _
--      _
--      exit 100
--   _
