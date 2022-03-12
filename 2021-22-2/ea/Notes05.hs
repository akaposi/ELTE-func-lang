{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Monad
import Data.Foldable

-- State ismétlés, Monád törvény, Applicative, Foldable, Traversable

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where

  -- return : nem módosítjuk az állapotot
  return :: a -> State s a
  return a = State (\s -> (a, s))

  -- egymás után két műveletet végrehajtunk
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) (State f) g = State $ \s -> case f s of
    (a, s) -> runState (g a) s


put :: s -> State s ()        -- konkrét "s" értékre módosítja az állapotot
put s = State $ \_ -> ((), s)

get :: State s s              -- lekérdezni a jelenlegi állapotot
get = State $ \s -> (s, s)
  -- do
  --   s <- get
  --   _

modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put (f s)

evalState :: State s a -> s -> a
evalState sta s = fst (runState sta s)

execState :: State s a -> s -> s
execState sta s = snd (runState sta s)

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

label :: Tree a -> Tree (Int, a)
label t = evalState (go t) 0 where

  go :: Tree a -> State Int (Tree (Int, a))
  go (Leaf a) = do
    n <- get
    put (n + 1)
    return (Leaf (n, a))
  go (Node l r) = do
    l <- go l
    r <- go r
    return (Node l r)


-- Monád törvények
------------------------------------------------------------

-- emlékezzünk:
--   Functor: fmap id x == x     fmap f (fmap g x) == fmap (f . g) x

-- class Functor m => Monad m where
--   return :: a -> m a
--   (>>=)  :: m a -> (a -> m b) -> m b

-- (.)       :: (b -> c) -> (a -> b) -> (a -> c)
-- flip (.)  :: (a -> b) -> (b -> c) -> (a -> c)

-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
-- (>=>) f g a = do
--   b <- f a
--   g b

-- törvények:
--    1. return egységeleme >=>-nek  (return-nek nincs mellékhatása)
--       f >=> return = f
--       return >=> f = f

--       ugyanez bind-al:
--           do {a <- ma; return a} == ma
--           do {a <- return a; f a} == f a

--   2. >=> asszociatív:
--        f >=> (g >=> h) == (f >=> g) >=> h

--        ugyanez bind-al (egymásba ágyazott do kilaposítható)
--            do {b <- do {a <- ma; f a}; g b}
--        ==  do {a <- ma; b <- f a; g b}

--    műveleteknek csak a sorrendje számít, nem a csoportosítása
--      "szekvenciális" a végrehajtás


-- Applicative
------------------------------------------------------------

-- Functor => Applicative => Monad

{-
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b     -- kiejtése: "ap"
-}

-- class Applicative m => Monad m where
--   ...

-- valójában csak az (<*>) új a korábbiakhoz képest!
--   return ugyanaz mint a pure
--     (backwards kompatibilitás miatt van return és pure külön)
--

-- ajánlás: használjuk a return helyett a pure-t!


-- tetszőleges aritású fmap-et tesz lehetővé
-- (<*>) :: f (a -> b) -> f a -> f b

-- fmap  :: (a -> b) -> f a -> f b
-- fmap2 :: (a -> b -> c) -> f a -> f b -> f c
-- fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d

-- Functor instance-ból kizárólag az 1-es aritású map jön.


-- fmap  :: (a -> b) -> f a -> f b
-- pure  :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
fmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2 f fa fb = pure f <*> fa <*> fb

  -- pure f :: f (a -> b -> c)
  -- pure f <*> fa  :: f (b -> c)p
  -- pure f <*> fa <*> fb :: f c

  -- alternatív:
  --   fmap f fa <*> fb

  --   fmap f fa :: f (b -> c)
  --   fmap f fa <*> fb

  --   fmap operátoros formája: (<$>)

  -- idiomatikus fmap2:
  --   f <$> fa <*> fb

fmap3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 f fa fb fc = f <$> fa <*> fb <*> fc

  -- fmapN:   f <$> arg1 <*> arg2 <*> ... <*> argN

label' :: Tree a -> Tree (Int, a)
label' t = evalState (go t) 0 where

  go :: Tree a -> State Int (Tree (Int, a))
  go (Leaf a) = do
    n <- get
    put (n + 1)
    return (Leaf (n, a))
  go (Node l r) =
    Node <$> go l <*> go r

  -- imperatív pszeudokód:
  -- go (Node l r) =
  --    return Node(go(l), go(r));


-- Mit lehet pusztán Applicative-al definiálni,
-- és mihez kell mindenképp Monad művelet?
------------------------------------------------------------

-- Applicative művelet
io1 :: IO String
io1 = (++) <$> getLine <*> getLine

-- Monád only
io2 :: IO ()
io2 = do
  l <- getLine
  case l of
    [] -> pure ()
    _  -> io2

-- Applicative: statikusan ismerjük az összes mellékhatást
-- Monad: "interaktív", csak a futás során derül ki, hogy milyen
--   mellékhatások történnek

-- Applicative adatbázis-lekérdezés: lehet optimalizálni futtatás előtt
--   library: Haxl (Facebook spam-szűrő logikája van benne implementálva)
--     (monádikus lekérdezést Applicative-ra próbál transzformálni,
--      Applicative részeket statikusan optimalizáljuk)

-- Monad m =>
-- Applicative f =>

-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f []     = pure []
mapA f (a:as) = (:) <$> f a <*> mapA f as

filterA :: Applicative f => (a -> f Bool) -> [a] -> f [a]
filterA f [] = pure []
filterA f (a:as) = process <$> f a <*> filterA f as
  where process b as = if b then a:as else as

   -- f a          :: f Bool
   -- filterA f as :: f [a]


-- Foldable osztály
------------------------------------------------------------

-- foldr-ezhető struktúrű osztálya
-- ekvivalensen: Foldable az, amire van toList függvény
--   (hatékonyság miatt van több különböző metódus, nem csak toList)

{-
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b
-}
  -- foldl
  -- sum
  -- length
  -- minimum

  -- lista foldr :: (a -> b -> b) -> b -> [a] -> b


-- data Tree a = Leaf a | Node (Tree a) (Tree a)
--   deriving (Show)

-- csaló verzió
treeToList :: Tree a -> [a]
treeToList (Leaf a) = [a]
treeToList (Node l r) = treeToList l ++ treeToList r -- kvadratikus

-- lineáris treeToList függvény:
treeToList' :: Tree a -> [a] -> [a]
treeToList' (Leaf a)   rest = a:rest
treeToList' (Node l r) rest = treeToList' l (treeToList' r rest)

-- DiffList-ra a (++) megadható a (.) operátorral
type DiffList a = [a] -> [a]

treeToList'' :: Tree a -> DiffList a
treeToList'' (Leaf a)   = (a:)
treeToList'' (Node l r) = treeToList' l . treeToList' r

-- (:)  :: a -> [a] -> [a]
-- (a:) :: [a] -> [a]

instance Foldable Tree where
  -- csaló verzió
  -- foldr f b t = foldr f b (treeToList' t [])

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f b (Leaf a)   = f a b           -- foldr f b [a] = f a b
  foldr f b (Node l r) = foldr f (foldr f b r) l

  -- foldr f z [a, b, c ... ] = f a (f b (f c .... (f ... z)))
  -- foldr f z (a:as) = f a (foldr f z as)

treeToList''' :: Tree a -> [a]
treeToList''' = foldr (:) []

--------------------------------------------------------------------------------
