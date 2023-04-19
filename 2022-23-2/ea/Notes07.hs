{-# language ApplicativeDo #-}

import Control.Monad hiding (guard)

-- (követelmény anyag: State, Applicative
--  extra: lista monád, egyéb Monad instance-ok
--         Const applicative instance)

-- Functor => Applicative => Monad

-- Mi a lényeg?
--   (prog feladatot *jobban* megoldani, mint X nélkül)
--   (nem lesz hatékonyabb)
--   (bónusz: tömörebb, jobban átlátható/refaktorálható kód)
--   (mapM, filterM, stb.)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)

-- leveleket számozzuk be bejárási sorrendben
label :: Tree a -> Tree (a, Int)
label t = fst (go t 0) where

              --  State Int (Tree (a, Int))
  go :: Tree a -> Int -> (Tree (a, Int), Int)
  go (Leaf a) n = (Leaf (a, n), n + 1)
  go (Node l r) n = case go l n of
    (l, n) -> case go r n of
      (r, n) -> (Node l r, n)

  -- ugyanez megírható úgy, hogy 1 darab Int típusú
  -- mutábilis változó

-- instance Functor ((->) c) where
--    fmap = (.)

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State $ \s -> case g s of
    (a, s) -> (f a, s)

instance Applicative (State s) where
  pure :: a -> State s a
  pure = return

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) mf ma = do
    f <- mf
    a <- ma
    return (f a)

  -- Ha van Monad instance, akkor automatikusan
  -- következik az Applicative instance
  -- (Functor is következik)

{-
fmapFromMonadMethods :: Monad m => (a -> b) -> m a -> m b
fmapFromMonadMethods f ma = do
  a <- ma
  return (f a)
-}

instance Monad (State s) where

  -- return: minden Monad-ban: nincs mellékhatása
  -- State s: mellékhatás: "s" típusú állapot módosítása
  return :: a -> State s a
  return a = State (\s -> (a, s))  -- s |-> s

  -- (s -> (a, s)) -> (a -> s -> (b, s)) -> s -> (b, s)
  -- futtatjuk az első műveletet, aztán a másodikat
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) (State f) g = State $ \s -> case f s of
    (a, s) -> runState (g a) s
           -- g a s   (runState nélkül)

-- írjuk az állapotot konkrét értékkel
put :: s -> State s ()
put s = State $ \_ -> ((), s)  -- régi állapotot s-re cseréli

-- olvassuk az állapotot
get :: State s s
get = State $ \s -> (s, s)     -- állapot változatlan,
                               -- értékként visszaadjuk

-- függvényt alkalmazunk az állapotra
modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put (f s)

-- f :: State s a
-- runState f :: s -> (a, s)
-- runState f s :: (a, s)

-- érték kell eredménynek
evalState :: State s a -> s -> a
evalState f s = fst (runState f s)

-- állapot kell eredménynek
execState :: State s s -> s -> s
execState f s = snd (runState f s)


-- leveleket számozzuk be bejárási sorrendben
label' :: Tree a -> Tree (a, Int)
label' t = evalState (go t) 0 where
  go :: Tree a -> State Int (Tree (a, Int))
  go (Leaf a) = do
    n <- get
    put (n + 1)
    return (Leaf (a, n))
  go (Node l r) = do
    l <- go l
    r <- go r
    return (Node l r)

-- State s a: egy darab "s" típus írható-olvasható változó
-- runState, evalState, execState


-- Lista Monad
------------------------------------------------------------

{-
instance Monad [] where

  return :: a -> [a]  -- return: nincs mellékhatás
  return a = [a]

  (>>=) :: [a] -> (a -> [b]) -> [b]
  (>>=) as f = concatMap f as
-}

l1 :: [Int]
l1 = do
  x <- [0..10]
  y <- [0..10]
  return (x * y)

-- lista kifejezés az a do-nak a cukorkája
l1' :: [Int]
l1' = [x * y | x <- [0..10], y <- [0..10]]

l2 :: [Int]
l2 = [x * y | x <- [0..10], y <- [0..10], even x]

guard :: Bool -> [()]
guard True  = return () -- nincs mellékhatás
guard False = []

l2' :: [Int]
l2' = do
  x <- [0..10]
  y <- [0..10]
  guard $ even x
  return (x * y)

l2'' :: [Int]
l2'' =
  flip concatMap [0..10] $ \x ->
  flip concatMap [0..10] $ \y ->
  flip concatMap (if even x then [()] else []) $ \_ ->
  [x * y]

-- adjuk vissza egy lista összes részlistáját
sublists :: [a] -> [[a]]
sublists as = filterM (\_ -> [True, False]) as
-- minden érték szűrve is van meg nem is

  -- filterM :: (a -> m Bool) -> [a] -> m [a]
  -- filterM :: (a -> [Bool]) -> [a] -> [[a]]

-- -- filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- filterM :: (a -> [Bool]) -> [a] -> [[a]]
-- filterM f []     = [[]]  -- OK
-- filterM f (a:as) = do    -- f az (\_ -> [True, False])
--   b  <- f a
--   as <- filterM f as
--   if b then
--     return (a:as)
--   else
--     return as

   -- visszaadjuk az összes eredményt amiben benne van "a"
   -- és az összes olyat is amiben nincs benne.

-- Applicative
------------------------------------------------------------

{-
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b    -- "ap"
-}

-- Viszont! pure = return-el minden olyan esetben, ha
-- Monad instance is van.

-- OK: library kompatibilitás
--     először volt   Functor => Monad
--     később         Functor => Applicative => Monad

-- ideális esetben:
-- class Functor where fmap
-- class Applicative where pure; (<*>)
-- class Monad where (>>=)

-- Mire jó az Applicative?
--   fmap, ahol N-paraméteres függvényt map-elünk

-- fmap :: Functor f => (a -> b) -> f a -> f b

-- nincs:
-- fmap2 :: Functor f => (a -> b -> c) -> f a -> f b -> f c
-- fmap2 f fa fb =

-- nincs:
-- fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- fmap3 =

-- fmapN definiálható, hogyha Applicative f van

-- pure  :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b   -- "f alatt"
--                                     -- alkalmazunk fv-t

fmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2 f fa fb = pure f <*> fa <*> fb
  -- pure f               :: f (a -> b -> c)
  -- pure f <*> fa        :: f (b -> c)
  -- pure f <*> fa <*> fb :: f c
  -- (f x) y

-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- (<$>) = fmap

fmap2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2' f fa fb = f <$> fa <*> fb

-- fmapN f arg1 arg2 .. argN =
--    f <$> arg1 <*> .... <*> argN

-- imperatív párhuzamok:

-- Haskell-ben:
--   f <$> x <*> y <*> z
-- imperatív pszeudokód:
--   f(x, y, z)

-- C-ben nem szoktunk:
--    f(++x; ++y; putc('c'));  -- sorrend nem definiált
--
-- Haskell-ben:
--   f <$> x <*> y     -- *mindig* balról jobbra

   -- (x += 10; x += 20)   (kommutatív)
   -- (x += 20; x += 10)

-- Applicative: 95%-ban: rövidebb kód
--              5%-ban:  ha van olyan Applicative ami nem Monád
--                       extra lehetőségek

-- forever :: Monad m => m a -> m b
-- forever ma = ma >> forever ma

-- mapM ::
-- filterM ::
-- zipWithM
-- replicateM :: Int -> m a -> m [a]
-- replicateM_ :: Int -> m a -> m ()

-- Monád: "interaktív"
--    (>>=) : mellékhatás függ a visszatérési értéktől
-- Applicative: "nem interaktív"
--             futtatás előtt tudjuk, hogy mi a program hatása

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f []     = pure []
mapA f (a:as) = (:) <$> f a <*> mapA f as

-- mapA: futtatás előtt tudjuk, hogy milyen hatás jön létre

-- Bizonyos Applicative-ok:
--   - statikusan elemezhető
--   - Haxl (Facebook)
--       runMyDSL :: MyDSL a -> IO a
--       MyDSL a
--       - A GHC mindent megpróbál minél inkább Applicative-ra
--         fordítani
--       -

-- random Facebook employee:
--     do
--        x <- ...
--        y <- ..
--        return (foo x y)

-- GHC:   foo <$> x <*> y
--    runMYDSL (foo <$> x <*> y) (statikus elemzés)
