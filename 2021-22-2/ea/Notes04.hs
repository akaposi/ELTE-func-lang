{-# language InstanceSigs #-}

import Prelude hiding (mapM, mapM_)

import Control.Monad (ap)

-- IO folyt, generikus monád függvények, State intro
------------------------------------------------------------

-- IO   :: * -> *
-- ma   :: IO a          ma egy mellékhatásos művelet, ami
--                       IO hatásokat tud létrehozni
--                       és "a" típusú értékkel tér vissza

-- Monad m,
-- m a                   -- "m" lehetséges mellékhatás
--                       -- "a" visszatérési érték típusa

-- m ()                  -- csak a hatás érdekes
--                       -- () ("unit"), egy értéke van: ()

-- data Zero
-- data One = One

-- lásd: void f()

------------------------------------------------------------

-- getLine  :: IO String         -- beolvas egy sort
-- putStrLn :: String -> IO ()   -- kinyomtat egy sort

p1 :: IO ()
p1 = do
  l <- getLine
  putStrLn ("hello "++l)

p1' :: IO ()
p1' =
  getLine >>= \l ->
  putStrLn ("hello "++l)

-- minden Monad egyben Functor is
-- fmap :: (a -> b) -> IO a -> IO b
--        visszatérési értékre egy tiszta függvényt alkalmazunk

--             getLine :: IO String
-- fmap length getLine :: IO Int


-- print :: Show a => a -> IO ()       -- kinyomtat egy Show értéket
-- print a = putStrLn (show a)


-- generikus Monad függvények
------------------------------------------------------------

-- standard függvények: import Control.Monad

-- return :: a -> m a
-- (>>=)  :: m a -> (a -> m b) -> m b

-- mellékhatásos map
-- map ::              (a ->   b) -> [a] ->   [b]
mapM     :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f []     = return []
mapM f (a:as) = do
  b  <- f a
  bs <- mapM f as
  return (b:bs)

-- konstans bind : egymás utáni végrehajtás
-- (>>) :: Monad m => m a -> m b -> m b
-- (ma >> mb) = ma >>= \_ -> mb

--   egymás utáni végrehajtás + második művelet függhet az
--     első visszatérési értékétől
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f []     = return ()
mapM_ f (a:as) = f a >> mapM_ f as  -- konstans bind elég!
   -- do f a
   --    mapM_ f as      -- szintaktikus cukorka konstans bind-ra

-- mapM (\n -> if even n then Just (n + 10) else Nothing) [0,2..10]
--  == Just [10,12,14,16,18,20]

-- opcionális házi
-- zipWithM  :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
-- zipWithM_ :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()

-- filterM  :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- filterM_ :: Monad m => (a -> m Bool) -> [a] -> m ()

-- replicate   :: Int -> a -> [a]  -- érték ismétlése

--   művelet ismétlése n-szer, visszaadjuk az összes végeredményt
--   listában
-- replicateM  :: Monad m => Int -> m a -> m [a]

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n ma | n <= 0 = return []
replicateM n ma = do
  a <- ma
  as <- replicateM (n - 1) ma
  return (a:as)

replicateM_ :: Monad m => Int -> m a -> m ()
replicateM_ n ma | n <= 0 = return ()
replicateM_ n ma = ma >> replicateM_ (n - 1) ma

-- loop :: a
-- loop = loop

forever :: Monad m => m a -> m b
forever ma = ma >> forever ma

-- Megszakítás ghci-ben: Ctrl-c-c
p2 :: IO ()
p2 = forever $ do
  l <- getLine
  putStrLn ("hello "++l)

sequence :: Monad m => [m a] -> m [a]
sequence = mapM id

   -- mapM :: (a -> m b) -> [a] -> m [b]
   -- mapM :: (m a -> m a) -> [a] -> m [a]

sequence_ :: Monad m => [m a] -> m ()
sequence_ = mapM_ id

-- "m a" típusú értékek tetszőleges tárolhatók, átadhatók,
-- anélkül, hogy végrehajtanánk őket


-- State monád
------------------------------------------------------------

-- mellékhatás: egy darab adott típusú mutábilis változó
--              (írható-olvasható változó)

-- State :: * -> * -> *      -- két paraméteres típus

-- State s a                 -- "s" : állapot típusa
--                           -- "a" : visszatérési érték típusa

-- ma :: State s a    -- művelet, ami egy "s" típusú változót
--                       módosíthat, és "a" értékkel tér vissza

-- instance Monad (State s)


-- motiváló példa:

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show)

-- balról-jobbra bejárás sorrendjében számozzuk be a leveleket:

label :: Tree a -> Tree (Int, a)
label t = fst (go 0 t) where   -- "go" : standard segédfüggvény név
  go :: Int -> Tree a -> (Tree (Int, a), Int)
  go n (Leaf a) = (Leaf (n, a), n + 1)
  go n (Node l r) = case go n l of
    (l', n') -> case go n' r of
      (r', n'') -> (Node l' r', n'')

-- data MyRecord = MyRecord {field1 :: Int, field2 :: Int}

-- fst :: (a,b) -> a
-- snd :: (a,b) -> b

-- field1 :: MyRecord -> Int
-- field2 :: MyRecord -> Int

-- newtype ugyanaz mint data, viszont csak 1 konstruktor + 1 mező lehet
--   newtype-nak nincs futásidejű költsége
newtype State s a = State {runState :: s -> (a, s)}

-- State    :: (s -> (a, s)) -> State s a
-- runState :: State s a -> (s -> (a, s))

   --     s              ->    (a, s)
   -- kezedeti állapot       (visszatérési érték, végső állapot)

-- Functor => Applicative => Monad

-- fmapDefault :: Monad m => (a -> b) -> m a -> m b
-- fmapDefault f ma = do
--    a <- ma
--    return (f a)

-- State    :: (s -> (a, s)) -> State s a
-- runState :: State s a -> (s -> (a, s))

instance Functor (State s) where
  --      (a -> b) -> (s -> (a, s)) -> (s -> (b, s))

  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State $ \s -> case g s of
    (a, s) -> (f a, s)

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return :: a -> State s a
  return a = State $ \s -> (a, s) -- nincs hatás: állapot nem változik

  -- (s -> (a, s)) -> (a -> s -> (b, s)) -> s -> (b, s)
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State g >>= f = State $ \s -> case g s of
    (a, s) -> runState (f a) s

    -- g :: s -> (a, s)
    -- f :: (a -> State s b)
    -- f a :: State s b
    -- runState (f a) :: s -> (b, s)
    -- runState (f a) s :: (b, s)

-- írás-olvasás:

-- State s a     : "s" állapotot mutálunk, "a" értéket adunk vissza

-- put: egy adott értéket beír az állapotba, és ()-t ad vissza
put :: s -> State s ()
put s = State $ \_ -> ((), s) -- kezdeti állapotot adott "s"-re
                              -- cseréljük

-- getLine :: IO String
-- get     :: State s s    -- "State s": hatás "s" írás-olvasás

-- a jelenlegi állapotot értékként visszaadja
get :: State s s
get = State $ \s -> (s,s)   -- állapot nem változik!

-- példákat:

f1 :: State Int ()
f1 = do
  n <- get
  if n < 0
    then put (n + 100)
    else put (n - 10)

-- runState f1 :: Int -> ((), Int)
-- runState f1 10 :: ((), Int)

evalState :: State s a -> s -> a
evalState ma s = fst (runState ma s)

execState :: State s a -> s -> s
execState ma s = snd (runState ma s)



label' :: Tree a -> Tree (Int, a)
label' t = evalState (go t) 0 where

  go :: Tree a -> State Int (Tree (Int, a))
  go (Leaf a) = do
    n <- get
    put (n + 1)
    return (Leaf (n, a))
  go (Node l r) = do
    l <- go l
    r <- go r
    return (Node l r)

-- mutábilis kód: definiáljuk mint absztrakció
--    (a háttérben csak immutáblisi típusok vannak!)
