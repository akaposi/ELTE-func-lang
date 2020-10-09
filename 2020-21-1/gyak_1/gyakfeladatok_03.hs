
{-# language DeriveFunctor #-}

import Control.Monad

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))
  State f >>= g = State (\s -> case f s of (a, s') -> runState (g a) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--------------------------------------------------------------------------------

push :: a -> State [a] ()
push a = modify (\as -> a:as)

pop :: State [a] (Maybe a)
pop = do
  as <- get
  case as of
    []   -> pure Nothing
    a:as -> do
      put as
      pure (Just a)

-- Írj egy függvényt, ami pop-ol egy Int-et, és ha az eredmény Just n, akkor n-szer
-- push-olja n-t.
poppush :: State [Int] ()
poppush = do
  mn <- pop
  case mn of
    Nothing -> pure ()
    Just n  -> replicateM_ n (push n)

-- Írj egy függvényt, ami kizárólag push és pop felhasználásával map-eli az állapot listát
mapPP :: (a -> a) -> State [a] ()
mapPP f = do
  ma <- pop
  case ma of
    Just a -> do
      mapPP f
      push (f a)
    Nothing -> pure ()

-- Írj egy függvényt, ami egy lista n-edik elemét módosítja egy függvénnyel, ha
-- a lista n-nél rövidebb, egyébként nem csinál semmit.
modifyAtIx :: (a -> a) -> Int -> State [a] ()
modifyAtIx f n = do
  as <- get
  case drop n as of
    a:as -> put (take n as ++ f a : as)
    []   -> pure ()

--------------------------------------------------------------------------------

-- Írj egy függvényt, ami egy Int listában minden elemet kicserél az addigi elemek maximumára.
-- Tegyük fel, hogy a lista nem-negatív számokat tartalmaz.
-- Használd a (State Int)-et. Legyen az állapot a jelenlegi maximális Int.
maxs :: [Int] -> [Int]
maxs as = evalState (go as) 0 where
  go []     = pure []
  go (n:ns) = do
    m <- get
    let m' = max n m
    put m'
    ns <- go ns
    pure (m':ns)

-- Értelmezd a következő utasítások listáját. Minden utasítás
-- egy Int-et módosító művelet. Az "Add n" adjon n-et a jelenlegi
-- állapothoz, a "Subtract n" vonjon ki n-t, és a "Mul" értelemszerűen.

data Op = Add Int | Subtract Int | Mul Int

evalOps :: [Op] -> State Int ()
evalOps []       = pure ()
evalOps (op:ops) = case op of
  Add n      -> modify (n+) >> evalOps ops
  Subtract n -> modify (\x -> x - n) >> evalOps ops
  Mul n      -> modify (*n) >> evalOps ops

-- Add meg ennek segítségével az állapotot módosító (Int -> Int) függvényt.
runOps :: [Op] -> Int -> Int
runOps ops = execState (evalOps ops)


--------------------------------------------------------------------------------

-- 1. Feladat: írd át az összes következő függvényt úgy, hogy
-- a (Monad m) helyett (Applicative m) legyen használva.
-- A függvények viselkedése ne változzon!
-- Tipp: Control.Applicative kombinátorait érdemes megnézni.

f1 :: Monad m => b -> m a -> m b
f1 b ma = do
  a <- ma
  return b

f1' :: Applicative m => b -> m a -> m b
f1' b ma = b <$ ma

f2 :: Monad m => m Bool -> m a -> m a -> m a
f2 mb ma ma' = do
  b <- mb
  if b then do {a <- ma; ma'; return a} -- egy sorban is lehet do blokk ilyen módon
       else do {ma; ma'}

f2' :: Applicative m => m Bool -> m a -> m a -> m a
f2' mb ma ma' = (\b a a' -> if b then a else a') <$> mb <*> ma <*> ma'

f3 :: Monad m => (a -> m b) -> [a] -> m [b]
f3 f []     = return []
f3 f (a:as) = do
  b  <- f a
  bs <- f3 f as
  return (b : bs)

f3' :: Applicative m => (a -> m b) -> [a] -> m [b]
f3' f []     = pure []
f3' f (a:as) = (:) <$> f a <*> f3' f as

f4 :: Monad m => (a -> m Bool) -> [a] -> m [a]
f4 f []     = return []
f4 f (a:as) = do
  b <- f a
  if b then do {as' <- f4 f as; return (a:as')}
       else f4 f as

f4' :: Applicative m => (a -> m Bool) -> [a] -> m [a]
f4' f []     = pure []
f4' f (a:as) = (\b as -> if b then a:as else as) <$> f a <*> f4' f as

f5 :: Monad m => m a -> m b -> m c -> m d -> m (a, c)
f5 ma mb mc md = do
  md
  c <- mc
  mb
  a <- ma
  return (a, c)

f5' :: Applicative m => m a -> m b -> m c -> m d -> m (a, c)
f5' ma mb mc md =
     (\d c b a -> (a, c)) <$> md <*> mc <*> mb <*> ma

f6 :: Monad m => (a -> b) -> m a -> m b
f6 f ma = do
  a <- ma
  return (f a)

f6' :: Applicative m => (a -> b) -> m a -> m b
f6' f ma = f <$> ma

f7 :: Monad m => m (a -> b) -> m a -> m b
f7 mf ma = do
  f <- mf
  a <- ma
  return (f a)

f7' :: Applicative m => m (a -> b) -> m a -> m b
f7' mf ma = mf <*> ma

f8 :: Monad m => m a -> m b -> m b
f8 ma mb = ma >> mb

f8' :: Applicative m => m a -> m b -> m b
f8' ma mb = ma *> mb
