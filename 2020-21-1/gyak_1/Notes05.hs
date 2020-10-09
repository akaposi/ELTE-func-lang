
{-# language DeriveFunctor #-}
{-# language ScopedTypeVariables #-}

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
-- Minden helytelen megoldás 1 pont

-- nem volt definiálva Monad (State s)
-- mivel newtype State s a = State {runState :: s -> (a, s)}

f :: (a -> b -> c) -> (s -> (a, s)) -> (s -> (b, s)) -> (s -> (c, s))
f g h1 h2 s = case h1 s of
  (a, s') -> case h2 s' of
    (b, s'') -> (g a b, s'')

f' :: (a -> b -> c) -> State s a -> State s b -> State s c
f' g (State h1) (State h2) =
  State $ \s -> case h1 s of
    (a, s') -> case h2 s' of
      (b, s'') -> (g a b, s'')

-- push/pop
--------------------------------------------------------------------------------

-- get :: State s s                     -- lekérdezés
-- put :: s -> State s ()               -- állapot írása
-- modify :: (s -> s) -> State s ()     -- módosítás függvénnyel
-- + monad instance

-- Egészítsd ki az állapotot egy elemmel. Ne használd a State konstruktort!
-- Állapot: [a]
-- módosítás: adjunk egy elemet a listára
push :: a -> State [a] ()
push a = modify (\as -> a:as)
      -- modify (a:)

-- futtatás: runState, evalState, execState
-- runState  :: State s a -> s -> (a, s)
-- evalState :: State s a -> s -> a
-- execState :: State s a -> s -> s

-- példa: execState (push 10) [] == [10]
-- execState (push 10 >> push 20 >> push 30) [] == [30, 20, 10]

f1 :: State [Int] ()
f1 = do
  as <- get
  case as of
    [] -> push 5 >> push 10
    _  -> push 20

-- Ha az állapot üres, adj vissza Nothing-ot, egyébként vedd le az első
-- listaelemet az állapotról, és add vissza Just-ban. Ne használd a State
-- konstruktort!
pop :: State [a] (Maybe a)
pop = do
  as <- get
  case as of
    []   -> pure Nothing   -- (jegyezzük meg: nincs control-flow hatása return-nek!) (használjuk return helyett a pure nevet)
    a:as -> do
      put as -- maradék listát visszaírjuk az állapotba
      pure (Just a)

-- példa: execState (pop >> pop >> pop) [0..10] == [3,4,5,6,7,8,9,10]
--        evalState (pop >> pop >> pop) [0..10] == Just 2
--        evalState (pop >> pop >> pop) [0, 1] == Nothing

-- Írd meg mindkét függvényt csak a State konstruktort használva!
push' :: a -> State [a] ()
push' a = State $ \as -> ((), a:as)

pop' :: State [a] (Maybe a)
pop' = State $ \as -> case as of
  []   -> (Nothing, [])
  a:as -> (Just a, as)

push'' :: a -> [a] -> ((), [a])
push'' a as = ((), a:as)

push''' :: a -> [a] -> [a]
push''' a as = (a:as)
    -- push''' = (:)

pop'' :: [a] -> (Maybe a, [a])
pop'' as = case as of
  []   -> (Nothing, [])
  a:as -> (Just a, as)


--------------------------------------------------------------------------------

-- Számozzuk be balról jobbra egy fa leveleit

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Show)

-- egy Int-et minded Leaf-nél megnövelünk
-- tehát: State Int -et használunk
label :: Tree a -> Tree (a, Int)
label t = evalState (go t) 0 where

  -- go t               :: State Int (Tree (a, Int))
  -- evalState (go t)   :: Int -> Tree (a, Int)
  -- evalState (go t) 0 :: Tree (a, Int)
  --           ^      ^
  --   State művelet  kezdő állapot

  go :: Tree a -> State Int (Tree (a, Int))
  go (Leaf a)   = do    -- annotáljuk a-t a jelenlegi Int értékkel
    n <- get            -- State-ben, ha bind-olunk, akkor <- jobb oldaláb (State s a) kifejezés kell hogy legyen
    put (n + 1)
    pure (Leaf (a, n))
  go (Node l r) = do
    l' <- go l          -- bal fa címkézve
    r' <- go r          -- jobb fa címkézve
    pure (Node l' r')

    -- go l >>= \l' ->
    -- go r >>= \r' ->
    -- pure (Node l' r')

-- label (Node (Node (Leaf True) (Leaf True)) (Leaf False))
--   == (Node (Node (Leaf (True, 0)) (Leaf (True, 1))) (Leaf (False, 2)))

-- instance Monad (State s) where
--   -- return :: a -> State s a
--   return a = _
--   -- (>>=) :: State s a -> (a -> State s b) -> State s b
--   State f >>= g = State $ \s ->
--       case f s of                       -- 1. végrehajtjuk f-et s-en
--         (a, s') -> runState (g a) s'    -- 2. végrehajtjuk (g a) s'-n

--   f :: s -> (a, s)
--   g :: a -> State s b
--   cél : s -> (b, s)


--------------------------------------------------------------------------------

-- Definiálj egy függvényt, ami kicsérli egy fa leveleiben tárolt értékeket
-- balról jobbra haladva egy megadott lista elemeire.
-- Használj State monádot!

-- pl: replaceLeaves [10, 20, 30] (Node (Leaf 2) (Leaf 3)) == Node (Leaf 10) (Leaf 20)
--     replacereplaceLeaves [5] (Leaf 10) == Leaf 5
--     replacereplaceLeaves [5] (Node (Leaf 0) (Node (Leaf 0) (Leaf 0))) ==
--        (Node (Leaf 5) (Node (Leaf 0) (Leaf 0)))

replaceLeaves :: [a] -> Tree a -> Tree a
replaceLeaves = undefined

-- Ugyanezt jobbról balra is implementáld! Azaz jobbról balra haladj
-- a fában, és úgy illeszd a lista elemeit a fába.
replaceLeaves' :: [a] -> Tree a -> Tree a
replaceLeaves' = undefined


-- Definiáld a függvényt, ami megfordítja a fa leveleiben tárolt értékek sorrendjét!
-- tipp: használd a replaceLeaves függvényt.
reverseElems :: Tree a -> Tree a
reverseElems = undefined

--------------------------------------------------------------------------------

-- Írd át a következő függvényeket úgy, hogy *csak* a State
-- konstruktort használd, monád/funktor instance-t és get/put/modify-t
-- ne használj.
modify' :: (s -> s) -> State s ()
modify' f = State $ \s -> ((), f s)
  -- modify' f = do
  --   s <- get
  --   put (f s)

  -- Művelet végrehajtása lokálisan: az állapot visszaáll a művelet után.
locally :: State s a -> State s a
locally ma = State $ \s -> case runState ma s of
  (a, s') -> (a, s)  -- régi s-et adom vissza

  -- locally ma = do
  --   s <- get        -- "elmentjük" az állapotot
  --   a <- ma         -- futtatjuk ma-t
  --   put s           -- visszaállítjuk
  --   pure a

  -- execState (push 10 >> push 10 >> locally (push 10 >> push 10)) [] == [10,10]
  -- runState (push 10 >> push 20 >> locally pop) [] == (Just 20, [20, 10])
  -- házi feladat: lépésenként behelyettesíteni a get/put/bind/pure definíciókat


-- Állapot módosítás n-szer
modifyNTimes n f = State $ runState (go n) where
  go 0 = State $ \s -> ((), s)                      -- pure/return definíciója
  go n = State $ \s -> runState (go (n - 1)) (f s)
  -- modifyNTimes 0 f = pure ()
  -- modifyNTimes n f = modify f >> modifyNTimes (n - 1) f

-- Hajtsunk végre egy műveletet n-szer, az összes köztes állapotot és
-- visszatérési értéket adjuk visza listában.
iterateState :: Int -> State s a -> State s [(a, s)]
iterateState n (State f) = State $ \s ->
  case n of
    0 -> ([], s)
    n -> case f s of
      (a, s') -> case iterateState (n - 1) (State f) `runState` s of
        (ass, s'') -> ((a, s'):ass, s'')

-- iterateState n ma = go n where
--   go 0 = pure []
--   go n = do
--     a    <- ma
--     s    <- get
--     rest <- go (n - 1)
--     pure ((a, s):rest)
