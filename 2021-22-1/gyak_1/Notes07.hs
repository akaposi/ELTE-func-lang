
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

------------------------------------------------------------

-- Következő órai eleji feladat:
--    State + fa/lista

------------------------------------------------------------

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

pop :: State [a] (Maybe a)
pop = do
  as <- get
  case as of
    []   -> return Nothing
    a:as -> do
      put as
      return (Just a)

------------------------------------------------------------
-- canvas feladat

data Tree' a = Leaf1' a | Leaf2' a a | Node' (Tree' a) (Tree' a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- State [a] ()
-- állapot: [a]

toList :: Tree' a -> [a]
toList t = execState (go t) [] where

  -- -- naiv: balról jobbra járjuk a fát (nem hatékony ++)
  -- go :: Tree' a -> State [a] ()
  -- go (Leaf1' a) = do
  --   as <- get
  --   put (as ++ [a])
  -- go (Leaf2' a1 a2) = do
  --   as <- get
  --   put (as ++ [a1, a2])
  -- go (Node' l r) = go l >> go r

  -- jobbról járjuk be a fát, lista elejére tesszük
  -- az értéket (hatékony, mivel (:)-ot használunk csak)
  -- (jobbra zárójelezett ++ lineáris idejű, balra: kvadratikus)
  go :: Tree' a -> State [a] ()
  go (Leaf1' a) = do
    as <- get
    put (a:as)
  go (Leaf2' a1 a2) = do
    as <- get
    put (a1:a2:as)
  go (Node' l r) = go r >> go l

toList' :: Tree' a -> [a]
toList' t = execState (traverse go t) [] where
  go :: a -> State [a] ()
  go a = do
    as <- get
    put (as ++ [a])

------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Functor, Foldable, Traversable)

-- Definiálj egy függvényt, ami jobbról balra bejárva map-eli egy fa elemeit
-- egy mellékhatásos függvénnyel.
traverseRightToLeft :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
traverseRightToLeft = go where
  go :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  go f (Leaf a)   = Leaf <$> f a
  go f (Node l r) = (\l r -> Node r l) <$> go f r <*> go f l

-- definiáld újra az előzői órai függvényeket "traverse" felhasználásával!

-- cseréljük ki minden elemet az előző elemek maximumára
maxs' :: [Int] -> [Int]
maxs' ns = evalState (traverse go ns) 0 where
  go :: Int -> State Int Int
  go n = do
    m <- get     -- eddigi maximum
    put (max n m)
    pure m       --

-- evalState :: State Int [Int] -> Int -> [Int]

-- traverse : map + mellékhatás
-- map      :: (Int ->           Int) -> [Int] ->           [Int]
-- traverse :: (Int -> State Int Int) -> [Int] -> State Int [Int]

-- maxs' ns = evalState (go ns) 0 where
--   go :: [Int] -> State Int [Int]
--   go [] = pure []
--   go (n:ns) = do
--     m <- get
--     put (max n m)
--     ns' <- go ns
--     pure (m:ns')

-- leveleket cseréljük ki egy lista elemeire
-- állapot : [a]
-- Traversable struktúra : Tree a
replaceLeaves' :: [a] -> Tree a -> Tree a
replaceLeaves' as t = evalState (traverse go t) as where

  -- fmap     :: (a ->           a) -> Tree a ->            Tree a
  -- traverse :: (a -> State [a] a) -> Tree a -> State [a] (Tree a)

  -- használjuk a hole-t!
  --     traverse _ t        _ :: a -> State [a] a

  go :: a -> State [a] a
  go a = do
    ma <- pop     -- pop :: State [a] (Maybe a)
    case ma of
      Nothing -> pure a
      Just a' -> pure a'

-- házi + feladatsor lesz

reverseElems' :: Tree a -> Tree a
reverseElems' = undefined

treeSums' :: Tree Int -> Tree Int
treeSums' = undefined

-- definiáld újra a következő függvényt "traverseRightToLeft" felhasználásával
labelRightToLeft' :: Tree a -> Tree (Int, a)
labelRightToLeft' = undefined

--------------------------------------------------------------------------------

-- Írd át az összes következő függvényt úgy, hogy a (Monad m) helyett
-- (Applicative m) legyen használva. A függvények viselkedése ne változzon!
-- Tipp: Control.Applicative kombinátorait érdemes megnézni.

-- Feladat: program, ami *nem interaktív*, (nincsenek valódi
--   függőségek mellékhatások között, futtatás előtt látjuk,
--   hogy mik a hatások)

f1 :: Monad m => b -> m a -> m b
f1 b ma = do
  a <- ma
  return b

-- Functor elég!
f1' :: Functor m => b -> m a -> m b   -- ugyanígy a többi függvényre
f1' b ma = fmap (\_ -> b) ma

f2 :: Monad m => m Bool -> m a -> m a -> m a
f2 mb ma ma' = do
  b <- mb
  if b then do {a <- ma; ma'; return a}
       else do {ma; ma'}

-- Tipp: megnézzük, hogy mik a mellékhatásos műveletek
-- írunk egy N-aritású Applicative map-et
f2' :: Applicative m => m Bool -> m a -> m a -> m a
f2' mb ma ma' =
  (\b a a' -> if b then a else a') <$> mb <*> ma <*> ma'

f3 :: Monad m => (a -> m b) -> [a] -> m [b]
f3 f []     = return []
f3 f (a:as) = do
  b  <- f a
  bs <- f3 f as
  return (b : bs)

f3' :: Applicative m => (a -> m b) -> [a] -> m [b]
f3' = traverse

-- std függvény: filterM, lista szűrés + mellékhatás
f4 :: Monad m => (a -> m Bool) -> [a] -> m [a]
f4 f []     = return []
f4 f (a:as) = do
  b <- f a
  if b then do {as' <- f4 f as; return (a:as')}
       else f4 f as
  -- sorban (f a) és (f4 f as) műveletet hajtja végre
  --   (minden ágon ugyanazok a műveletek hajtódnak végre)

f4' :: Applicative m => (a -> m Bool) -> [a] -> m [a]
f4' f [] = pure []
f4' f (a:as) =
  (\b as' -> if b then a:as' else as') <$> f a <*> f4' f as

f5 :: Monad m => m a -> m b -> m c -> m d -> m (a, c)
f5 ma mb mc md = do
  md
  c <- mc
  mb
  a <- ma
  return (a, c)

f6 :: Monad m => (a -> b) -> m a -> m b
f6 f ma = do
  a <- ma
  return (f a)

f7 :: Monad m => m (a -> b) -> m a -> m b
f7 mf ma = do
  f <- mf
  a <- ma
  return (f a)

f8 :: Monad m => m a -> m b -> m b
f8 ma mb = ma >> mb


-- Írd át a következő függvényeket úgy, hogy *csak* a State newtype konstruktort
-- használd, monád/funktor instance-t és get/put/modify-t ne használj.
--------------------------------------------------------------------------------

modify' :: (s -> s) -> State s ()
modify' f = do
  s <- get
  put (f s)

  -- Művelet végrehajtása lokálisan: az állapot visszaáll a művelet után.
locally :: State s a -> State s a
locally ma = do
  s <- get        -- "elmentjük" az állapotot
  a <- ma         -- futtatjuk ma-t
  put s           -- visszaállítjuk
  pure a

-- Állapot módosítás n-szer
modifyNTimes :: Int -> (s -> s) -> State s ()
modifyNTimes n f = replicateM_ n (modify f)

--------------------------------------------------------------------------------

-- Értelmezd a következő utasítások listáját. Minden utasítás
-- egy Int-et módosító művelet. Az "Add n" adjon n-et a jelenlegi
-- állapothoz, a "Subtract n" vonjon ki n-t, és a "Mul" értelemszerűen.
data Op = Add Int | Subtract Int | Mul Int

evalOps :: [Op] -> State Int ()
evalOps = undefined

-- Add meg ennek segítségével az állapotot módosító (Int -> Int) függvényt.
runOps :: [Op] -> Int -> Int
runOps = undefined

------------------------------------------------------------
