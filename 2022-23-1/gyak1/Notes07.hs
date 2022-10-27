
{-# language InstanceSigs, DeriveFunctor #-}

import Control.Monad

------------------------------------------------------------

go :: String -> IO ()
go prevLine = do
  l <- getLine
  case l of
    "" -> putStrLn prevLine
    _  -> go l

readUntilEmpty :: IO ()
readUntilEmpty = do
  l <- getLine
  case l of
    "" -> return ()
    _  -> go l


------------------------------------------------------------


-- Definiáld a következő függvényeket tetszőlegesen,
-- de típushelyesen.
f1 :: Monad m => (a -> b) -> m a -> m b -- fmap nélkül definiáld!
f1 f ma = do
  a <- ma
  return (f a)

f2 :: Monad m => m a -> m b -> m (a, b)
f2 ma mb = do
  a <- ma
  b <- mb
  return (a, b)

f3 :: Monad m => m (m a) -> m a
f3 = undefined

f4 :: Monad m => m (a -> b) -> m a -> m b
f4 = undefined

f5 :: Monad m => (a -> m b) -> m a -> m b
f5 = undefined

f6 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
f6 = undefined

f7 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
f7 = undefined

f8 :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f8 = undefined


-- State monád
--------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

instance Applicative (State s) where
  pure  a = State (\s -> (a, s))
  (<*>) = ap

instance Monad (State s) where
  return = pure
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


-- State monád használata
------------------------------------------------------------

-- t :: State s a      Monad (State s)
--                     - 1 darab mutábilis referencia, "s" típusa
--                     - "a" visszatérési érték típusa
--                     - mellékhatás: referencia írás/olvasás

-- put    : egy konkrét értéket írunk a ref-be
-- get    : olvassa a ref jelenlegi értékét
-- modify : egy függvényt alkalmaz a ref jelenlegi értékén

--         futattni a definiált műveleteket
--             művelet       kezdő érték   (visszatérési érték, végső ref érték)
-- runState  :: State s a   ->  s           -> (a, s)
-- evalState :: State s a   ->  s           -> a
-- execState :: State s a   ->  s           -> s

-- put :: s -> State s ()           -- beír egy értéket az állapotba
-- get :: State s s                 -- értékként visszaadja az állapotot
-- modify :: (s -> s) -> State s () -- függvényt alkalmaz az állapotra

p1 :: State Int Int
p1 = do
  put 10
  put 20  -- felülírom az értéket
  modify (+100)
  n <- get
  return n

-- runState p1 0 == (120, 120)

p2 :: State Int ()
p2 = do
  modify (*10)
  return ()

-- runState p2  1 == ((), 10)
-- evalState p2 1 == ()
-- execState p2 1 == 10

------------------------------------------------------------

push :: a -> State [a] ()
push a = modify (a:)

pop :: State [a] (Maybe a)
pop = do
  as <- get
  case as of
    []   -> return Nothing
    a:as -> do
      put as
      return $ Just a


-- Definiálj egy függvényt, ami kicseréli egy fa leveleiben tárolt értékeket
-- balról jobbra haladva egy megadott lista elemeire. Használj State monádot!

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Show)

-- pl: replaceLeaves [10, 20, 30] (   Node (Leaf 2) (Leaf 3))
--                                 == Node (Leaf 10) (Leaf 20)
--     replacereplaceLeaves [5] (Leaf 10) == Leaf 5
--     replacereplaceLeaves [5]
--        (Node (Leaf 0) (Node (Leaf 0) (Leaf 0))) ==
--        (Node (Leaf 5) (Node (Leaf 0) (Leaf 0)))

replaceLeaves :: [a] -> Tree a -> Tree a
replaceLeaves as t = evalState (go t) as where

  -- go :: Tree a -> [a] -> (Tree a, [a])  -- State nélkül ez lenne a típus
  go :: Tree a -> State [a] (Tree a)
  go (Leaf a) = do
    ma <- pop
    case ma of
      Nothing -> return (Leaf a)
      Just a' -> return (Leaf a')

    -- ma <- pop
    -- return $ Leaf $ maybe a id ma
  go (Node l r) = do
    l' <- go l
    r' <- go r
    return $ Node l' r'


-- Definiáld a függvényt, ami megfordítja a fa leveleiben tárolt értékek
-- sorrendjét!  tipp: használhatod a replaceLeaves függvényt.
reverseElems :: Tree a -> Tree a
reverseElems = undefined



-- Írd át a következő függvényeket úgy, hogy *csak* a (State :: (s -> (a, s)) ->
-- State s a) konstruktort használd, monád/funktor instance-t és
-- get/put/modify-t ne használj.
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
modifyNTimes 0 f = pure ()
modifyNTimes n f = modify f >> modifyNTimes (n - 1) f


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


-- bónusz feladat: fordítsd meg a tárolt értékek sorrendjét egy tetszőleges
-- Traversable struktúrában!
-- (Traversable: lásd előadás. Röviden: a "traverse" függvényt használhatod
--  a megoldáshoz, ami a mapM általánosítása különböző struktrákra).
reverseElems' :: Traversable t => t a -> t a
reverseElems' = undefined


-- Foldable, Traversable
--------------------------------------------------------------------------------

-- Definiáld a következő instance-okat:

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr = undefined

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse = undefined


-- Definiáld a következő függvényeket úgy, hogy csak foldr-t használj!

isEmpty :: Foldable t => t a -> Bool
isEmpty = undefined

length' :: Foldable t => t a -> Int
length' = undefined

sum' :: (Foldable t, Num a) => t a -> a
sum' = undefined

toList' :: Foldable t => t a -> a
toList' = undefined

-- első elemet add vissza, ha van
safeHead :: Foldable t => t a -> Maybe a
safeHead = undefined

-- utolsó elemet add vissza, ha van
safeLast :: Foldable t => t a -> Maybe a
safeLast = undefined

-- bónusz feladat: definiáld a foldl-t foldr *egyszeri* felhasználásával,
-- rekurzió nélkül.
foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl' = undefined
