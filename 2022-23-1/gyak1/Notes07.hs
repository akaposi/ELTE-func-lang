
{-# language InstanceSigs, DeriveFunctor #-}

import Control.Monad

-- kisfeladat:
------------------------------------------------------------

-- Egyszerű State definíció valamilyen függvényre
--   get,put,modify,bind
--   nincs: adatszerkezet reukrzív bejárás

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
f1 f ma = fmap f ma
  -- do
  --   a <- ma
  --   return (f a)

-- Applicative: tetszőleges aritású fmap-elés
--  f <$> arg1 <*> arg2 <*> arg3

-- fmap2 :: Applicative m => (a -> b -> c) -> m a -> m b -> m c
-- fmap2 f ma mb = f <$> ma <*> mb

-- fmap3 ::
--   Applicative m => (a -> b -> c -> d)
--                 -> m a -> m b -> m c -> m d
-- fmap3 f ma mb mc = f <$> ma <*> mb <*> mc

-- f <$> ma               fmap f ma       (1-es fmap aritás)
-- f <$> ma <*> mb                        (2-es aritás)
-- f <$> ma <*> mb <*> mc                 (3-as aritás)
-- .....

-- (,) :: (a -> b -> (a, b))
-- fmap2 (,) :: m a -> m b -> m (a, b)
f2 :: Monad m => m a -> m b -> m (a, b)
f2 ma mb = (,) <$> ma <*> mb
  -- do
  --  a <- ma
  --  b <- mb
  --  return (a, b)

-- standard: Control.Monad.join
f3 :: Monad m => m (m a) -> m a
f3 mma = do
  ma <- mma
  ma        -- ma :: m a

-- do
--   ma <- mma
--   a <- ma           (monád törvény!)
--   return a

-- standard: (<*>)  ("ap")
--   (<*>) :: Applicative f => f (a -> b) -> f a -> f b
f4 :: Monad m => m (a -> b) -> m a -> m b
f4 = (<*>)

  -- do
  --   f <- mf
  --   a <- ma
  --   return (f a)

-- (>>=) :: m a -> (a -> m b) -> m b
-- Control.Monad.(=<<)
f5 :: Monad m => (a -> m b) -> m a -> m b
f5 = (=<<)

f6 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
f6 f ma mb = f <$> ma <*> mb

f7 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
f7 f ma mb mc = f <$> ma <*> mb <*> mc

--        (.) :: (b ->   c) -> (a ->   b) -> a ->   c

-- Control.Monad.(>=>)     ("kompozíció")

f8 :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f8 = (>=>)
  -- do
  --   b <- f a
  --   g b

  -- monád törvények:
  --    return >=> f = f
  --    f >=> return = f
  --    ((f >=> g) >=> h) = (f >=> (g >=> h))


-- State monád
------------------------------------------------------------

newtype State s a =
  State {runState :: s -> (a, s)} deriving Functor

  -- State művelet:

  --  bemenő állapot        (érték, új állapot)
  --    s              ->   (a,     s         )

  -- runState :: State s a -> (s -> (a, s))

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
reverseElems t = replaceLeaves (revToList t []) t where
  revToList :: Tree a -> [a] -> [a]
  revToList (Leaf a)   acc = a : acc
  revToList (Node l r) acc =
    revToList r (revToList l acc)

-- revToList vesd össze:
--   reverse as = foldl (\acc x -> x : acc) [] as

-- Írd át a következő függvényeket úgy, hogy *csak* a (State :: (s -> (a, s)) ->
-- State s a) konstruktort használd, monád/funktor instance-t és
-- get/put/modify-t ne használj.
--------------------------------------------------------------------------------

modify' :: (s -> s) -> State s ()
modify' f = do
  s <- get
  put (f s)

modify'' :: (s -> s) -> State s ()
modify'' f = State $ \s -> ((), f s)

-- Művelet végrehajtása lokálisan: az állapot visszaáll a művelet   után.
locally :: State s a -> State s a
locally ma = do
  s <- get        -- "elmentjük" az állapotot
  a <- ma         -- futtatjuk ma-t
  put s           -- visszaállítjuk
  pure a

  -- (pure ugyanaz mint a return)
  --  pure Applicative metódus
  --  használjuk return helyett a pure-t

locally' :: State s a -> State s a
locally' (State f) = State $ \s -> case f s of
  (a, s') -> (a, s)

-- Állapot módosítás n-szer
modifyNTimes :: Int -> (s -> s) -> State s ()
modifyNTimes 0 f = pure ()
modifyNTimes n f = modify f >> modifyNTimes (n - 1) f

-- modifyNTimes n f = replicateM_ n (modify f)

modifyNTimes' :: Int -> (s -> s) -> State s ()
modifyNTimes' n f = State (go n f) where

  go :: Int -> (s -> s) -> s -> ((), s)
  go 0 f s = ((), s)
  go n f s = go (n - 1) f (f s)

-- egyszerű példa a State definíciók kifejtésére:

p3 :: State Int Int
p3 = do
  n <- get
  put (n + 10)
  pure n

-- State $ \n -> (n, n + 10)

-- get >>= \n -> put (n + 10) >> pure n

-- State (\s -> (s, s)) >>= (\n -> put (n + 10) >> pure n)

-- State $ \s -> case (\s -> (s, s)) s of
--   (x, s) -> runState (\n -> put (n + 10) >> pure n) x s

-- State $ \s -> case (s, s) of
--   (x, s) -> runState (\n -> put (n + 10) >> pure n) x s

-- State $ \s -> runState ((\n -> put (n + 10) >> pure n) s) s

-- State $ \s -> runState (put (s + 10) >> pure s) s

-- ...

-- State $ \n -> (n, n + 10)

--------------------------------------------------------------------------------

-- Értelmezd a következő utasítások listáját. Minden utasítás
-- egy Int-et módosító művelet. Az "Add n" adjon n-et a jelenlegi
-- állapothoz, a "Subtract n" vonjon ki n-t, és a "Mul" értelemszerűen szorozza n-el az állapotot.
data Op = Add Int | Subtract Int | Mul Int

-- [Add 10, Add 20, Subtract 10, Mul 300]

evalOps :: [Op] -> State Int ()
evalOps ops = mapM_ go ops where
  go :: Op -> State Int ()
  go (Add n)      = modify (+n)
  go (Subtract n) = modify (\x -> x - n)
  go (Mul n)      = modify (*n)


-- Add meg ennek segítségével az állapotot módosító (Int -> Int) függvényt.
runOps :: [Op] -> Int -> Int
runOps ops = execState (evalOps ops)


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
