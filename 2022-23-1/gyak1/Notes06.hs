{-# language InstanceSigs, DeriveFunctor #-}

import Control.Monad

-- köv feladat
------------------------------------------------------------
-- IO művelet, amiben lehet rekurzió, lehet szükség segédfüggvényre

-- feladat
------------------------------------------------------------

-- olvassunk be egy sort, és utána olvassunk annyi sort
-- ahány karakter van a beolvasott sorban

help :: Int -> IO ()
help 0 = return ()
help n = do
  l <- getLine
  help (n - 1)

f :: IO ()
f = do
  l <- getLine           -- getLine :: IO String
  let len = length l     -- nincs mellékhatás!  (length l :: Int)
  help len


f' :: IO ()
f' = do
  l <- getLine           -- getLine :: IO String

  let len = length l     -- nincs mellékhatás!  (length l :: Int)

  let help 0 = return ()
      help n = do
        l <- getLine
        help (n - 1)

  help len

-- replicate   :: Int -> a -> [a]
-- replicateM  :: Monad m => Int -> m a -> m [a]
-- replicateM_ :: Monad m => Int -> m a -> m ()

f'' :: IO ()
f'' = do
  l <- getLine
  replicateM_ (length l) getLine

f''' :: IO ()
f''' =
  getLine >>= \l ->
  replicateM_ (length l) getLine


------------------------------------------------------------

-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert. Ha a sorban 'x' van, akkor a program nyomtassa ki az
-- összes eddig beolvasott sort és térjen vissza.

-- tipp 1: elem :: Char -> String -> Bool
-- tipp 2: eddig beolvasott sorokat segédfüggvény paramétereként
--         lehet nyilván tartani [String]


io3 :: IO ()
io3 = go [] where

  go :: [String] -> IO ()
  go lines = do
    l <- getLine
    if elem 'x' l
      then mapM_ putStrLn $ reverse (l:lines)
      -- print (l:lines)
      else do
        go (l:lines)

-- io3' :: IO ()
-- io3' = do
--   l <- getLine
--   if elem 'x' l then do
--     putStrLn l
--   else do
--     io3'
--     putStrLn l

-- A következőt ismételd végtelenül: olvass be egy sort, majd nyomtasd ki a
-- sorban a kisbetűk számát. A Ctrl-c -vel lehet megszakítani a futtatást
-- ghci-ben.
io4 :: IO ()
io4 = forever $ do
  l <- getLine
  print $ length $ filter (\c -> 'a' <= c && c <= 'z') l

io4' :: IO ()
io4' = do
  l <- getLine
  print $ length $ filter (\c -> 'a' <= c && c <= 'z') l
  io4'

-- Definiáld a következő függvényeket tetszőlegesen,
-- de típushelyesen.
f1 :: Monad m => (a -> b) -> m a -> m b -- fmap nélkül definiáld!
f1 = undefined

f2 :: Monad m => m a -> m b -> m (a, b)
f2 = undefined

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

-- Definiálj egy függvényt, ami a lista állapotot kiegészíti egy elemmel
push :: a -> State [a] ()
push a = do
  as <- get
  put (a:as)

push' :: a -> State [a] ()
push' a = modify (a:)

-- példák:
-- runState (push 10) [] == ((), [10])
-- runState (push 10 >> push 10 >> push 20) [] == ((), [20, 10, 10])

-- Ha az állapot lista nem üres, akkor a következő függvény leveszi az első
-- elemet és visszaadja Just értékként, egyébként Nothing-ot ad.
pop :: State [a] (Maybe a)
pop = do
  as <- get
  case as of
    []   -> return Nothing
    a:as -> do
      put as
      return $ Just a


-- példák:
-- runState pop []        == (Nothing, [])
-- runState pop [0, 1, 2] == (Just 0, [1, 2])


-- Írj egy függvényt, ami egy Int listában minden elemet kicserél az elemtől
-- balra levő elemek maximumára (beleértve az elemet). Legyen az állapot
-- Nothing, ha még egy értéket sem jártunk be, egyébként pedig Just-ban tárolva
-- az eddigi értékek maximuma.

maxs :: [Int] -> State (Maybe Int) [Int]
maxs []     = return []
maxs (n:ns) = do
  mn <- get
  let (mn', m) = case mn of
        Nothing -> (Just n, n)
        Just m  -> (Just (max m n), m)
  put mn'
  ns <- maxs ns
  return (m:ns)

  -- case mn of
  --   Nothing -> do
  --     put $ Just n
  --     ns <- maxs ns
  --     return (n:ns)
  --   Just m -> do
  --     put $ Just (max n m)
  --     ns <- maxs ns
  --     return (m:ns)

  -- mn <- get
  -- case mn of
  --   Nothing -> do
  --     put $ Just n
  --     ns <- maxs ns
  --     return (n:ns)
  --   Just m -> do
  --     put $ Just (max n m)
  --     ns <- maxs ns
  --     return (m:ns)



-- példák:
-- evalState (maxs [1, 2, 5, 2]) Nothing == [1, 2, 5, 5]
-- evalState (maxs [10, 5, 12, 3] Nothing) == [10, 10, 12, 12]

-- pop  :: State [a] (Maybe a)
-- push :: a -> State [a] ()

-- Írj egy függvényt, ami kizárólag push, pop és rekurzió felhasználásával
-- map-eli az állapot listát.
mapPushPop :: (a -> a) -> State [a] ()
mapPushPop f = do
  ma <- pop
  case ma of
    Nothing -> return ()
    Just a  -> do
      mapPushPop f
      push (f a)

-- példák:
-- execState (mapPushPop (+10)) [0, 1, 2] == [10, 11, 12]


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
