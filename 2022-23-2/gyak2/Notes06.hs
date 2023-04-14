
{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Monad

--------------------------------------------------------------------------------
-- Következő feladat: rekurzív IO feladat megint

--------------------------------------------------------------------------------

-- ismételten: beolvas egy sort, kinyomtatja a hosszát
-- getLine :: IO String
-- print   :: Show a => a -> IO ()
-- print   :: Int -> IO ()

f :: IO ()
f = do
  l <- getLine      -- getLine :: IO String
                    -- l :: String
  print (length l)
  f

f' :: IO ()
f' = forever $ do
  l <- getLine
  print (length l)

{-
loop :: a
loop = loop

forever :: Monad m => m a -> m b
forever ma = do
  ma
  forever ma
-}

-- Monád segéd függvények/operátorok

-- (>>) :: Monad m => m a -> m b -> m b       -- "kontans bind"
-- (>>) ma mb = ma >>= \_ -> mb

-- (egymás után végrehajtás)

-- alternatív:
-- (>>) ma mb = do
--   ma
--   mb

{-
forever :: Monad m => m a -> m b
forever ma = ma >> forever ma
-}

-- (=<<) :: Monad m => (a -> m b) -> m a -> m b
-- (=<<) = flip (>>=)


f'' :: IO ()
f'' = forever $
  print . length =<< getLine
  -- length :: [a] -> Int
  -- print  :: Int -> IO ()
  -- print . length :: [a] -> IO ()
  -- print . length =<< getLine :: IO ()

  -- imperatív nyelvben: print(length(getLine()));


--------------------------------------------------------------------------------
-- Írj egy függvényt, ami beolvas egy sort, majd a sort kinyomtatja annyiszor,
-- ahány karakter van a sorban!
io2 :: IO ()
io2 = undefined

-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert. Ha a sorban 'x' van, akkor a program nyomtassa ki az
-- összes eddig beolvasott sort és térjen vissza.
io3 :: IO ()
io3 = goio3 []

-- vegyünk fel egy függvényt, aminek a paraméter a beolvasott sorok
-- listája:

goio3 :: [String] -> IO ()
goio3 lines = do
  l <- getLine
  if elem 'x' l then
    printAllLines (l:lines)
  else
    goio3 (l:lines)

-- hátulról előre nyomtassuk ki az összes sort
printAllLines :: [String] -> IO ()
printAllLines [] = return ()
printAllLines (l:lines) = do
  printAllLines lines
  putStrLn l

goio3' :: [String] -> IO ()
goio3' lines = do
  l <- getLine
  if elem 'x' l then
    mapM_ putStrLn (l:lines)  -- balról jobbra bejárja a listát
  else
    goio3 (l:lines)

-- listákra standard függvények 3 másolatban vannak:

-- tiszta, monádikus, monádikus + visszatérési érték ()

-- map    ::            (a ->   b) -> [a] ->   [b]
-- mapM   :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM_  :: Monad m => (a -> m b) -> [a] -> m ()

-- filter   ::            (a ->   Bool) -> [a] ->   [a]
-- filterM  :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- filterM_  (nincs!)

-- zipWith
-- zipWithM
-- zipWithM_

-- forM = flip mapM
-- forM_ = flip mapM_

-- forM_: for ciklust lehet vele írni

foo :: IO ()
foo = forM_ [0..100] $ \i -> print (i + 20)

foo2 :: IO ()
foo2 = forM_ [0..10] $ \i ->
         forM_ [0..10] $ \j ->
           print (i + j)

-- egymás után hajtsuk végre az összes műveletet egy listában
-- adjuk vissza a visszatérési értékek listáját
-- sequence :: Monad m => [m a] -> m [a]

-- foo3 = sequence [putStrLn "foo", putStrLn "bar]
--      = putStrLn "foo" >> putStrLn "bar

-- foo2 = sequence [print (i + j) | i <- [0..10], j <- [0..10]]


-- A következőt ismételd végtelenül: olvass be egy sort, majd nyomtasd ki a
-- sorban a kisbetűk számát.  A Ctrl-c-c -vel lehet megszakítani a futtatást
-- ghci-ben.
io4 :: IO ()
io4 = forever $ do
  l <- getLine
  print $ length $ filter (\c -> 'a' <= c && c <= 'z') l

--------------------------------------------------------------------------------
-- Definiáld a következő függvényeket tetszőlegesen,
-- de típushelyesen.


f1 :: Monad m => (a -> b) -> m a -> m b
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

-- (newtype: ugyanaz, mint a data, de csak 1 konstruktor + 1 mező lehetséges
--           nincs futásidejű költsége)

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

   -- bemenő állapot ->  (visszatérési érték, kimenő állapot)
   --     s          ->    (a, s)

{-







-}

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  -- return :: a -> State s a
  -- return :: a -> s -> (a, s)         -- nincs hatás: nem változik s
  return a = State (\s -> (a, s))

  -- (>>=) :: State s a -> (a -> State s b) -> State s b
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
  put 20         -- felülírom az értéket
  modify (+100)  -- függvényt alkalmazok
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
push = undefined


-- példák:
-- runState (push 10) [] == ((), [10])
-- runState (push 10 >> push 10 >> push 20) [] == ((), [20, 10, 10])

-- Ha az állapot lista nem üres, akkor a következő függvény leveszi az első
-- elemet és visszaadja Just értékként, egyébként Nothing-ot ad.
pop :: State [a] (Maybe a)
pop = undefined

-- példák:
-- runState pop []        == (Nothing, [])
-- runState pop [0, 1, 2] == (Just 0, [1, 2])


-- Írj egy függvényt, ami egy Int listában minden elemet kicserél az elemtől
-- balra levő elemek maximumára (beleértve az elemet). Legyen az állapot
-- Nothing, ha még egy értéket sem jártunk be, egyébként pedig Just-ban tárolva
-- az eddigi értékek maximuma.

maxs :: [Int] -> State (Maybe Int) [Int]
maxs = undefined

-- példák:
-- evalState (maxs [1, 2, 5, 2]) Nothing == [1, 2, 5, 5]
-- evalState (maxs [10, 5, 12, 3] Nothing) == [10, 10, 12, 12]

-- pop  :: State [a] (Maybe a)
-- push :: a -> State [a] ()

-- Írj egy függvényt, ami kizárólag push, pop és rekurzió felhasználásával
-- map-eli az állapot listát.
mapPushPop :: (a -> a) -> State [a] ()
mapPushPop = undefined

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
replaceLeaves = undefined

-- Definiáld a függvényt, ami megfordítja a fa leveleiben tárolt értékek
-- sorrendjét!  tipp: használhatod a replaceLeaves függvényt.
reverseElems :: Tree a -> Tree a
reverseElems = undefined


-- Írd át a következő függvényeket úgy, hogy *csak* a
-- (State :: (s -> (a, s)) -> State s a) konstruktort használd,
-- monád/funktor instance-t és get/put/modify-t ne használj.
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
