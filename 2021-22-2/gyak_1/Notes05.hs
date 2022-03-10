{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Monad

-- köv feladat: IO feladat, iteráció/ciklus van benne

-- canvas feladat
------------------------------------------------------------

canvas :: a -> (a -> Maybe b)
            -> (b -> Maybe c) -> (c -> Maybe d) -> Maybe d
canvas a amb bmc cmd = case amb a of
  Nothing -> Nothing
  Just b -> case bmc b of
    Nothing -> Nothing
    Just c -> cmd c

canvas' :: a -> (a -> Maybe b)
            -> (b -> Maybe c) -> (c -> Maybe d) -> Maybe d
canvas' a amb bmc cmd = do
  b <- amb a
  c <- bmc b
  cmd c

-- IO monád
--------------------------------------------------------------------------------

-- getLine  :: IO String             -- beolvas
-- print    :: Show a => a -> IO ()  -- kinyomtat értéket
-- putStrLn :: String -> IO ()       -- String-et nyomtat ki

-- (>>=)  :: IO a -> (a -> IO b) -> IO b
-- return :: a -> IO a
-- fmap   :: (a -> b) -> IO a -> IO b

-- + do notáció


-- Írj egy függvényt, ami beolvas egy sort, majd visszaadja a sorban az 'a' és
-- 'z' közötti karakterek számát.
io1 :: IO Int
io1 = do
  l <- getLine
  -- let-definíció do-blokkban (nincs "in" a végén)
  let lowerCaseNum = length $ filter (\c -> 'a' <= c && c <= 'z') l
  return lowerCaseNum

io1' :: IO Int
io1' =
  getLine >>= (\l ->
  let lowerCaseNum = length $ filter (\c -> 'a' <= c && c <= 'z') l
  in return lowerCaseNum)

-- tiszta függvényt alkalmazunk a getLine eredményére
io1'' :: IO Int
io1'' = fmap process getLine  where
  process :: String -> Int
  process l = length $ filter (\c -> 'a' <= c && c <= 'z') l

-- Írj egy függvényt, ami beolvas egy sort,
-- majd a sort kinyomtatja annyiszor,
-- ahány karakter van a sorban!
io2 :: IO ()
io2 = do
  l <- getLine          -- getLine :: IO String
                        -- l :: String

  let len = length l    -- tiszta definíció

  let go 0 = return ()
      go n = do
        putStrLn l
        go (n - 1)

  go len

-- replicateM  :: Monad m => Int -> m a -> m [a]
-- replicateM_ :: Monad m => Int -> m a -> m ()

io2' :: IO ()
io2' = do
  l <- getLine
  replicateM_ (length l) (putStrLn l)

-- fmap :: Monad m => (a -> b) -> m a -> m b
-- flip fmap :: Monad m => m a -> (a -> b) -> m b
io2'' :: IO ()
io2'' =
  getLine >>= \l ->
  replicateM_ (length l) (putStrLn l)

-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert. Ha a sorban 'x' van, akkor a program nyomtassa ki az
-- összes eddig beolvasott sort és térjen vissza.
io3 :: IO ()
io3 = do
  let go :: [String] -> IO ()
      go lines = do
        l <- getLine
        if elem 'x' l
          -- then print $ reverse (l:lines)
          then mapM_ putStrLn $ reverse (l:lines)
          else go (l:lines)
  go []

io3' :: IO ()
io3' = do
  l <- getLine
  if elem 'x' l then
    putStrLn l
  else
    io3' >> putStrLn l

  -- else do
  --   io3'
  --   putStrLn l

-- forever :: Monad m => m a -> m b

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

-- return, (>>=), fmap

-- (fmap nélkül próbáld!)
f1 :: Monad m => (a -> b) -> m a -> m b  -- fmap (csak Monad-ra)
f1 f ma = do
  a <- ma
  return (f a)
-- f1 = fmap

f2 :: Monad m => m a -> m b -> m (a, b)
f2 ma mb = do
  a <- ma
  b <- mb
  return (a, b)
  -- (ezt nem lehet fmap-el kiváltani!)
  --   (mivel két paraméteres függvénnyel kellen map-elni)
  --   (Applicative: n-paraméteres fmap általánosítás)

f3 :: Monad m => m (m a) -> m a
f3 mma = do
  ma <- mma
  a <- ma         -- (ma >>= \a -> return a) == ma
  return a

foo = f3 $ do
  putStrLn "hello"
  return (putStrLn "world")

-- Maybe (Maybe a) -> Maybe a
-- f3 (Just ma) = ma
-- f3 Nothing   = Nothing

-- standard:
-- class Functor m => Monad m where
--   return :: a -> m a
--   (>>=)  :: m a -> (a -> m b) -> m b

-- class Functor m => Monad m where
--   return :: a -> m a
--   join   :: m (m a) -> m a

-- extra házi: bind definiálható join-ból és fordítva
--  (vö: (==) definiálható (/=)-ből és fordítva)


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

-- State monád definíció
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


-- Írj egy függvényt, ami egy Int listában minden elemet kicserél az addigi
-- elemek maximumára.  Tegyük fel, hogy a lista nem-negatív számokat tartalmaz.
-- Használd a (State Int)-et. Legyen az állapot a jelenlegi maximális Int.
maxs :: [Int] -> [Int]
maxs = undefined


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
