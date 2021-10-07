{-# LANGUAGE DeriveFunctor #-}

import Control.Monad

-- IO monád
--------------------------------------------------------------------------------

-- getLine  :: IO String             -- beolvas
-- print    :: Show a => a -> IO ()  -- kinyomtat értéket
-- putStrLn :: String -> IO ()       -- String-et nyomtat ki

-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- return :: a -> IO a

-- Írj egy függvényt, ami beolvas egy sort, majd a sort kinyomtatja annyiszor,
-- ahány karakter van a sorban!
io2 :: IO ()
io2 = undefined

-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert. Ha a sorban 'x' van, akkor a program nyomtassa ki az
-- összes eddig beolvasott sort és térjen vissza.
io3 :: IO ()
io3 = undefined


-- Olvass be két sort, nyomtasd ki közülük a rövidebbet.
io4 :: IO ()
io4 = undefined


--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen, de típushelyesen.
-- return, (>>=), fmap
-- (alternatív: do-notáció)
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


-- definiálj egy függvényt, ami a lista állapotot kiegészít egy elemmel
push :: a -> State [a] ()
push a = undefined

-- runState (push 10) [] == ((), [10])

-- ha az állapot lista nem üres, akkor a következő függvény leveszi az első elemet
-- és visszaadja Just értékként, egyébként Nothing-ot ad.
pop :: State [a] (Maybe a)
pop = undefined

-- runState pop [0, 1, 2] == (Just 0, [1, 2])
-- runState pop []        == (Nothing, [])


-- Írj egy függvényt, ami kizárólag push, pop és rekurzió felhasználásával map-eli az állapot listát.
mapPP :: (a -> a) -> State [a] ()
mapPP = undefined

-- execState (mapPP (+10)) [0, 1, 2] == [10, 11, 12]


-- Írj egy függvényt, ami egy Int listában minden elemet kicserél az addigi
-- elemek maximumára.  Tegyük fel, hogy a lista nem-negatív számokat tartalmaz.
-- Használd a (State Int)-et. Legyen az állapot a jelenlegi maximális Int.
maxs :: [Int] -> [Int]
maxs = undefined


data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Show)

-- Definiálj egy függvényt, ami kicsérli egy fa leveleiben tárolt értékeket
-- balról jobbra haladva egy megadott lista elemeire.
-- Használj State monádot!

-- pl: replaceLeaves [10, 20, 30] (Node (Leaf 2) (Leaf 3)) == Node (Leaf 10) (Leaf 20)
--     replacereplaceLeaves [5] (Leaf 10) == Leaf 5
--     replacereplaceLeaves [5] (Node (Leaf 0) (Node (Leaf 0) (Leaf 0))) ==
--        (Node (Leaf 5) (Node (Leaf 0) (Leaf 0)))


replaceLeaves :: [a] -> Tree a -> Tree a
replaceLeaves = undefined


-- Definiáld a függvényt, ami megfordítja a fa leveleiben tárolt értékek
-- sorrendjét!  tipp: használhatod a replaceLeaves függvényt.
reverseElems :: Tree a -> Tree a
reverseElems = undefined


-- Írd át a következő függvényeket úgy, hogy *csak* a State
-- konstruktort használd, monád/funktor instance-t és get/put/modify-t
-- ne használj.
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
