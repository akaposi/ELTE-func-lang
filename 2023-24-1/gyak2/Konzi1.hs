module Konzi1 where

import Control.Monad
-- Monádokról

{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b
-}
-- ez általánosította a map műveletet tetszőleges tárolóra
-- pl.: Fa, Lista, Maybe

-- Képzeljük azt hogy az f valami "mellékhatást" jelent.
-- f a == a típusú érték ami valami f mellékhatás eredménye
-- pl.: IO a = a típusú kifejezés ami valami I/O művelet eredménye
-- Egy ilyen  "f a" mellékhatás eredményét fel tudnánk használni
-- és új mellékhatást építeni belőle.
-- ??? :: f a -> (a -> # f b #) -> f b

-- Monád
-- "Újraépíti a struktúrát"
-- (>>=) :: m a -> (a -> m b) -> m b (bind művelet)
-- return :: a -> m a (tiszta érték mellékhatásos környezetbe való berakása)

-- IO műveletek
-- Haskell tiszta

-- leaveIO :: IO a -> a

{-
getLine :: IO String -- I/O művelet ami stringbe végződik
IO a = RealWorld -> (RealWorld, a)
putStrLn :: String -> IO () -- () = pontosan egy elemű típus (olyan mint a void más nyelvekbe)
-- formális neve unit
putStr :: String -> IO ()
-}

-- Írjunk egy IO műveletet ami kíirja hogy hello world
helloWorld :: IO () -- érdemes eredményt nem ad vissza
helloWorld = putStrLn "Hello World"

-- Monád műveletek megengedik hogy az eredményeit felhasználjuk
-- Olvassunk be egy sort és írjuk azt ki
rAndP :: IO ()
rAndP = getLine >>= \x -> putStrLn x -- éta-redukció \x -> f x = f

-- (>>) ún constans bind
-- f >> g = f >>= \_ -> g

pp :: IO ()
pp = putStr "a" >> putStr "b"

-- Mi az hogy állapotváltozás?
-- Legyen az állapot típusa s
-- Az állapot változás azt jelenti
-- - van egy kezdeti állapot
-- - és abból valami végállapotba képzünk
-- - esetleg valami részeredmény is van
-- s -> (s, a) ahol s az állapot típusa és a a részeredmény/mellékhatás

newtype State s a = State { runState :: s -> (s,a) } deriving Functor

instance Applicative (State s) where
  pure x = State $ \s -> (s,x)
  (<*>) = ap

instance Monad (State s) where
  (State fa) >>= f = State $ \s -> let (s', a') = fa s in runState (f a') s'

-- f $ x = f x

get :: State s s -- olyan állapotváltozás ami részeredményként visszaadja az állapotot
get = State $ \s -> (s,s)

put :: s -> State s () -- felülírja az állapotot
put s = State $ const (s, ())

modify :: (s -> s) -> State s ()
modify f = get >>= \s -> put (f s)

-- do-notáció
{-
do
  x <- y
  z

x <- y
y >>= \x ->
z
z >>
-}

modifyWithDo :: (s -> s) -> State s ()
modifyWithDo f = do
  s <- get -- s := get()
  put (f s)


-- Adjuk össze a listaelmeket state-el
emptySum :: Num a => State [a] a
emptySum = do
  l <- get
  case l of
    [] -> return 0
    (x:xs) -> do
      put xs
      xs' <- emptySum
      return (x + xs')
