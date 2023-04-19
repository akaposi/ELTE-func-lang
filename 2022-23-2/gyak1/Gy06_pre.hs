{-# LANGUAGE DeriveFunctor, InstanceSigs #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module Gy06_pre where

import Control.Monad
import Control.Applicative
import Data.Array (Ix(range))

-- Mai óra anyaga: Monad műveletek és do-notáció használata.
-- Ismert műveletek:
-- pure  :: Applicative f => a -> f a
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (>>)  :: Monad m => m a -> m b -> m b

-- Ismert IO műveletek
-- getLine :: IO String
-- putStrLn :: String -> IO ()

-- Új IO műveletek
-- readLn :: Read a => IO a - beolvas "tetszőleges" típusú értéket
-- print  :: Show a => a -> IO () -- kiír "tetszőleges" típusú értéket stdout-range


-- Olvassunk be két számot és írjuk ki az összegüket!
printSum, printSum' :: IO ()
printSum = undefined
printSum' = undefined

-- Olvassunk be egy számot és eg stringet, és annyiszor írjuk ki a stringet, amennyi a szám értéke!
printNTimes :: IO ()
printNTimes = undefined

-- Olvassunk be n számot és adjuk őket vissza egy listába!
readN :: Integer -> IO [Integer]
readN = undefined


-- Írjuk ki egy lista összes elemét sorfolytonosan
printAll :: [Integer] -> IO ()
printAll = undefined


-- Olvassunk be a lista összes eleméhez egy számot és adjuk hozzá!
addToEach :: [Integer] -> IO [Integer]
addToEach = undefined

-- Ezekre az utóbbi műveletekre van két hasznos függvény már megírva:
-- mapM  :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM_ :: Monad m => (a -> m b) -> [a] -> m () -- ha az eredmény nem számít
-- Írjuk át a printAll és addToEach függvényeket ezekkel!

-- Szekvenciálisan futassunk le egy listányi IO műveletet
runIOs :: [IO a] -> IO [a]
runIOs = undefined

-- Olvassunk be a lista összes eleméhez egy stringet és fűzzük hozzá az elemekhez
concatToStrings :: [String] -> IO [String]
concatToStrings = undefined

-- Egyéb hasznos függvények: replicateM, filterM, replicateM_, forM_, forM, when
-- Érdemes a Control.Applicative és Control.Monad libraryket nézni hoogle-n vagy :bro parancssal

----- Állapot haskellben

-- Mi is az állapotváltozás?
-- Van valami kezdeti 's' állapotunk és valami másik 's' állapotot csinálunk belőle
-- stateChange :: s -> s
-- Esetleg még lehet valami 'a' mellékhatásunk is
-- stateChangeWithEffect :: s -> (s,a)

-- Így modellezünk haskellben állapotváltozást
newtype State s a = State { runState :: s -> (a, s) } deriving Functor
-- Rekord szintaxis: Ez csak annyit jelent hogy
{-
newtype State s a = State (s -> (a,s))

runState :: State s a -> (s -> (a,s))
runState (State f) = f
-}


-- NAGYON FONTOS, A STATE TÍPUS NEM EGY ÁLLAPOT, HANEM AZ ÁLLAPOTVÁLTOZÁS FOLYAMATA

-- Functor, Monád, Applikatív instance-ai vannak a State-nek akit érdekel nézze meg

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a,s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) = ap

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State sa) >>= f = State $ \s -> let (a, s') = sa s in runState (f a) s'


-- A számunkra hasznos műveletek:
-- Az állapot lekérése mellékhatásba:
get :: State s s
get = State $ \s -> (s,s)

-- Az állapot felülírása
put :: s -> State s ()
put s = State $ const ((), s)

-- Az állapot változtatása
modify :: (s -> s) -> State s ()
modify f = get >>= put . f
-- kiírva:
{-
modify f = do
  s <- get
  put (f s)
-}

-- Definiáljunk egy olyan állapotváltozást amely megnöveli az állapotot 1-el
addOne :: State Integer ()
addOne = undefined

-- Definiáljunk egy olyan állapotváltozást amely leszedi egy lista első elemét és visszaadja mellékhatásba!
removeHead :: State [a] (Maybe a)
removeHead = undefined

-- Definiáljunk egy olyan állapotváltozást amely az első kettő elemet szedi le a lista elejéről!
removeHeads :: State [a] (Maybe a, Maybe a)
removeHeads = undefined

-- Definiáljunk egy olyan áll. vált.-t amely leszedi az első K elemet egy listából
takeHeads :: State [a] (Maybe [a])
takeHeads = undefined

-- Definiáljunk egy olyan áll. vált.-t amely megcímzkézi egy lista összes elemét
labelList :: [a] -> State Integer [(Integer, a)]
labelList = undefined

-- Futtassuk le a fenti állapotváltozást 0 kezedeti label-el!
labelList' :: [a] -> [(Integer, a)]
labelList' = undefined


-- Segédműveletek
execState :: State s a -> s -> s
execState st s = let (a, s') = runState st s in s'

evalState :: State s a -> s -> a
evalState st s = let (a, s') = runState st s in a


-- A fenti séma szerint címkézzünk megy egy fát!
-- Csináljuk meg mind preorder, inorder és postorder bejárással
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)

labelTreePRE, labelTreeIN, labelTreePOST :: Tree a -> Tree (Integer, a)
labelTreePRE = undefined
labelTreeIN = undefined
labelTreePOST = undefined

-- Kövi heti +/-: State vagy IO
