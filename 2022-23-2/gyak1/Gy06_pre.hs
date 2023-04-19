{-# LANGUAGE DeriveFunctor, InstanceSigs, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module Gy06_pre where

import Control.Monad
import Control.Applicative
import Data.List (intercalate)

data DualList a = Cons1 a (DualList a) | Cons2 a a (DualList a) | Nil deriving (Eq, Show)

(+++) :: DualList a -> DualList a -> DualList a
Nil +++ xs = xs
Cons1 a as +++ xs = Cons1 a (as +++ xs)
Cons2 a a' as +++ xs = Cons2 a a' (as +++ xs)

instance Functor DualList where
  fmap :: (a -> b) -> DualList a -> DualList b
  fmap _ Nil = Nil
  fmap f (Cons1 a as) = Cons1 (f a) (fmap f as)
  fmap f (Cons2 a a' as) = Cons2 (f a) (f a') (fmap f as)

instance Applicative DualList where
  pure :: a -> DualList a
  pure a = Cons1 a Nil

  (<*>) :: DualList (a -> b) -> DualList a -> DualList b
  (<*>) = ap

-- Írjunk rá Monád instance-ot! (1 pont)
instance Monad DualList where
  (>>=) :: DualList a -> (a -> DualList b) -> DualList b
  Nil >>= f = Nil
  (Cons1 a as) >>= f = f a +++ (as >>= f)
  (Cons2 a a' as) >>= f = f a +++ (f a' +++ (as >>= f))

print2n :: Int -> IO ()
print2n n
  | n <= 0 = return () -- pure ()
  | otherwise = do
      str1 <- getLine
      str2 <- getLine
      putStr str1
      putStrLn str2
      print2n (n - 1)


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
printSum = do
  a <- readLn
  b <- readLn
  print (a + b)
printSum' = readLn >>= \i -> readLn >>= \j -> print (i + j)

-- Olvassunk be egy számot és eg stringet, és annyiszor írjuk ki a stringet, amennyi a szám értéke!
printNTimes :: IO ()
printNTimes = do
  i <- readLn
  str <- getLine
  printN i str

printN :: Int -> String -> IO ()
printN n s
  | n <= 0 = pure ()
  | otherwise = do
      putStrLn s
      printN (n - 1) s

-- Olvassunk be n számot és adjuk őket vissza egy listába!
readN :: Integer -> IO [Integer]
readN 0 = pure []
readN n = do -- readLn >>= \i -> (i:) <$> readN (n - 1)
  i <- readLn
  xs <- readN (n - 1)
  pure (i:xs)


-- Írjuk ki egy lista összes elemét sorfolytonosan
printAll :: [Integer] -> IO ()
printAll [] = pure ()
printAll (x:xs) = do
  putStr (show x)
  printAll xs--putStrLn . concat . map show--putStrLn $ intercalate "" $ fmap show $ xs



-- Olvassunk be a lista összes eleméhez egy számot és adjuk hozzá!
addToEach :: [Integer] -> IO [Integer]
addToEach [] = pure []
addToEach (x:xs) = do
  y <- readLn
  ys <- addToEach xs
  pure (x + y : ys)


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
addOne = do -- modify (+1)
  i <- get
  put (i + 1)

-- Definiáljunk egy olyan állapotváltozást amely leszedi egy lista első elemét és visszaadja mellékhatásba!
removeHead :: State [a] (Maybe a)
removeHead = do
  i <- get
  case i of
    [] -> pure Nothing
    (x:xs) -> do
      put xs
      pure (Just x)

-- Definiáljunk egy olyan állapotváltozást amely az első kettő elemet szedi le a lista elejéről!
removeHeads :: State [a] (Maybe a, Maybe a)
removeHeads = do
  i <- get
  case i of
    [] -> pure (Nothing, Nothing)
    [x] -> do
      put []
      pure (Just x, Nothing)
    (x:y:xs) -> do
      put xs
      pure (Just x, Just y)

{-
do
  x <- removeHead
  y <- removeHead
  pure (x, y)
-}

-- Definiáljunk egy olyan áll. vált.-t amely leszedi az első K elemet egy listából
takeHeads :: Int -> State [a] (Maybe [a])
takeHeads k
  | k <= 0 = pure (Just [])
  | otherwise = do
  i <- get
  case i of
    [] -> pure Nothing
    (x:xs) -> do
      put xs
      m <- takeHeads (k - 1)
      case m of
        Nothing -> pure (Just [x])
        Just y -> pure (Just $ x:y)


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
data Tree a = Leaf a | Node (Tree a) a  (Tree a) deriving (Eq, Show, Functor, Foldable, Traversable)

labelTreePRE, labelTreeIN, labelTreePOST :: Tree a -> Tree (Integer, a)
labelTreePRE = undefined
labelTreeIN = undefined
labelTreePOST = undefined

-- Kövi heti +/-: State vagy IO
