module Gy05 where

import Gy04

-- C# ban pl int.Parse(Console.ReadLine())
-- Read típusosztály
-- readLn :: Read a => IO a
-- print :: Show a => a -> IO ()

-- Minden listaelemhez beolvas egy számot stdin-ról és hozzáadja
readAndAdd :: [Int] -> IO [Int]
readAndAdd [] = return []
readAndAdd (x : xs) = do
  k <- readLn :: IO Int -- Hindley-Milner
  -- x + k
  ks <- readAndAdd xs
  return $ (x + k) : ks


-- get :: State s s
-- put :: s -> State s ()
-- (>>=) :: State s a -> (a -> State s b) -> State s b
-- return :: a -> State s a

-- Tfh [a] az most egy verem
-- Írjunk egy állapotváltozást ami a verembe belerak egy értéket
putInStack :: a -> State [a] ()
putInStack a = do
  xs <- get
  put (a : xs)
  -- modify (a:)

sumViaState :: Num a => [a] -> State a a
sumViaState [] = get
sumViaState (x : xs) = do -- modify (+x) >> sumViaState xs
  y <- get
  put (x + y)
  sumViaState xs

-- kiszedi a verem tetejét ha nem üres
getOutOfStack :: State [a] (Maybe a)
getOutOfStack = do
  xs <- get
  case xs of
    [] -> return Nothing
    (x:xs) -> do
      put xs
      return (Just x)

putAll :: [a] -> State [a] ()
putAll [] = return ()
putAll (x:xs) = do
  stack <- get
  put (x:stack) -- putInStack x
  putAll xs
-- rakja be az összes elemet a verembe
takeK :: Int -> State [a] [a]
takeK k | k < 0 = return []
takeK k | k == 0 = return []
takeK k = do
  y <- getOutOfStack
  case y of
    Nothing -> return []
    Just y -> do
      ys <- takeK (k - 1) -- fmap (\ys -> y:ys) $ takeK (k - 1)
      return (y:ys)
-- szedjen ki K elemet a veremből
-- removeUntil :: Eq a => a -> State [a] [a]
-- addig szedje ki az elemeket a veremből amíg a paraméterül kapott értéket nem találja

mapMonadic :: Monad m => (a -> m b) -> [a] -> m [b]
mapMonadic f [] = return []
mapMonadic f (x : xs) = do
  x' <- f x -- jelenlegi művelet
  xs' <- mapMonadic f xs -- rekurzív hívás
  return (x' : xs')

{-
foreach (int x in xs) {
  Console.WriteLine(x)
}

mapM_ (\x -> print x) xs

-- Olvassunk be egy számot minden listaelemhez és adjuk hozzá
-- most mapM-el
-- almafa :: [Int] -> IO [Int]
-- Szorozzuk be a statet egyessével egy lista elemeivel és adjuk vissza minden lépés után az eredményt
-- foldMultiply :: [Int] -> State Int [Int]
-}
