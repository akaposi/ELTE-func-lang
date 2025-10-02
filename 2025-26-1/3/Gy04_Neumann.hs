module Gy04 where

-- Functor
-- flip fmap :: Functor f => f a -> (a -> b) -> f b 


-- liftA2 :: Functor f => (a -> b -> c) -> f a -> f b -> f c
-- f :: a -> b -> c
-- fa :: f a
-- fmap (f $) fa :: f (b -> c)
-- fb :: fb
-- ap :: f (a -> b) -> f a -> f b
--       ^             ^      ^


-- Applicative
-- Tud "kombinálni" statikus f-eket

-- Statikus hatások típusosztály

-- f a "ez vmi ami a-kat tárol"
--     "ez vmi folyamat, ami a-kat produkálhat"

-- Maybe a = Hibakezelés effektusát fogja reprezentálni
-- List a  = Nemdeterminizmus hatás
-- IO a    = I/O hatás

-- putStrLn :: String -> IO ()
-- getLine  :: IO String
{-
[a -> b]
[a]

[b]
-}

-- beolvasok egy sort, majd kiirom STDOUTra
-- f a -> (a -> f b) -> f b

-- Monád
-- A monád a dinamikus mellékhatás

readAndAdd :: (Num a, Read a) => [a] -> IO [a]
readAndAdd [] = pure []
readAndAdd (x : xs) = readLn >>= \n ->
  readAndAdd xs >>= \ls -> return (n + x : ls)

{-

f >>= \a -> g

===
do

  a <- f
  g

f >> g

===
do
  f
  g

-}

-- writeFile :: FilePath -> String -> IO ()
-- readFile :: FilePath -> IO String

-- CPS = Continuation Passing Style

f :: Int -> Int -> Int
f x y = x + y

cpsf :: forall k. (Int -> k) -> Int -> Int -> k
cpsf continuation x y = continuation (x + y)
