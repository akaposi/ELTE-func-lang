{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Gyak04 where
import Control.Monad

data InsaneType f a b = I1 (f a) (f b) | I2 a (InsaneType f a b) b | I3 (InsaneType f a b) (f b) | I4 b b a deriving Eq


instance (Foldable f) => Foldable (InsaneType f fixed) where

    foldMap :: (Foldable f, Monoid m) => (a -> m) -> InsaneType f fixed a -> m
    foldMap f (I1 ffix fb) = foldMap f fb
    foldMap f (I2 fix it b) = foldMap f it <> f b
    foldMap f (I3 it fb) = foldMap f it <> foldMap f fb
    foldMap f (I4 b1 b2 fix) = f b1 <> f b2
  
    foldr :: Foldable f => (a -> b -> b) -> b -> InsaneType f fixed a -> b
    foldr f n (I1 ffix fb) = foldr f n fb
    foldr f n (I2 fix it b) = foldr f (f b n) it
    foldr f n (I3 it fb) = foldr f (foldr f n fb) it
    foldr f n (I4 b1 b2 fix) = f b1 (f b2 n)

-- Probléma:
-- Tfh van sok, például Maybe a-ba képző függvényünk:

incrementIfEven :: Integral a => a -> Maybe a
incrementIfEven x
  | even x = Just (x + 1)
  | otherwise = Nothing

combineThrees :: Integral a => (a -> a -> a) -> a -> a -> Maybe a
combineThrees f x y
  | (x + y) `mod` 3 == 0 = Just (f x y)
  | otherwise = Nothing

-- Hogyan tudnánk egy olyan függvényt leírni, ami egy számot kap paraméterül
-- erre meghívja az incrementIfEvent, majd ha az Just-ot ad vissza, annak az eredményét
-- és az eredeti számra alkalmazza a combineThrees függvényt a (*) függvénnyel?
-- Pl.:
-- magicFunction 4 == Just 20 (incrementIfEven 4 == 5, 4 + 5 `mod` 3 == 0, 4 * 5 == 20)
-- magicFunction 3 == Nothing (incrementIfEven 3 == Nothing)
-- magicFunction 2 == Nothing (incrementIfEven 2 == 3, 2 + 3 `mod` 3 /= 0)

magicFunction :: Integral a => a -> Maybe a
magicFunction x = case incrementIfEven x of
  Just y -> case combineThrees (*) x y of
    Just z -> incrementIfEven z
    Nothing -> Nothing
  Nothing -> Nothing

-- Ez még egy darab Maybe vizsgálatnál annyira nem vészes, de ha sokat kell, elég sok boilerplate kódot vezethet be
-- Az úgynevezett "mellékhatást" (tehát ha egy számítás az eredményen kívül valami mást is csinál, Maybe esetén a művelet elromolhat)
-- Erre a megoldás a Monád típusosztály
{-
:i Monad
type Monad :: (* -> *) -> Constraint
class Functor m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  {-# MINIMAL (>>=), return #-}
-}
-- A >>= (ún bind) művelet modellezi egy előző "mellékhatásos" számítás eredményének a felhasználását.
-- Maybe esetén (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--                                   ^ csak akkot fut le ha az első paraméter Just a

-- Bind, nagyon hasonít a függvény applikációra ($)
-- (>>=) :: m a -> (a -> m b) -> m b
-- ($)   ::   a -> (a ->   b) ->   b
-- TODO : Miért

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe (Just a) f = f a
bindMaybe Nothing _ = Nothing

magicFunctionM :: Integral a => a -> Maybe a
magicFunctionM x = incrementIfEven x 
  >>= \a -> combineThrees (-) a x
  >>= incrementIfEven

-- Így lehet több olyan műveletet komponálni, amelyeknek vannak mellékhatásaik
-- Akinek nem tetszik a >>= irogatás létezik az imperatív stílusú do notáció
{-
do
   x <- y
   a
===
y >>= \x -> a
-}

magicFunctionDo :: Integral a => a -> Maybe a
magicFunctionDo x = do
  y <- incrementIfEven x
  z <- combineThrees (*) x y
  incrementIfEven z


-- Monád példa: IO monád
-- "IO a" egy olyan "a" típusú értéket jelent, amelyhez valami I/O műveletet kell elvégezni, pl konzolról olvasás
{-
getLine :: IO String
putStrLn :: String -> IO ()
                         ^ A 'void' megfelelője imperatív nyelvekből, egy olyan típus amelynek pontosan 1 irreleváns eleme van
readLn :: Read a => IO a
print :: Show a => a -> IO ()
-}


-- IO esetén abstrakt módon kell gondolni a >>=-ra
-- IO a    = Egy 'a' típusú érték aminek az értéke egy I/O számítás eredménye
-- f >>= g = Kiszámítja az f IO műveletet és átadja g-nek eredményül
-- Gyakorlatban ugye nem ez történik, tehát nincsen IO a -> a művelet
-- A >>= mindig IO környezetben tartja az értékeket: A monádoktól nem lehet megszabadulni
-- Ez garantálja hogy IO művelet tiszta környezetben nincs


-- Az ilyen ()-ba (ún unitba) visszatérő műveleteknél hasznos a >> művelet
-- m1 >> m2 = m1 >>= \_ -> m2
--                    ^ eredmény irreleváns, csak fusson le

-- Írjunk olyan IO műveleteket do notációval és bindokkal amely
-- a, beolvas két sort és a konkatenációjuk kiírja
-- b, beolvas egy számot és kiírja a négyzetét
-- c, kiírja egy lista összes elemét
-- d, beolvas egy számot minden listaelemhez és azt hozzáadja

readAndConcat :: IO ()
readAndConcat = do
  s1 <- getLine
  s2 <- getLine
  putStrLn (s1 ++ s2)

readAndConcat' :: IO ()
readAndConcat' = getLine >>= \s1 -> getLine >>= \s2 -> putStrLn (s1 ++ s2)

readAndSq :: IO ()
readAndSq = do
  n <- readLn :: IO Integer
  print (n * n)

readAndSq' :: IO ()
readAndSq' = (readLn :: IO Integer) >>= \n -> print (n * n)

printAll :: Show a => [a] -> IO ()
printAll [] = return ()
printAll (x : xs) = print x >> printAll xs

printAll' :: Show a => [a] -> IO ()
printAll' [] = return ()
printAll' (x : xs) = do
  print x
  printAll' xs

--n db sort beolvas és listába gyűjti
readThatMany :: Int -> IO [String]
readThatMany 0 = return []
readThatMany n = do
  l <- getLine --readLn :: IO String
  ls <- readThatMany (n - 1)
  return (l : ls)

readThatMany' :: Int -> IO [String]
readThatMany' 0 = return []
readThatMany' n = getLine >>= \l -> readThatMany (n - 1) 
                          >>= \ls -> return (l : ls)

readAndAdd :: forall a. (Read a, Num a) => [a] -> IO [a]
readAndAdd [] = return []
readAndAdd (x : xs) = do
  n <- readLn :: IO a
  ns <- readAndAdd xs
  return (n + x : ns)

readAndAdd' :: forall a. (Read a, Num a) => [a] -> IO [a]
readAndAdd' [] = return []
readAndAdd' (x : xs) = (readLn :: IO a) >>= \n -> readAndAdd' xs 
  >>= \ns -> return (n + x : ns)

-- Olvassunk be egy X számot majd olvassunk be X darab számot és adjuk azokat vissza egy listába
-- pl.: readNums
-- > 2
-- > 13
-- > 1
-- [13, 1]
readNums :: IO [Int]
readNums = do
  n <- readLn
  readNum n
    where
      readNum :: Int -> IO [Int]
      readNum 0 = return [] 
      readNum n = do
        x <- readLn :: IO Int
        xs <- readNum (n -1)
        return (x : xs)

-- [1,2,3] 4 5 6 === [5,7,9]
-- Micsoda még monád?
-- Pl lista:
{-

[1,2,3] >>= \n -> replicate n n

         >>=  
[
1        ->         [1]                  \ 
2        ->         [2,2]                | => [1,2,2,3,3,3]
3        ->         [3,3,3]              /
]

-}

-- Másik megközelítése a monándak: a join művelet

join' :: Monad m => m (m a) -> m a
join' mma = mma >>= id
