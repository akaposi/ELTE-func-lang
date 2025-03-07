{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Gyak04 where
import Control.Monad

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
magicFunction a = case incrementIfEven a of
  Just x -> case combineThrees (*) a x of
    Just y -> case incrementIfEven y of
      Just z -> incrementIfEven (z * 13)
      Nothing -> Nothing
    Nothing -> Nothing
  Nothing -> Nothing

helper :: Maybe a -> (a -> Maybe b) -> Maybe b
helper Nothing f = Nothing
helper (Just a) f = f a

magicFunction' :: Integral a => a -> Maybe a
magicFunction' a =
  incrementIfEven a `helper`
  (\x -> combineThrees (*) a x `helper`
  (\y -> incrementIfEven y `helper`
  (\z -> incrementIfEven (z * 13))))

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

magicFunctionM :: Integral a => a -> Maybe a
magicFunctionM a =
  incrementIfEven a >>= \x -> combineThrees (*) a x >>= \y -> incrementIfEven y

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
magicFunctionDo a = do
  x <- incrementIfEven a
  y <- combineThrees (*) a x
  z <- incrementIfEven y
  incrementIfEven (13 * z)

-- Monád példa: IO monád
-- "IO a" egy olyan "a" típusú értéket jelent, amelyhez valami I/O műveletet kell elvégezni, pl konzolról olvasás
{-
getLine :: IO String
putStrLn :: String -> IO ()
                         ^ A 'void' megfelelője imperatív nyelvekből, egy olyan típus amelynek pontosan 1 irreleváns eleme van
readLn :: Read a => IO a
print :: Show a => a -> IO ()
-}

f1 :: IO ()
f1 = getLine >>= \line -> putStrLn line

f2 :: IO ()
f2 = readLn >>= \valami -> print (valami + 1)

-- Az ilyen ()-ba (ún unitba) visszatérő műveleteknél hasznos a >> művelet
-- m1 >> m2 = m1 >>= \_ -> m2
--                    ^ eredmény irreleváns, csak fusson le

-- Írjunk olyan IO műveleteket do notációval és bindokkal amely
-- a, beolvas két sort és a konkatenációjuk kiírja
-- b, beolvas egy számot és kiírja a négyzetét
-- c, kiírja egy lista összes elemét
-- d, beolvas egy számot minden listaelemhez és azt hozzáadja

readAndConcat :: IO ()
readAndConcat = undefined

readAndConcat' :: IO ()
readAndConcat' = do
  l1 <- getLine
  l2 <- getLine
  let l = l1 ++ l2
  putStrLn l

readAndSq :: IO ()
readAndSq = do
  x <- readLn
  print (x * x)

readAndSq' :: IO ()
readAndSq' = readLn >>= \szam -> print (szam * szam)

printAll :: Show a => [a] -> IO ()
printAll [] = return ()
printAll (x : xs) = print x >> printAll xs -- print x >>= \_ -> printAll xs

printAll' :: Show a => [a] -> IO ()
printAll' = undefined

readAndAdd :: (Read a, Num a) => [a] -> IO [a]
readAndAdd [] = return []
readAndAdd (x : xs) = do
  l <- readLn
  maradék <- readAndAdd xs
  return ((x + l) : maradék)

readAndAdd' :: (Read a, Num a) => [a] -> IO [a]
readAndAdd' [] = return []
readAndAdd' (x : xs) = readLn >>= \l -> readAndAdd xs >>= \maradék -> return ((x + l) : maradék)
