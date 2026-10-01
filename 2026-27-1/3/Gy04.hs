{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
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
-- és az eredeti számra alkalmazza a combineThrees függvényt a (*) függvénnyel, majd ismét
-- az incrementIfEven függvényt?
-- Pl.:
-- magicFunction 4 == Just 21 (incrementIfEven 4 == 5, 4 + 5 `mod` 3 == 0, 4 * 5 == 20, incrementIfEven 20 == 21)
-- magicFunction 3 == Nothing (incrementIfEven 3 == Nothing)
-- magicFunction 2 == Nothing (incrementIfEven 2 == 3, 2 + 3 `mod` 3 /= 0)

magicFunction :: Integral a => a -> Maybe a
magicFunction a = case incrementIfEven a of
  Just b -> case combineThrees (*) a b of
    Just c -> incrementIfEven c
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

magicFunctionM :: Integral a => a -> Maybe a
magicFunctionM x = incrementIfEven x >>= \b -> combineThrees (*) x b >>= \c -> incrementIfEven c

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
  b <- incrementIfEven a
  c <- combineThrees (*) a b
  let x = 3
  incrementIfEven c


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
-- a, beolvas harmat sort és a forditotjuk konkatenaciojat kiírja
-- b, beolvas egy számot és kiírja a négyzetét
-- c, kiírja egy lista összes elemét
-- d, beolvas egy számot minden listaelemhez és azt hozzáadja

readAndConcat :: IO ()
readAndConcat = do
  line1 <- getLine
  line2 <- getLine
  line3 <- getLine
  putStrLn $ concat (reverse <$> [line1, line2, line3])

readAndConcat' :: IO ()
readAndConcat' = getLine >>= \line1 -> getLine >>= \line2 -> getLine >>= \line3 ->  putStrLn $ concat (reverse <$> [line1, line2, line3])

readAndSq :: IO ()
readAndSq = do
 num <- readLn :: IO Int
 print (num ^ 2)

readAndSq' :: IO ()
readAndSq' = (readLn :: IO Int) >>= \n -> print (n ^ 2)

printAll :: Show a => [a] -> IO ()
printAll [] = return ()
printAll (x:xs) = print x >> printAll xs 

printAll' :: Show a => [a] -> IO ()
printAll' [] = return ()
printAll' (x:xs) = do
  print x
  printAll xs

readAndAdd :: forall a. (Read a, Num a) => [a] -> IO [a]
readAndAdd [] = return []
readAndAdd (x:xs) = do
  n <- readLn :: IO a
  ns <- readAndAdd xs
  return (n + x : ns)

readAndAdd' :: forall a. (Read a, Num a) => [a] -> IO [a]
readAndAdd' [] = return []
readAndAdd' (x:xs) = (readLn :: IO a) >>= \num -> readAndAdd xs >>= \nl -> return (num + x : nl)
--                                                (\nl -> num + x : nl) <$> readAndAdd xs
--                                                readAndAdd xs <&> (\nl -> num + x : nl)

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
join' m = m >>= id 

