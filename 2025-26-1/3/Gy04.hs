{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
magicFunction x = case incrementIfEven x of
  Nothing -> Nothing
  Just y  -> case combineThrees (*) x y of
    Nothing -> Nothing
    Just z -> incrementIfEven z

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
bindMaybe Nothing f = Nothing
bindMaybe (Just a) f = f a

magicFunctionM :: Integral a => a -> Maybe a
--                                      | akkor történik meg, ha Just-ba voltunk |
magicFunctionM x = incrementIfEven x
  >>= \y -> combineThrees (*) x y
  >>= \z -> incrementIfEven z

-- Így lehet több olyan műveletet komponálni, amelyeknek vannak mellékhatásaik
-- Akinek nem tetszik a >>= irogatás létezik az imperatív stílusú do notáció
{-
do
   x <- y
   a
===
y >>= \x -> a


do
  x
  y
===
x >> y
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
readAndConcat = getLine >>= \s1 -> getLine >>= \s2 -> putStrLn (s1 ++ s2)

readAndConcat' :: IO ()
readAndConcat' = do
  line1 <- getLine
  line2 <- getLine
  putStrLn (line1 ++ line2)

readAndSq :: IO ()
readAndSq = (readLn :: IO Int) >>= \x -> print (x ^ 2)

readAndSq' :: IO ()
readAndSq' = do
  l <- readLn :: IO Int
  print (l ^ 2)

printAll :: Show a => [a] -> IO ()
printAll [] = return ()
printAll (x : xs) = print x >> printAll xs

printAll' :: Show a => [a] -> IO ()
printAll' [] = return ()
printAll' (x : xs) = do
  print x
  printAll' xs

{-

Kapunk egy számot paraméterül
Olvassunk be annyi db sort a STDIN-ról és gyűjtsük össze egy listába

-}

readThatMany :: Int -> IO [String]
readThatMany 0 = return []
readThatMany x = do
  line <- getLine
  lines <- readThatMany (x - 1)
  return (line : lines)

readAndAdd :: forall a. (Read a, Num a) => [a] -> IO [a]
readAndAdd [] = return []
readAndAdd (x : xs) = do
  y <- readLn :: IO a
  ys <- readAndAdd xs
  return (y + x : ys)

readAndAdd' :: (Read a, Num a) => [a] -> IO [a]
readAndAdd' [] = return []
readAndAdd' (x : xs) = readLn >>= \y -> readAndAdd xs >>= \ys -> return (y + x : ys)
-- BIND
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
