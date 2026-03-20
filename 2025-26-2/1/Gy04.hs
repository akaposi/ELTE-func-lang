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
-- és az eredeti számra alkalmazza a combineThrees függvényt a (*) függvénnyel, majd ismét
-- az incrementIfEven függvényt?
-- Pl.:
-- magicFunction 4 == Just 21 (incrementIfEven 4 == 5, 4 + 5 `mod` 3 == 0, 4 * 5 == 20, incrementIfEven 20 == 21)
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

magicFunctionM :: Integral a => a -> Maybe a
magicFunctionM x =
  incrementIfEven x >>= \y ->
  combineThrees (*) x y >>= \z ->
  incrementIfEven z

{-
magicFunction :: Integral a => a -> Maybe a
magicFunction x = case incrementIfEven x of
  Just y -> case combineThrees (*) x y of
    Just z -> incrementIfEven z
    Nothing -> Nothing
  Nothing -> Nothing
-}

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

f = do
  line <- getLine
  putStrLn line
  putStrLn line
  


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
  line1 <- getLine
  line2 <- getLine
  putStrLn (line1 ++ line2)

readAndConcat' :: IO ()
readAndConcat' =
  getLine >>= \x ->
  getLine >>= \y ->
  putStrLn (x ++ y)

readAndSq :: IO ()
readAndSq = undefined

readAndSq' :: IO ()
readAndSq' = undefined

printAll :: Show a => [a] -> IO ()
printAll [] = return ()
printAll (x:xs) = do
  print x
  printAll xs

printAll' :: Show a => [a] -> IO ()
printAll' = undefined

readAndAdd :: (Read a, Num a) => [a] -> IO [a]
readAndAdd [] = return []
readAndAdd (x:xs) = do
  y <- readLn
  ys <- readAndAdd xs
  case 1 of
    1 -> print 1
    _ -> print 2
  let x = 3
  return $ (x + y) : ys

readAndAdd' :: (Read a, Num a) => [a] -> IO [a]
readAndAdd' [] = return []
readAndAdd' zs@(_:_) = do
	let x = last zs
	let xs = init zs
	y <- readLn
	ys <- readAndAdd' xs
	return (ys ++ [x + y])  
	-- readLn >>= \y -> readAndAdd' xs >>= \ys -> return ((x + y) : ys)

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
join' = undefined
