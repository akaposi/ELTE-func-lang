{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Gy04_pre where
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
magicFunction k = case incrementIfEven k of
  Nothing -> Nothing
  Just x -> case combineThrees (*) k x of
    Nothing -> Nothing
    Just x -> incrementIfEven x


-- Maybe a -> (a -> Maybe b) -> Maybe b
hf :: Maybe a -> (a -> Maybe b) -> Maybe b
hf Nothing f = Nothing
hf (Just a) f = f a

magicFunction2 :: Integral a => a -> Maybe a
magicFunction2 k = incrementIfEven k `hf` (\x -> combineThrees (*) k x `hf` (\x -> incrementIfEven x))

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
magicFunctionM x = incrementIfEven x >>= \a -> combineThrees (*) x a >>= \l -> incrementIfEven l

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
  a <- incrementIfEven x
  l <- combineThrees (*) a x
  incrementIfEven l

-- Monád példa: IO monád

getAndPutLine :: IO ()
getAndPutLine = do
  x <- getLine
  putStrLn x

-- "IO a" egy olyan "a" típusú értéket jelent, amelyhez valami I/O műveletet kell elvégezni, pl konzolról olvasás
{-
getLine :: IO String
putStrLn :: String -> IO ()
                         ^ A 'void' megfelelője imperatív nyelvekből, egy olyan típus amelynek pontosan 1 irreleváns eleme van
readLn :: Read a => IO a
print :: Show a => a -> IO ()
-}
-- Az ilyen ()-ba (ún unitba) visszatérő műveleteknél hasznos a >> művelet
-- m1 >> m2 = m1 >>= \_ -> m2
--                    ^ eredmény irreleváns, csak fusson le

-- Írjunk olyan IO műveleteket do notációval és bindokkal amely
-- a, beolvas két sort és a konkatenációjuk kiírja
-- b, beolvas egy számot és kiírja a négyzetét
-- c, kiírja egy lista összes elemét
-- d, beolvas egy számot minden listaelemhez és azt hozzáadja

readAndConcat :: IO ()
--              getLine >>= (\line1 -> getLine >>= (\line2 -> putStrLn (line1 ++ line2)))
readAndConcat = getLine >>= \line1 -> getLine >>= \line2 -> putStrLn (line1 ++ line2)

readAndConcat' :: IO ()
readAndConcat' = do
  s1 <- getLine
  s2 <- getLine
  putStrLn (s1 ++ s2)

readAndSq :: IO ()
readAndSq = (readLn) >>= \num -> print (num ^ 2)

readAndSq' :: IO ()
readAndSq' = do
  x <- readLn
  print (x ^ 2)

printAll :: Show a => [a] -> IO ()
printAll [] = return ()
printAll (x : xs) = print x >> printAll xs

printAll' :: Show a => [a] -> IO ()
printAll' [] = return ()
printAll' (x : xs) = do
  print x
  printAll' xs

readAndAdd :: (Read a, Num a) => [a] -> IO [a]
readAndAdd = undefined

readAndAdd' :: (Read a, Num a) => [a] -> IO [a]
readAndAdd' = undefined

-- Monád példa: Állapotváltozás monád (State monád)
--                          v rekord szintaxis, ekvivalens azzal hogy State (s -> (s,a))
newtype State s a = State { runState :: s -> (a, s) } deriving Functor
-- Ezzel nem foglalkozunk még
instance Applicative (State s) where
  pure = return
  (<*>) = ap
-- "State s a" egy s típusú állapot változását (ez az s -> s rész) és egy "a" eredményt reprezentál
-- Példa:
incrementAndEven :: State Int Bool --     v állapotváltozás eredménye, az eredeti állapot megnövelve 1-el
incrementAndEven = State $ \i -> (even i, i + 1)
--                                ^ eredmény: az állapot páros e

-- Primitív állapotváltozások
-- Eredményül visszaadja a jelenlegi állapotot
get :: State s s
get = State $ \s -> (s,s)

-- Felülírja a jelenlegi állapotot
put :: s -> State s ()
put s = State $ const ((), s)

-- runState :: State s a -> s -> (a,s)
-- lefuttat egy állapotváltozást egy adott kezdeti állapotra

-- Monád műveletek
instance Monad (State s) where

  return :: a -> State s a
  return a = State $ \s -> (a,s)

  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State f) >>= g = State $ \s -> let (a, s') = f s in runState (g a) s'

-- Néha a modify-t is primitívnek szokták mondani
modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

-- Minden állapotválotzást megírható >>=, return, get és put segítségével
-- Írjuk meg bindal/do notációval az incrementAndEven állapotváltozást
incrementAndEvenBind :: State Int Bool
incrementAndEvenBind = undefined

incrementAndEvenDo :: State Int Bool
incrementAndEvenDo = undefined

-- runState-el lehet tesztelni, pl runState incrementAndEvenBind 3 == (False, 4)

-- Definiáljunk állapotváltozásokat mely
-- a, leszedi az állapotbeli lista fejelemét ha van olyan
-- b, X darab elemet leszed az állapotbeli listából (csak a primitív kombinátorokat és az a,-t használd)
-- c, addig szedi le az elemekeg az állapotbeli listából, amíg egy predikátum igaz (csak a primitív kombinátorokat és az a,-t használd)
-- d, összeadj az állapotbeli lista összes elemét és kiűríti azt (csak a primitív kombinátorokat és az a,-t használd)

behead :: State [a] (Maybe a)
behead = undefined

takeFromSt :: Integral i => i -> State [a] [a]
takeFromSt = undefined

takeWhileFromSt :: (a -> Bool) -> State [a] [a]
takeWhileFromSt = undefined

summing :: Num a => State [a] a
summing = undefined

-- Definiáljuk egy fa preorder, postorder és inorder címkézését állapotváltozásokkal
-- Az állapotban a legutoljára kiadott indexet tárolja

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)

preorder :: Tree a -> Tree (a, Int)
preorder = undefined

postorder :: Tree a -> Tree (a, Int)
postorder = undefined

inorder :: Tree a -> Tree (a, Int)
inorder = undefined
