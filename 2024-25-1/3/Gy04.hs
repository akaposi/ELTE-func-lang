{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Gy04 where
import Control.Monad

-- Láttunk példákat például arra hogy a hibákat, expliciten kezeljük (errors as values, (Go, ...))
-- Viszont még nem próübáltuk ki tényleges nagy programokban

-- Probléma:
-- Tfh van sok, például Maybe a-ba képző függvényünk:


{-
  Java, C#-ban lehetne egy Exception, vagy semmit nem csinálunk az értékkel ha páratlan ...
-}
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
magicFunction n = case incrementIfEven n of
  { Just n' -> combineThrees (*) n n' 
  ; Nothing -> Nothing}

{-
  case incrementIfEven n of
    Just n' -> combineThrees (*) n n'
    Nothing -> Nothing 
-}

{-
  let Just n' = incrementIfEven n in 
  combineThrees (*) n n'
-}

{-

Bigger example : 
Adatbázis query ( query :: ... -> Either Error Result ) Either-be tér vissza, csináljunk több query-t egymás után

res = query "select * from table1"
first = {find first element}
res' = query "select" ++ first ++ " from table2"
...

Itt minden res helyett case of olni kéne, redundens (Error e -> Error e) ágakkal

-}

-- Ez még egy darab Maybe vizsgálatnál annyira nem vészes, de ha sokat kell, elég sok boilerplate kódot vezethet be
-- Az úgynevezett "mellékhatást" (tehát ha egy számítás az eredményen kívül valami mást is csinál, Maybe esetén a művelet elromolhat)
-- Erre a megoldás a Monád típusosztály
{-
:i Monad
type Monad :: (* -> *) -> Constraint
class Functor m => Monad m where         -- Functor > Monad
  (>>=) :: m a -> (a -> m b) -> m b      -- Ez lesz a fontos a (>>=) 
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
-- Lehet úgy nézni mintha függvény applikáció a Monádban

magicFunctionM :: Integral a => a -> Maybe a
magicFunctionM n = (incrementIfEven n) >>= \n' -> combineThrees (*) n n'



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
magicFunctionDo n = do
  n' <- incrementIfEven n
  m <- combineThrees (*) n n'
  return m

return' :: a -> Maybe a
return' = Just

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind m f = case m of
  Just m' -> f m'
  Nothing -> Nothing

-- Monád példa: IO monád
-- "IO a" egy olyan "a" típusú értéket jelent, amelyhez valami I/O műveletet kell elvégezni, pl konzolról olvasás
{-
getLine :: IO String
putStrLn :: String -> IO ()
--                       ^ A 'void' megfelelője imperatív nyelvekből, egy olyan típus amelynek pontosan 1 irreleváns eleme van
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
readAndConcat = do
  s <- getLine
  s' <- getLine
  putStrLn (s ++ s')

readAndConcat' :: IO ()
readAndConcat' =
  getLine >>=
  \s -> getLine >>=
  \s' -> putStrLn (s ++ s')


readAndSq :: IO ()
readAndSq = do
  n <- readLn
  print (n * n)

readAndSq' :: IO ()
readAndSq' = undefined

printAll :: Show a => [a] -> IO ()
printAll [] = putStrLn ""
printAll (x:xs) = do
  print x
  printAll xs

printAll' :: Show a => [a] -> IO ()
printAll' = foldr (\a io -> print a >> io ) (putStrLn "") -- :: (a -> IO () -> IO ()) -> IO () -> [a] -> IO ()

readAndAdd :: (Read a, Num a) => [a] -> IO [a]
readAndAdd [] = return []
readAndAdd (x:xs) = do
  y <- readLn
  xs' <- readAndAdd xs
  return $ (x + y) : xs'

readAndAdd' :: (Read a, Num a) => [a] -> IO [a]
readAndAdd' [] = return []
readAndAdd' (x:xs) = 
  readLn >>= 
  \y   -> readAndAdd xs >>=
  \xs' -> return $ (y + x) : xs'

readAndAddF :: (Read a, Num a) => [a] -> IO [a]
readAndAddF = foldr (\x io -> do {y <- readLn; xs <- io; return $ (x + y) : xs}) (return [])

-- Találki mi a különbség
readAndAddF' :: (Read a, Num a) => [a] -> IO [a]
readAndAddF' = foldr (\x io -> do {xs <- io; y <- readLn; return $ (x + y) : xs}) (return [])

-- Ezeket nem kell megoldani



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