{-# LANGUAGE DeriveFunctor #-}
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
  (Just y) -> case combineThrees (*) x y of
    Nothing -> Nothing
    (Just k) -> Just (k ^ 2)

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
--                                   ^ csak akkor fut le ha az első paraméter Just a

-- Bind, nagyon hasonít a függvény applikációra ($)
-- (>>=) :: m a -> (a -> m b) -> m b
-- ($)   ::   a -> (a ->   b) ->   b
-- TODO : Miért

main = print "Hello World!"

magicFunctionM :: Integral a => a -> Maybe a
magicFunctionM x = incrementIfEven x >>= combineThrees (*) x <$> (^ 2)

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
  -- let h = incrementIfEven x
  k <- combineThrees (*) x y
  return (k ^ 2)


-- Monád példa: IO monád
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
readAndConcat = getLine >>= 
  \a -> getLine >>= 
  \b -> putStrLn (a ++ b)

readAndConcat' :: IO ()
readAndConcat' = do
  a <- getLine
  b <- getLine
  putStrLn (a ++ b)

readAndSq :: IO ()
readAndSq = readLn >>= \num -> print (num ^ 2)

readAndSq' :: IO ()
readAndSq' = do
  (num :: Integer) <- readLn
  print (num ^ 2)

printAll :: Show a => [a] -> IO ()
printAll = print

printAll' :: Show a => [a] -> IO ()
printAll' ls = do
  print ls

readAndAdd :: (Read a, Num a) => [a] -> IO [a]
readAndAdd [] = return []
readAndAdd (a : ls) = readLn >>= 
  \num -> readAndAdd ls >>= 
  \nls -> return ((num + a) : nls)

readAndAdd' :: (Read a, Num a) => [a] -> IO [a]
readAndAdd' [] = return []
readAndAdd' (a : ls) = do
  num <- readLn
  nls <- readAndAdd' ls 
  return $ (a + num ) : nls

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
put s = State $ const ((), s) -- State $ \_ -> ((), s)

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
incrementAndEvenBind = get >>= \i -> put (i +1) >> return (even i)

incrementAndEvenDo :: State Int Bool
incrementAndEvenDo = do
  i <- get
  put (i + 1)
  return $ even i

-- runState-el lehet tesztelni, pl runState incrementAndEvenBind 3 == (False, 4)

-- >>> runState incrementAndEvenBind 3 == (False, 4)
-- True

-- Definiáljunk állapotváltozásokat mely
-- a, leszedi az állapotbeli lista fejelemét ha van olyan
-- b, X darab elemet leszed az állapotbeli listából (csak a primitív kombinátorokat és az a,-t használd)
-- c, addig szedi le az elemekeg az állapotbeli listából, amíg egy predikátum igaz (csak a primitív kombinátorokat és az a,-t használd)
-- d, összeadj az állapotbeli lista összes elemét és kiűríti azt (csak a primitív kombinátorokat és az a,-t használd)

behead :: State [a] (Maybe a)
behead = get >>= \ls -> case ls of
  [] -> return Nothing
  (a : ls) -> put ls >> return (Just a)

behead' :: State [a] (Maybe a)
{-
behead' = do
  ls <- get
  case ls of
    [] -> return Nothing
    (a : ls) -> do
      put ls
      return $ Just a
-}

behead' = do {
  ls <- get;
  case ls of
    [] -> return Nothing;
    (a : ls) -> do {
      put ls;
      return $ Just a ;
    }
}

takeFromSt :: Integral i => i -> State [a] [a]
takeFromSt i | i <= 0 = return []
takeFromSt i          = get >>= \ls -> case ls of
  [] -> return []
  (a : ls) -> put ls >>
    takeFromSt (i - 1) >>=
    \nls -> return $ a : nls


takeFromSt' :: Integral i => i -> State [a] [a]
takeFromSt' i | i <= 0 = return []
takeFromSt' i  = do
  ls <- get
  case ls of
    [] -> return []
    (a : ls) -> do
      put ls
      nls <- takeFromSt' (i - 1)
      return $ a : nls

takeWhileFromSt :: (a -> Bool) -> State [a] [a]
takeWhileFromSt p = get >>= \ls -> case ls of
  [] -> return []
  (a : ls) -> case p a of
    False -> return []
    True -> put ls >>
      takeWhileFromSt p >>= 
      \nls -> return $ a : nls

takeWhileFromSt' :: (a -> Bool) -> State [a] [a]
takeWhileFromSt' p = do
  ls <- get
  case ls of
    [] -> return []
    (a : ls) -> case p a of
      False -> return []
      True -> do
        put ls
        nls <- takeWhileFromSt' p
        return $ a : nls

summing :: Num a => State [a] a
summing = get >>= \ls -> case ls of
  [] -> return 0
  (a : ls) -> put ls >>
    summing >>=
    \sum -> return $ a + sum

summing' :: Num a => State [a] a
summing' = do
  ls <- get
  case ls of
    [] -> return 0
    (a : ls) -> do
      put ls
      sum <- summing'
      return $ sum + a


-- Definiáljuk egy fa preorder, postorder és inorder címkézését állapotváltozásokkal
-- Az állapotban a legutoljára kiadott indexet tárolja

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)

preorder :: Tree a -> Tree (a, Int)
preorder a = fst $ flip runState 0 $ do
  _

postorder :: Tree a -> Tree (a, Int)
postorder = undefined

inorder :: Tree a -> Tree (a, Int)
inorder = undefined
