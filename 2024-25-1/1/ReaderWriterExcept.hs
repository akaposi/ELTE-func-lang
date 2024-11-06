-- Cheatsheet:
{-
+---------------------------+---------------------------------------------+-------------------------------------------------------------+
| Monád                     | Primitív Művelet #1                         | Primitív Művelet #2                                         |
+---------------------------+---------------------------------------------+-------------------------------------------------------------+
| State s a                 | get :: State s s                            | put :: s -> State s ()                                      |
+---------------------------+---------------------------------------------+-------------------------------------------------------------+
| Monoid w => Writer w a    | listen :: Writer w a -> Writer w (a, w)     | tell :: w -> Writer w ()                                    |
+---------------------------+---------------------------------------------+-------------------------------------------------------------+
| Reader r a                | ask :: Reader r r                           | local :: (r -> r) -> Reader r a -> Reader r a               |
+---------------------------+---------------------------------------------+-------------------------------------------------------------+
| Except e a                | throwError :: e -> Except e a               | catchError :: Except e a -> (e -> Except e a) -> Except e a |
+---------------------------+---------------------------------------------+-------------------------------------------------------------+

A writernek még létezik a
pass :: Writer w (a, w -> w)  -> Writer w a
függvénye, amivel meg lehet változtatni egy Writer kimenetét

Hasznos listafüggvények a Data.List modulból:

* partition :: (a -> Bool) -> [a] -> ([a], [a])
- Kettéválasztja a listaelemeket egy predikátum alapján (ld. Quicksort)
- partition p xs = (filter p xs, filter (not . p) xs)

* unfoldr :: (b -> Maybe (a,b)) -> b -> [a]
- Egy seed elemből felépít egy listát
- unfoldr f b
-   | Just (a, bnew) <- f b = a : unfoldr f bnew
-   | otherwise = []

* lookup :: Eq a => a -> [(a,b)] -> Maybe b
- Ha a paramétert tartalmazza a lista, akkor visszaadja a tuple második felét
- lookup key xs
-   | ((_, y):_) <- filter (\(a,b) -> a == key) xs = Just y
-   | otherwise = Nothing

* find :: Foldable f => (a -> Bool) -> f a -> Maybe a
- Visszadja az első lista elemet amire teljesül egy predikátum

* elemIndices :: Eq a => a -> [a] -> [Int]
* elemIndex :: Eq a => a -> [a] -> Maybe Int
- Visszadja azokat az index(eket) ahol az adott elem megtalálható

* splitAt :: Int -> [a] -> ([a], [a])
- Az adott index mentén elválasztja a listát
- splitAt k xs = (take k xs, drop k xs)
-}

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except

import Data.Complex
import Data.List

-- READER FELADATOK

-- Példa feladat: Definiáljuk az attachIndices függvényt amely minden listaelemhez az indexét hozzácsatolja
-- Az olvasási környezetben tároljuk el azt, hogy eddig hány elemet címkéztünk meg:

attachIndicesR :: [a] -> Reader Int [(a, Int)]
attachIndicesR [] = return []
attachIndicesR (x : xs) = do
  i <- ask
  xs' <- local (+1) $ attachIndicesR xs
  return ((x, i) : xs')

attachIndices :: [a] -> [(a, Int)]
attachIndices xs = runReader (attachIndicesR xs) 0

-- Számítsuk ki az n-edik fibbonacci számot, úgy hogy az olvasási környezetben eltároljuk az előző kettő fibonacci számot!

fibTailR :: Integer -> Reader (Integer, Integer) Integer
fibTailR = undefined

fibTail :: Integer -> Integer
fibTail i = runReader (fibTailR i) undefined

-- Definiáljuk a stalinSort algoritmust reader segítségével. Az algoritmus kiszűri azokat az elemeket a listábók, amelyek kisebbek az előttük lévőnél.
-- Táróljuk el az olvasási környezetben az előző listaelemet.

stalinSortR :: Ord a => [a] -> Reader a [a]
stalinSortR = undefined

stalinSort :: Ord a => [a] -> [a]
stalinSort [] = []
stalinSort (x:xs) = runReader (stalinSortR xs) undefined

-- pl.: stalinSort [1,2,3,2,1,4,2,3] == [1,2,3,4]

-- EXCEPT feladatok

-- Példa: Definiáljuk a mandelbrot és listOfMandelbrot függvényeket
-- A mandelbrot függvény végezze el a mandelbrot iterációt és adjuk vissza hány iteráció után robban fel (>= 4 lesz az érték négyzete)
-- Ha 1000 iteráción belül nem robban fel, dobjunk hibát
-- a listOfMandelbrot adja vissza mely értékek robban fel és hány iteráción belül

mandelbrot :: Complex Double -> Except String Integer
mandelbrot c = mbHelper 0 1000
  where
    mbHelper z 0 = throwError "Nem robban fel"
    mbHelper z i
      | n :+ _ <- abs (z ^ 2), n >= 4 = return (1000 - i)
      | otherwise = mbHelper (z ^ 2 + c) (i - 1)

listOfMandelbrot :: [Complex Double] -> Except String [Integer]
listOfMandelbrot [] = return []
listOfMandelbrot (x : xs) = do
  vs <- catchError (singleton <$> mandelbrot x) (\_ -> return [])
  xs' <- listOfMandelbrot xs
  return (vs ++ xs')

-- Definiáljuk a safeDiv függvényt ami biztonságosan oszt!
-- Definiáljuk a divable függvényt amely safeDiv-el elosztja a számokat. Amelyekre az kivételt dob, hadjuk ki az eredményből

safeDiv :: Integer -> Integer -> Except String Integer
safeDiv = undefined

divable :: [(Integer, Integer)] -> Except String [(Integer, Integer, Integer)]
divable = undefined


-- WRITER FELADATOK
-- Példa: Definiáljuk a subsums feladatot, amely összeadja egy lista elemét és az összes részeredményt!

subsumsW :: Num a => [a] -> Writer [a] a
subsumsW [] = return 0
subsumsW (x : xs) = do
  tell [x]
  xs' <- pass $ (\a -> (a, map (+x))) <$> subsumsW xs
  -- Ha a fönti két sort felcseréljük fordítva lesz a lista
  return (x + xs')

subsums :: Num a => [a] -> [a]
subsums xs = execWriter (subsumsW xs)

{-
Alternatív megoldás:
subsumsW :: Num a => [a] -> Writer [a] a
subsumsW [] = 0 <$ tell [0]
subsumsW (x : xs) = do
  tell [x + xs']
  xs' <- subsumsW xs
  return (x +  xs')
-}

-- Definiáljuk a subproducts függvényt amely a részszorzatokat kiszámolja!
subproductsW :: Num a => [a] -> Writer [a] a
subproductsW = undefined

-- Definiáljuk a partitionW függvényt, amely kiírja az írási környezetbe ha teljesül egy feltétel.
-- A számítás eredménye azok az elemek legyenek amelyekre nem teljesül.
partitionW :: (a -> Bool) -> [a] -> Writer [a] [a]
partitionW = undefined

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p s = runWriter $ partitionW p s
