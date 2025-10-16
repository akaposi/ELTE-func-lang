{-# LANGUAGE DerivingVia, DerivingStrategies #-}

module Gy06 where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Semigroup
import Data.List

-- Tegyük fel, hogy rendszergazdák vagyunk és Haskellben akarunk permission rendszert írni
-- Ehhez mi el akarjuk tárolni, melyik felhasználónak mi a home directoryja

type User = String
data UserInfo = MkUserInfo { getHomeDirectory :: String, isAdmin :: Bool } deriving (Eq, Show)
type Environment = [(User, UserInfo)] -- nevekhez asszociált home direcory és admin infó

-- A környezet a műveletek során nem változik, ezért kimeneti környezet nem szükséges
-- Definiáljunk egy függvényt ami egy adott nevű felhasználó home directoryját lekéri

-- s -> (a,s)
-- r -> a
homeDirOf :: String -> Environment -> Maybe String
homeDirOf u [] = Nothing
homeDirOf u (x:xs) | u == fst x = Just (getHomeDirectory (snd x))
homeDirOf u (_:xs) = homeDirOf u xs

-- homeDirOf u xs = getHomeDirectory <$> lookup u xs

-- Definiáljuk egy függvényt amely lekéri az összes admin felhasználó home directoryját
-- Listafüggvényeket a demonstráció kedvéért ne használjuk és segédfüggvényt ne írjunk
-- A rekurzív hivás során elemeket a környezetből, mert, mivel nincsen kimenet, ezért ez *nincsen reflektálva az eredményben*

getAdminHomes :: Environment -> [String]
getAdminHomes = undefined

-- A fenti mintákban a környezet továbbadása explicit volt (nekünk kell manuálisan megcsinálni)
-- Viszont egy absztrakciós réteggel át lehet alakítani implicitté:
{-
newtype Reader r a = Reader { runReader :: r -> a }
-}

-- A Reader monádot fog alkotni, tehát használható a bind művelet
-- (>>=) :: Reader a  -> (a -> Reader b) -> Reader b
-- (>>=) :: (r ->  a) -> (a -> r ->   b) -> r ->   b
{-           ^     |      ^    ^      |     |      ^
             |     \------/    |      \-----+------/
             \-----------------+------------/

Az r environment duplikálása és továbbadása mostmár implicit
Hasonlóan mint Statehez, létezik reader függvény
-}

homeDirOfR :: String -> Reader Environment (Maybe String)
homeDirOfR s = reader (homeDirOf s)

getAdminHomesR :: Reader Environment [String]
getAdminHomesR = undefined

-- A Reader monádnak az alábbi két művelete van
-- ask: lekérdezi a környezetet
-- local: lokálisan megváltoztatja

-- reader :: (r -> a) -> Reader r a
ask' :: Reader r r
ask' = reader id

local' :: (r -> r) -> Reader r a -> Reader r a
local' f r = reader (runReader r . f)

-- Írjuk meg a két fenti függvényt ask-al és local-al

homeDirOfRM :: String -> Reader Environment (Maybe String)
homeDirOfRM str = do
  ls <- ask
  case ls of
    [] -> return Nothing
    (x : xs) | fst x == str -> return (Just $ getHomeDirectory $ snd x)
    (_ : xs) -> local (const xs) $ homeDirOfRM str


getAdminHomesRM :: Reader Environment [String]
getAdminHomesRM = undefined

-- Extra feladatok

-- Definiáljuk a labelWith függvényt readerrel

labelListR :: Num i => [a] -> Reader i [(i,a)]
labelListR [] = pure []
labelListR (x : xs) = do
  i <- ask
  xs' <- local (+1) $ labelListR xs
  return ((i, x) : xs')

-- Definiáljuk a sum függvényt úgy, hogy az olvasási környezetben van a részösszeg

sumTRR :: Num a => [a] -> Reader a a
sumTRR [] = ask
sumTRR (x : xs) = local (+x) $ sumTRR xs

-- Definiáljuk a filterWithIndex függvényt amely index alapján is szűr

filterWithIndexR :: Num i => (i -> a -> Bool) -> [a] -> Reader i [a]
filterWithIndexR p [] = pure []
filterWithIndexR p (x:xs) = do
  i <- ask
  xs' <- local (+1) $ filterWithIndexR p xs
  pure $ if p i x then xs' else x : xs'

-- Definiáljuk a foldl függvényt readerrel

foldlR :: (b -> a -> b) -> [a] -> Reader b b
foldlR = undefined

-- WRITER

-- Tegyük fel, hogy QA developerek vagyunk és tesztelni akarjuk, hogy a kódunknak milyen code coverage van.
-- Minden függvényt egy UUID-val trackelünk
type UUID = String

-- Megváltoztatjuk az aritmetikai függvényeinket, hogy gyűjtsék össze a kimenetben, milyen függvényhívások történtek
-- Ha az API request "localhost" adjunk vissza True-t különben False-ot. Függetlenül ettől loggoljuk az "apirequest" UUID-t
apiRequest :: String -> (Bool, [UUID])
apiRequest "localhost" = (True, ["apirequest"])
apiRequest _ = (False, ["apirequest"])

-- Definiáljuk az alábbi függvényt, amely ha páros számot kap paraméterül akkor a "1.0.0.1" címre küld egy API requestet, ha nem, akkor nem csinál semmit.
-- Függetlenül ettől loggoljuk a "nameserver" UUID-t.
-- Eredményül adjuk vissza a szám felét.
nameserver :: Int -> (Int, [UUID])
nameserver n | even n = let uuid = snd (apiRequest "1.0.0.1") in (div n 2, "nameserver" : uuid)
nameserver n = (div n 2, ["nameserver"])


-- A fenti két függvényben a tuple második paramétere, a [UUID] egy kiabsztrahálható réteg.
-- Ez lesz a Writer monád
{-
newtype Writer w a = Writer { runWriter :: (w,a) }
-}
-- A writer monádot fog alkotni, ha a w egy monoid
-- (>>=) :: Writer w a  -> (a -> Writer w b)  -> Writer w b
-- (>>=) ::       (w,a) -> (a ->       (w,b)) -> (w,b)
{-                 | \------^           | |       ^ ^
                   |                    | \-------+-/
                   \-------------------<+>--------/

Mivel valamilyen <+> módon össze kell a két w-t kombinálni, ezért szükséges a Semigroup megkötés. A Monoid megkötés a return miatt kell:
return :: a -> Writer w a
return :: a -> (w,a)
-}

-- Hasonlóan az előzőekhez, létezik writer függvény

apiRequestW :: String -> Writer [UUID] Bool
apiRequestW s = writer (apiRequest s)

nameserverW :: Int -> Writer [UUID] Int
nameserverW i = writer (nameserver i)

-- A writernek egy számunkra releváns primitív művelete van:

tell' :: Monoid w => w -> Writer w ()
tell' w = writer ((), w)

-- Advancedabb használathoz ld listen és pass
-- Definiáljuk a fenti két függvény tell segítségével

apiRequestWD :: String -> Writer [UUID] Bool
apiRequestWD s = do
  tell ["apirequest"]
  return (s == "localhost")

nameserverWD :: Int -> Writer [UUID] Int
nameserverWD = undefined

-- Leggyakrabban lista lesz az írási környezet, de akármilyen Monoid lehet
-- Tegyük fel, hogy hasító függvényt definiálunk és ehhez az alábbi Hash típust definiáljuk

newtype Hash = Hash { unHash :: Integer }
  deriving newtype (Show, Eq, Ord, Num, Real, Integral, Enum) -- Inheritálja az Integer instance implementációit
  deriving Semigroup via Product Integer                      -- Semigroup instance a szorzás alapán
  deriving Monoid    via Product Integer                      -- UA csak Monoiddal

-- A hasító függvény most prímeket fog összeszorozni modulusz nélkül (nem valami effektív, dont use it in production)

-- Sieve of Erasthothenes
primes :: Integral a => [a]
primes = unfoldr (\(x:xs) -> Just (x, filter (\y -> mod y x /= 0) xs)) [2..]

-- Hasítsunk úgy egy integer listát, hogy minden elemével beleindexelünk a prímek listájába és tell-eljük azt a prímet
hashIntList :: [Int] -> Writer Hash ()
hashIntList = undefined

-- Hasítsunk tetszőleges hajtogatható tárolót
hashFoldable :: Foldable f => f Int -> Writer Hash ()
hashFoldable = undefined
