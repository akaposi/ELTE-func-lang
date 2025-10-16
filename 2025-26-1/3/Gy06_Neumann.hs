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

homeDirOf :: String -> Environment -> Maybe String
homeDirOf = _

-- Definiáljuk egy függvényt amely lekéri az összes admin felhasználó home directoryját
-- Listafüggvényeket a demonstráció kedvéért ne használjuk és segédfüggvényt ne írjunk
-- A rekurzív hivás során elemeket a környezetből, mert, mivel nincsen kimenet, ezért ez *nincsen reflektálva az eredményben*

getAdminHomes :: Environment -> [String]
getAdminHomes = _

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
getAdminHomesR = _

-- A Reader monádnak az alábbi két művelete van
-- ask: lekérdezi a környezetet
-- local: lokálisan megváltoztatja

ask' :: Reader r r
ask' = _

local' :: (r -> r) -> Reader r a -> Reader r a
local' = _

-- Írjuk meg a két fenti függvényt ask-al és local-al

homeDirOfRM :: String -> Reader Environment (Maybe String)
homeDirOfRM = _

getAdminHomesRM :: Reader Environment [String]
getAdminHomesRM = _

-- Extra feladatok

-- Definiáljuk a labelWith függvényt readerrel

labelListR :: Num i => [a] -> Reader i [(i,a)]
labelListR = _

-- Definiáljuk a sum függvényt úgy, hogy az olvasási környezetben van a részösszeg

sumTRR :: Num a => [a] -> Reader a a
sumTRR = _

-- Definiáljuk a filterWithIndex függvényt amely index alapján is szűr

filterWithIndexR :: Num i => (i -> a -> Bool) -> Reader i [a]
filterWithIndexR = _

-- Definiáljuk a foldl függvényt readerrel

foldlR :: (b -> a -> b) -> [a] -> Reader b b
foldlR = _

-- WRITER

-- Tegyük fel, hogy QA developerek vagyunk és tesztelni akarjuk, hogy a kódunknak milyen code coverage van.
-- Minden függvényt egy UUID-val trackelünk
type UUID = String

-- Megváltoztatjuk az aritmetikai függvényeinket, hogy gyűjtsék össze a kimenetben, milyen függvényhívások történtek
-- Ha az API request "localhost" adjunk vissza True-t különben False-ot. Függetlenül ettől loggoljuk az "apirequest" UUID-t
apiRequest :: String -> (Bool, [UUID])
apiRequest = _

-- Definiáljuk az alábbi függvényt, amely ha páros számot kap paraméterül akkor a "1.0.0.1" címre küld egy API requestet, ha nem, akkor nem csinál semmit.
-- Függetlenül ettől loggoljuk a "nameserver" UUID-t.
-- Eredményül adjuk vissza a szám felét.
nameserver :: Int -> (Int, [UUID])
nameserver = _

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
nameserverW = _

-- A writernek egy számunkra releváns primitív művelete van:

tell' :: w -> Writer w ()
tell' = _

-- Advancedabb használathoz ld listen és pass
-- Definiáljuk a fenti két függvény tell segítségével

apiRequestWD :: String -> Writer [UUID] Bool
apiRequestWD = _

nameserverWD :: Int -> Writer [UUID] Int
nameserverWD = _

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
hashIntList = _

-- Hasítsunk tetszőleges hajtogatható tárolót
hashFoldable :: Foldable f => f Int -> Writer Hash ()
hashFoldable = _
