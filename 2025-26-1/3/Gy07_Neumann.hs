module Gy07 where

import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State
import Control.Monad
import Data.List


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
-}

-- Ezek a mellékhatások magukban annyira nem hasznosak
-- Akkor lennének erősebbek, ha többet tudnánk egyszerre használni
-- Ez a technológia lesz az ún "Monád transzformerek"
-- Az alábbi módon változtatjuk meg az ismert 4 monádot
{-

newtype State  s a = State  { runState  :: s -> (a,s) } ==> newtype StateT  s m a = StateT  { runStateT  :: s -> m (a, s) }
newtype Reader r a = Reader { runReader :: r -> a }     ==> newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a  }
newtype Writer w a = Writer { runWriter :: (a, w) }     ==> newtype WriterT s m a = WriterT { runWriterT :: m (a, w) }
newtype Except e a = Except { runExcept :: Either e a } ==> newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

Mindenhol az eredményt egy tetszőleges monádba csomagoljuk, így tudjuk nestelni a mellékhatásokat
A standard libraryben lévő primitív műveletek a nestelt monádokat is megtalálják
-}

-- Vegyük az Env típust korábbi gyakokról
data Env = MkEnv { isAdmin :: Bool, homeDir :: String } deriving (Eq, Show)

-- Definiáljunk egy függvényt, amely kiírja a felhasználó home directoryját
-- ha a felhasználó admin
printHomeDirIfAdmin :: ReaderT Env (Writer [String]) ()
printHomeDirIfAdmin = do
  MkEnv isAdmin hd <- ask
  when isAdmin $ tell [hd]

-- Miért typecheckel a tell, ask stb
-- :t ask
-- Minden transzformerhez tartozik egy típusosztály, aminek akkor van instance-a ha a monád stack tartalmazza a transzformert
-- Ezekkel a típusosztályokkal is fel lehet írni a függvényt

printHomeDirIfAdmin' :: (MonadReader Env m, MonadWriter [String] m) => m ()
printHomeDirIfAdmin' = do
  MkEnv isAdmin hd <- ask
  when isAdmin $ tell [hd]


-- Vizsgán ennél a feladatnál mindenkinek a saját típusszignatúráját fel kell majd írnia
-- Lehet mindkettőt használni

-- Vegyük az alábbi típusszinonímát
type FileSystem = [String]
-- pl.:
basicExecutables :: FileSystem
basicExecutables = ["/usr/bin/bash", "/usr/bin/ls", "/bin/sh"]

-- Feladat:
-- Legyen egy FileSystem típusú állapotváltozási környezetünk
-- Legyen egy Env típusú olvasási környezetünk
-- Rakjuk be a felhasználó home directoryját a fájl rendszerbe ha még nincs benne
addHomeIfNotIn :: StateT FileSystem (Reader Env) ()
addHomeIfNotIn = do
  fs <- get
  MkEnv _ hd <- ask
  unless (hd `elem` fs) $ modify (hd:)


-- Legyen egy FileSystem típusú állapotváltozási környezetünk
-- Legyen egy [String] típusú írási környezetünk
-- Töröljük ki a duplikált fájlokat a fájlrendszerből és azokat írjuk ki az írási környezetbe
undupe :: StateT FileSystem (Writer [String]) ()
undupe = undefined


data FSError = FileExists | NotAnAdmin | BadPath deriving (Eq, Show)
-- Legyen egy FileSystem típusú állapotváltozási környezetünk
-- Legyen egy FSError típusú hibakezelési környezetünk
-- Legyen egy Env típusú olvasási környezetünk
-- A függvény várjon egy útvonalat paraméterül. Ha a felhasználó nem amin, dobjunk NotAnAdmin hibát, illetve ha a fájl létezik dobjunk FileExists hibát
-- Ha nem létezik rakjuk be a fájlrendszerbe
tryAdd :: String -> StateT FileSystem (ExceptT FSError (Reader Env)) ()
tryAdd s = do
  MkEnv a _ <- ask
  unless a $ throwError NotAnAdmin
  fs <- get
  unless (s `elem` fs) $ throwError FileExists
  modify (s:)


-- ExceptT-vel vigyázzunk!!!
what :: (MonadError String m, MonadWriter [String] m) => m ()
what = do
  tell ["Ez ki lesz irva?"]
  throwError "Hiba!"
  tell ["Na es ez?"]

-- A monád stack sorrendje számít
-- Vizsgán csak olyan monád stack lesz, amelyben az Except a legkülső, tehát ha hiba van, minden más is meghal


-- IO a stackben
-- Az IO monádot is bele lehet varázsolni a stackbe, de csak a legaljára
-- Mivel minden transzformer monádot vár paraméterül ezért az IO-t is át lehet adni
getAndPrint :: WriterT [String] IO ()
getAndPrint = do
  x <- liftIO getLine
  tell [x]
  -- az hogy x <- getLine nem typecheckel, mivel getLine :: IO String
  -- erre van egy szuper liftIO nevű függvény, amely egy transzformerben le tud futtatni IO függvényt

-- a liftIO típusából látszódik, hogy az IO-t megkötéssel is le lehet írni
-- :t lifIO :: MonadIO m => IO a -> m a
getAndPrint' :: (MonadWriter [String] m, MonadIO m) => m ()
getAndPrint' = undefined

-- Lényegében MonadX azt jelenti, hogy m valami Monad, aminek X képességei is vannak

-- A további feladatban az alábbi környezettel dolgozunk
-- Env típusú olvasási környezet
-- FSError típusú hibakezelési környezet
-- FileSystem típusú állapotkezelési környezet
-- [String] típusú írási környezet
-- IO környezet
-- Vizsgán általában csak 4 lesz az 5-ből

splitOn x = map tail . groupBy (/=) . (x:)

-- Olvassunk be konzolrol egy stringet
-- ha valid útvonal (/-ekkel elváálasztott nem üres stringek) akkor adjuk hozzá a fájlrendszerhez
-- ha nem az dobjunk BadPath hibát
f1 :: type_signature_goes_here
f1 = undefined


-- Futtassuk le az f1-et
-- Ha hibát dob írjuk ki az írási környezetbe, hogy nem sikerült, majd próbáljuk az f1-et újra
-- Eredményként adjuk vissza, hányadik próbálkozásra sikerült a felhasználónak valid útvonalat adnia
f2 :: type_signature_goes_here
f2 = undefined

-- Menjünk végig az összes fájlrendszerbeli elemen
-- Kérdezzük meg a felhasználót (azaz írjuk ki a konzolra) hogy meg szeretné-e tartani azt a fájlt
-- Ha igennel válaszol vagy az a felhasználó home könyvtára hadjuk bent a listában, ha nem töröljük ki
-- Az írási környezetbe írjuk ki a kitörölt fájlokat
f3 :: type_signature_goes_here
f3 = undefined


-- Egyéb érdekes transzformerek
-- Megszakítási környezet: ContT transzformer
-- Akkumulációs környezet: AccumT transzformer
