module Gy07 where

import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State -- !!!
import Data.List
import Control.Monad
import Language.Haskell.TH (inferR)
import Data.Char
import GHC.Real (infinity)

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
  MkEnv admin hd <- ask
  if admin then tell [hd] else return ()

-- Miért typecheckel a tell, ask stb
-- :t ask
-- Minden transzformerhez tartozik egy típusosztály, aminek akkor van instance-a ha a monád stack tartalmazza a transzformert
-- Ezekkel a típusosztályokkal is fel lehet írni a függvényt

printHomeDirIfAdmin' :: (MonadReader Env m, MonadWriter [String] m) => m ()
printHomeDirIfAdmin' = do
  MkEnv admin hd <- ask
  if admin then tell [hd] else return ()

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
-- ReaderT Env (State FileSystem) ()
-- (MonadReader Env m, MonadState FileSystem m) => m ()
addHomeIfNotIn = do
  MkEnv _ home <- ask
  fs <- get
  if (home `notElem` fs) then put (home : fs) else return ()

-- Legyen egy FileSystem típusú állapotváltozási környezetünk
-- Legyen egy [String] típusú írási környezetünk
-- Töröljük ki az első 3 fájlt a rendszerből és írjuk ki az írási környezetbe
removeFst3 :: StateT FileSystem (Writer [String]) ()
removeFst3 = do
  env <- get
  case env of
    [] -> tell [] >> put []
    [x] -> tell [x] >> put []
    [x,y] -> tell [x,y] >> put []
    (x:y:z:xs) -> tell [x,y,z] >> put xs

{-
removeFst3 = do
  xs <- get
  tell (take 3 xs)
  put (drop 3 xs)
-}

-- Töröljük ki a duplikált fájlokat a fájlrendszerből és azokat írjuk ki az írási környezetbe
-- elem :: Eq a => a -> [a] -> Bool
-- filter :: (a -> Bool) -> [a] -> Bool
-- nub :: Eq a => [a] -> [a]
-- (\\) :: Eq a => [a] -> [a] -> [a]
undupe :: StateT FileSystem (Writer [String]) ()
undupe = do
  env <- get
  tell (env \\ nub env)
  put (nub env)

{-
undupe = do
  env <- get
  case env of
    [] -> return ()
    (x:xs) | x `elem` xs -> do
       tell [x]
       put (filter (/= x) xs)
       undupe
       modify (x:)
    (x:xs) -> do
       put xs
       undupe
       modify (x:)
-}

data FSError = FileExists | NotAnAdmin | BadPath deriving (Eq, Show)
-- Legyen egy FileSystem típusú állapotváltozási környezetünk
-- Legyen egy FSError típusú hibakezelési környezetünk
-- Legyen egy Env típusú olvasási környezetünk
-- A függvény várjon egy útvonalat paraméterül. Ha a felhasználó nem admin, dobjunk NotAnAdmin hibát, illetve ha a fájl létezik dobjunk FileExists hibát
-- Ha nem létezik rakjuk be a fájlrendszerbe
tryAdd :: String -> StateT FileSystem (ExceptT FSError (Reader Env)) ()
tryAdd path = do
  MkEnv admin _ <- ask
  unless admin $ throwError NotAnAdmin
  env <- get
  when (path `elem` env) $ throwError FileExists
  put (path : env)


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

type MyMonad a = ReaderT Env (StateT FileSystem (ExceptT FSError (WriterT [String] IO))) a

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = map tail . groupBy (/=) . (x:)


-- Olvassunk be konzolrol egy a konzolról egy stringet
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


-- (1 pont)
type Calc a = StateT Int IO a
type CalcC m = (MonadState Int m, MonadIO m)

runCalc m = runStateT m 0

-- (1 pont)
infinityCalc :: Calc () -- CalcC m => m (), StateT Int IO (), (MonadState Int m, MonadIO m) => m ()
infinityCalc = do
  line <- liftIO getLine
  case line of
    "print" -> do
      x <- get
      liftIO $ print x
    ('+' : ' ' : xs) -> do
      modify (+ read xs)
    ('-' : ' ' : xs) -> do
      modify (flip (-) (read xs))
    xs | all isDigit xs -> do
      put (read xs)
    _ -> pure ()
  infinityCalc


