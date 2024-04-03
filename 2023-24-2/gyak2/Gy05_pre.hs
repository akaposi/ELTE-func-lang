module Gy05 where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad

-- Egyszerű feladat (1 pont)
-- Olvassunk be egy X számot majd olvassunk be X darab számot és adjuk azokat vissza egy listába
-- pl.: readNums
-- > 2
-- > 13
-- > 1
-- [13, 1]
readNums :: IO [Int]
readNums = do
  x <- readLn :: IO Int
  readX x

readX :: Int -> IO [Int]
readX 0 = return []
readX x = do--(readLn :: IO Int) >>= \y -> readX (x - 1) >>= \xs -> return (y : xs)
  y <- readLn :: IO Int
  xs <- readX (x - 1)
  return (y : xs)

-- Nehezebb feladat (2 pont)
-- Címkézzünk meg egy fát (INORDER BEJÁRÁS SZERINT) State-el!
-- Használjunk State-t és monád műveleteket!
-- Az állapot a számláló legyen, a mellékhatás a megcímkézett fa!
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show, Functor)

labelTreeSt :: Tree a -> State Int (Tree (a, Int))
labelTreeSt (Leaf a) = do
  x <- get
  put (x + 1)
  return (Leaf (a, x))
labelTreeSt (Node tr a tr') = do
  tr1 <- labelTreeSt tr
  x <- get
  put (x + 1)
  tr2 <- labelTreeSt tr'
  return (Node tr1 (a, x) tr2)

labelTree :: Tree a -> Tree (a, Int)
labelTree tr = fst (runState (labelTreeSt tr) 0)

adding :: Num a => [a] -> State a a -- állapotváltozás utáni
adding [] = get
adding (x : xs) = do
  modify (+x)
  adding xs

producting :: Num a => [a] -> State a a
producting [] = get
producting (x : xs) = do
  modify (*x)
  producting xs

labelList :: [a] -> State Int [(a, Int)]
labelList [] = return []
labelList (x : xs) = do
  i <- get
  put (i + 1)
  xs' <- labelList xs
  return ((x, i) : xs')


-- A State monád állapot változást reprezentált
-- Vegyünk három új Monádot:


-- Reader: olvasási környezet, például globális konstans
-- newtype Reader r a = Reader { runReader :: r -> a }
--                                                ^ state-nek elhagytuk a kimeneti paraméterét

-- Primitív műveletek

-- ask :: Reader r r
-- ask = Reader id
-- Visszaadja környezetet, ugyanaz mint a get state-nél

data Env = MkEnv { homeDir :: String, isAdmin :: Bool }

canWriteHere :: String -> Reader Env Bool
canWriteHere path = do
  MkEnv homeDir adm <- ask
  return (adm || path == homeDir)

-- local :: (r -> r) -> Reader r a -> Reader r a
-- Lokális megváltoztatja a környezetet a második paraméterben

getHomeDir :: Reader Env String
getHomeDir = do
  MkEnv homeDir isAdmin <- ask
  return homeDir

sudo :: Reader Env a -> Reader Env a
sudo doas = do
  MkEnv homeDir adm <- ask
  local (const (MkEnv "/root" True)) doas

-- Feladatok



-- Írjunk egy olyan map függvényt reader segítségével, amely az egyes listaelemek indexével is összekombinálja az elemeket.
-- Az olvasási környezetben tároljuk a jelenlegi indexet
mapWithIndex :: Integral i => (i -> a -> b) -> [a] -> [b]
mapWithIndex f xs = runReader (mwiReader f xs) 0

mwiReader :: Integral i => (i -> a -> b) -> [a] -> Reader i [b]
mwiReader = undefined


-- Writer: írási környezet, például loggingra hasznos
-- newtype Writer w a = Writer { runWriter :: (a, w) }
--                                           ^ state-nek elhagytuk a bemeneti paraméterét
-- Primitív műveletek

-- tell :: Monoid w => w -> Writer w () -- üzenet írása, a >>= kombinálja az ezzel írt üzeneteket <>-vel
-- tell w = Writer (w, ())
-- ugyanaz mint a put State-nél

calculation :: Writer [String] Int
calculation = do
  tell ["1-esel kezdünk"]
  let x = 1
  tell ["Aztán egy 2-es"]
  let y = 2
  tell ["Majd az összeg", "Egyszerre többet is tud loggolni"]
  return (x + y)

--                         v lefuttatja ezt a writert
-- listen :: Monoid w => Writer w a -> Writer w (a, w) -- és visszaadja a loggolásait
-- listen (Writer res) = Writer (mempty, res)

calculation2 :: Writer [String] Int
calculation2 = do
  tell ["Na vágjunk bele"]
  (res, messages) <- listen calculation
  tell ["Elhagyjuk a résszámolás első üzenetét", "A többit reportáljuk"]
  tell (tail messages)
  tell ["Majd eredmény + 1"]
  return (res + 1)


-- Except: hibakörnyezet, képes hibakezelésre
-- newtype Except e a = Except { runExcept :: Either e a }
--                ^ az "e" hibát jelez, ha valaha Left e lesz belül, a bind-ok több műveletet nem tud elvégezni, tehát megáll az egész hamarabb

-- Primitív műveletek

-- throwError :: e -> Except e a
-- throwError e = Except (Left e)
-- hibát dob

tryDiv :: Int -> Int -> Except String Int
tryDiv x y = do
  when (y == 0) $ throwError "0-val való osztás"
  return (div x y)


--                                v ha hiba történik, ezt lefuttatja
-- catchError :: Except e a -> (e -> Except e a) -> Except e a
-- catchError (Except (Left e)) f = f e
-- catchError x _ = x

runCalc :: Except String Int
runCalc = catchError (tryDiv 1 0) $ \_ -> return 11

-- READER
-- Ha a felhasználó home directoryja root, adjunk vissza
-- true-t egyébként false-ot
isInRoot :: Reader Env Bool
isInRoot = undefined

-- Megmondja hogy az összes útvonalra tud-e írni a felhasználó (akkor tud írni ha az a homedir vagy root a felh)
hasAccessToAll :: [String] -> Reader Env Bool
hasAccessToAll = undefined

-- WRITER
-- Szorozzunk össze két számot és az írási környezetbe írjuk be, ha valamelyik szám 0
mulAndLog :: Int -> Int -> Writer [String] Int
mulAndLog = undefined

-- EXCEPT
-- Összeszoroz egy listányi számot, hibát dob ha valamelyik szám 0
mulAll :: [Int] -> Except String Int
mulAll = undefined


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
-}
readerAndExcept :: ExceptT String (Reader Int) Int
readerAndExcept = do
  i <- ask
  when (i == 0) $ throwError "i == 0"
  return i

readerAndExcept' :: (MonadError String m, MonadReader Int m) => m Int
readerAndExcept' = do
  i <- ask
  when (i == 0) $ throwError "i == 0"
  return i

h :: Either String Int
h = runReader (runExceptT readerAndExcept) 1

{-
Mindenhol az eredményt egy tetszőleges monádba csomagoljuk, így tudjuk nestelni a mellékhatásokat
A standard libraryben lévő primitív műveletek a nestelt monádokat is megtalálják
-}


--                         v itt azért nem WriterT van, mert több mellékhatást nem akarunk belerakni
adminCheck :: ReaderT Env (Writer [String]) Bool
adminCheck = do
  MkEnv _ admin <- ask
  tell ["Admin status:", show admin]
  return admin

-- futtatás: belülről kifele
runningAdminCheck :: (Bool, [String])
runningAdminCheck = runWriter (
                    runReaderT adminCheck (MkEnv "/home/root" True)
                    )

-- Alternatív felírás: típusosztályokkal, egy monádról megkötjük mikre képes
adminCheck' :: (MonadReader Env m, MonadWriter [String] m) => m Bool
adminCheck' = do
  MkEnv _ admin <- ask
  tell ["Admin status:", show admin]
  return admin

-- Bónusz: IO-t be lehet rakni monad stack legaljára, vagy a MonadIO megkötést használva
-- Ekkor a liftIO műveletet lehet használni
logInput :: WriterT [String] IO ()
logInput = do
  result <- liftIO getLine
  tell [result]

runMe :: IO ()
runMe = runWriterT logInput >>= print

--- READER ÉS WRITERT
-- Kiolvassa az értéket az olvasási könryezetből és kiírja X-szer az írási környeztebe
readAndPrintX :: Int -> WriterT [String] (Reader a) [a]
readAndPrintX x = do
  a <- ask
  case x of
    0 -> return []
    x -> do
      xs <- readAndPrintX (x - 1)
      return (a : xs)

--- STATE ÉS IO
--- Addig olvas számokat STDIN-ról amíg az összegük egy páros szám nem lesz-e
--- Ezt a számot a state környezetbe tároljuk el
readUntilEven :: (MonadIO m, MonadState Int m) => m Int
readUntilEven = do
  sval <- get
  case (sval `mod` 2) of
    0 -> return sval
    _ -> do
      num <- liftIO (readLn :: IO Int)
      modify (+ num)
      readUntilEven




logInput' :: (MonadWriter [String] m, MonadIO m) => m ()
logInput' = do
  result <- liftIO getLine
  tell [result]



-- Fontos hibakezelésnél! Nem mindegy melyik helyen van az ExceptT!
orderMatters :: (MonadError String m, MonadWriter [String] m) => m ()
orderMatters = do
  tell ["Ez lehet elveszik"]
  throwError "Elveszett a masik?"
  tell ["Ez tuti elveszik"]

o1 :: Either String ((), [String])
o1 = runExcept (runWriterT orderMatters)

o2 :: (Either String (), [String])
o2 = runWriter (runExceptT orderMatters)

-- Általában emiatt mindig az except ami utoljára van futtatva.

-- Feladatok
-- Szimuláljunk egy egyszerű bejelentkezési rendszert
-- a, Egy State monádban tároljuk el kik a felhasználók nevét ([String])
-- b, Egy Reader monádban tároljuk el a jelenlegi felhasználó nevét (String)
-- c, Egy Writer monádban írjuk ha egy felhasználó bejelentkezik ([String])
-- d, Egy Except monáddal kezeljük, ha nem létező felhasználó akar belépni

-- Definiáljuk a createNewUser függvényt, amely egy új felhasználót hozzáad a rendszerhez
-- Definiáljuk a login függvényt amely a jelenlegi felhasználó nevével megpróbál belépni
-- Definiáljuk a tryLoginAs függvényt, amely paraméterül kap egy felhasználónevet, azzal megpróbál belépni, és ha az sikertelen ezt kiírja a writerbe (ne hasaljon el)

type MonadLogin m = (MonadState [String] m, MonadReader String m, MonadWriter [String] m, MonadError String m)
type Login a = StateT [String] (ReaderT String (WriterT [String] (Except String))) a

createNewUser :: String -> Login ()
createNewUser newUser = modify (newUser :)

login :: MonadLogin m => m ()
login = do
  usrname <- ask
  tell [usrname ++ " be akar jelentkezni"]
  users <- get
  if usrname `elem` users then tell [usrname ++ " sikeresen bejelentkezett"] else throwError "Nincs ilyen felhasználó"
