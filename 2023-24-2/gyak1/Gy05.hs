module Gy05 where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Identity

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

sudo :: Reader Env () -> Reader Env ()
sudo doas = do
  MkEnv homeDir adm <- ask
  when adm $ local (const (MkEnv "/root" True)) doas

-- Feladatok

-- Írjunk egy olyan map függvényt reader segítségével, amely az egyes listaelemek indexével is összekombinálja az elemeket.
-- Az olvasási környezetben tároljuk a jelenlegi indexet
mapWithIndex :: Integral i => (i -> a -> b) -> [a] -> [b]
mapWithIndex f xs = runReader (mwiReader f xs) 0

mwiReader :: Integral i => (i -> a -> b) -> [a] -> Reader i [b]
mwiReader f [] = pure $ []
mwiReader f (x:xs) = do
  ind <- ask
  ls <- local (+1) (mwiReader f xs)
  pure $  (f ind x) : ls

-- >>> mapWithIndex (\i a -> (i, a)) [1..10]
-- [(0,1),(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9),(9,10)]

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

-- >>> runWriter calculation
-- (3,["1-esel kezd\252nk","Azt\225n egy 2-es","Majd az \246sszeg","Egyszerre t\246bbet is tud loggolni"])

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

-- >>> runWriter calculation2
-- (4,["Na v\225gjunk bele",
-- "1-esel kezd\252nk","Azt\225n egy 2-es","Majd az \246sszeg","Egyszerre t\246bbet is tud loggolni",
--"Elhagyjuk a r\233ssz\225mol\225s els\337 \252zenet\233t","A t\246bbit report\225ljuk",
--"Azt\225n egy 2-es","Majd az \246sszeg","Egyszerre t\246bbet is tud loggolni","Majd eredm\233ny + 1"])

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

-- >>> tryDiv 10 5
-- Except (Right 2)

-- >>> tryDiv 8 0
-- ExceptT (Identity (Left "0-val val\243 oszt\225s"))

--                                v ha hiba történik, ezt lefuttatja
-- catchError :: Except e a -> (e -> Except e a) -> Except e a
-- catchError (Except (Left e)) f = f e
-- catchError x _ = x

runCalc :: Except String Int
runCalc = catchError (tryDiv 1 0) $ \_ -> return 11

-- >>> (\a -> case a of Right x -> x; Left _ -> 0) $ runExcept $ runCalc
-- 11

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

f :: Except String (Reader Int (Writer [String] Double))
f = do
  pure $ do
    pure $ do
      pure $ 10.0

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

-- >>> :i Identity
-- type Identity :: * -> *
-- newtype Identity a = Identity {runIdentity :: a}
--   	-- Defined in ‘Data.Functor.Identity’
-- instance Floating a => Floating (Identity a)
--   -- Defined in ‘Data.Functor.Identity’
-- instance Fractional a => Fractional (Identity a)
--   -- Defined in ‘Data.Functor.Identity’
-- instance RealFloat a => RealFloat (Identity a)
--   -- Defined in ‘Data.Functor.Identity’
-- instance RealFrac a => RealFrac (Identity a)
--   -- Defined in ‘Data.Functor.Identity’
-- instance Monoid a => Monoid (Identity a)
--   -- Defined in ‘Data.Functor.Identity’
-- instance Semigroup a => Semigroup (Identity a)
--   -- Defined in ‘Data.Functor.Identity’
-- instance Integral a => Integral (Identity a)
--   -- Defined in ‘Data.Functor.Identity’
-- instance Num a => Num (Identity a)
--   -- Defined in ‘Data.Functor.Identity’
-- instance Real a => Real (Identity a)
--   -- Defined in ‘Data.Functor.Identity’
-- instance Bounded a => Bounded (Identity a)
--   -- Defined in ‘Data.Functor.Identity’
-- instance Enum a => Enum (Identity a)
--   -- Defined in ‘Data.Functor.Identity’
-- instance Applicative Identity -- Defined in ‘Data.Functor.Identity’
-- instance Eq a => Eq (Identity a)
--   -- Defined in ‘Data.Functor.Identity’
-- instance Foldable Identity -- Defined in ‘Data.Functor.Identity’
-- instance Functor Identity -- Defined in ‘Data.Functor.Identity’
-- instance Monad Identity -- Defined in ‘Data.Functor.Identity’
-- instance Ord a => Ord (Identity a)
--   -- Defined in ‘Data.Functor.Identity’
-- instance Read a => Read (Identity a)
--   -- Defined in ‘Data.Functor.Identity’
-- instance Show a => Show (Identity a)
--   -- Defined in ‘Data.Functor.Identity’
-- instance Traversable Identity -- Defined in ‘Data.Traversable’


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

-- >>> runningAdminCheck
-- (True,["Admin status:","True"])

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

--- >>> runMe

-- Fontos hibakezelésnél! Nem mindegy melyik helyen van az ExceptT!
orderMatters :: (MonadError String m, MonadWriter [String] m) => m ()
orderMatters = do
  tell ["Ez lehet elveszik"]
  throwError "Elveszett a masik?"
  tell ["Ez tuti elveszik"]

-- >>> orderMatters :: WriterT [String] (Except String) ()
-- WriterT (ExceptT (Identity (Left "Elveszett a masik?")))

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
