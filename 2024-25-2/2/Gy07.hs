module Gy05 where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.IO.Class

c :: Bool
c = True

a :: Maybe Bool
a = do { 
let b = case c of {True -> False;False -> False} in return True; return True }


{-

 \(a :: Int) -> "asd" + a

-}

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

newtype State  s a = State  { runState  :: s -> (a,s) } ==> newtype StateT  s (m :: * -> *) a = StateT  { runStateT  :: s -> m (a, s) }
newtype Reader r a = Reader { runReader :: r -> a }     ==> newtype ReaderT r m a             = ReaderT { runReaderT :: r -> m a  }
newtype Writer w a = Writer { runWriter :: (a, w) }     ==> newtype WriterT s m a             = WriterT { runWriterT :: m (a, w) }
newtype Except e a = Except { runExcept :: Either e a } ==> newtype ExceptT e m a             = ExceptT { runExceptT :: m (Either e a) }

Mindenhol az eredményt egy tetszőleges monádba csomagoljuk, így tudjuk nestelni a mellékhatásokat
A standard libraryben lévő primitív műveletek a nestelt monádokat is megtalálják
-}

data Env = MkEnv { homeDir :: String, isAdmin :: Bool }

-- "State" ~~ ReaderT s (Writer s) = s -> (_, s)
-- m = Writer [String] = (_, [String])
-- ReaderT Env m Bool = Env -> m Bool = Env -> (Bool, [String])
--                         v itt azért nem WriterT van, mert több mellékhatást nem akarunk belerakni
adminCheck :: 
  ReaderT Env 
  (Writer [String]) 
  Bool
adminCheck = do
  MkEnv _ admin <- ask
  tell ["Admin status:", show admin]
  return admin

-- futtatás: belülről kifele
runningAdminCheck :: (Bool, [String])
runningAdminCheck = 
  let a = runReaderT adminCheck (MkEnv "/home/root" True)
  in runWriter a

-- Alternatív felírás: típusosztályokkal, egy monádról megkötjük mikre képes
adminCheck' :: 
  ( MonadReader Env m
  , MonadWriter [String] m) 
  => m Bool
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

data Errors
  = NoSuchUser
  | NameTaken

-- Feladatok
-- Szimuláljunk egy egyszerű bejelentkezési rendszert

type LoginMonad {- a -} = 
  ExceptT Errors 
  (WriterT [String] 
  (ReaderT String 
  (State [String]))) -- a

-- a, Egy State monádban tároljuk el kik a felhasználók nevét ([String])
-- b, Egy Reader monádban tároljuk el a jelenlegi felhasználó nevét (String)
-- c, Egy Writer monádban írjuk ha egy felhasználó bejelentkezik ([String])
-- d, Egy Except monáddal kezeljük, ha nem létező felhasználó akar belépni

-- Más megoldás
-- :k Logintype'
type Logintype' m a = 
  ( MonadError String m
  , MonadWriter [String] m
  , MonadReader String m
  , MonadState [String] m) => m a 

createNewUser' :: String -> Logintype' m ()
createNewUser' str = undefined

-- :k Logintype
type Logintype m = 
  ( MonadError String m     -- d, Egy Except monáddal kezeljük, ha nem létező felhasználó akar belépni
  , MonadWriter [String] m  -- c, Egy Writer monádban írjuk ha egy felhasználó bejelentkezik ([String])
  , MonadReader String m    -- b, Egy Reader monádban tároljuk el a jelenlegi felhasználó nevét (String)
  , MonadState [String] m   -- a, Egy State monádban tároljuk el kik a felhasználók nevét ([String])
  )

createNewUserC :: (Logintype m) => String -> m ()
createNewUserC = undefined

createNewUser :: String -> LoginMonad ()
createNewUser str = do
  users <- get
  if str `elem` users 
    then throwError NameTaken
    else do
      tell ["Sikeresen hozzáadtuk " ++ str ++ " az adatbázishoz"]
      (modify (str:))


-- Definiáljuk a createNewUser függvényt, amely egy új felhasználót hozzáad a rendszerhez
-- Definiáljuk a login függvényt amely a jelenlegi felhasználó nevével megpróbál belépni
-- Definiáljuk a tryLoginAs függvényt, amely paraméterül kap egy felhasználónevet, azzal megpróbál belépni, és ha az sikertelen ezt kiírja a writerbe (ne hasaljon el)

-- egy megoldás

-- Definiáljuk a login függvényt amely a jelenlegi felhasználó nevével megpróbál belépni
-- (MonadError String m, MonadWriter [String] m,  MonadReader String m, MonadState [String] m) => m ()
login :: (Logintype m) => m ()
login = do
  current_user <- ask
  users <- get
  if current_user `elem` users
    then do
      tell ["A " ++ current_user ++ " nevu felhasznalo bejelentkezett a rendszerbe"]
    else do
      throwError "Nincs ilyen felhasznalo"

-- Definiáljuk a tryLoginAs függvényt, 
-- amely paraméterül kap egy felhasználónevet,
-- azzal megpróbál belépni, 
-- és ha az sikertelen ezt kiírja a writerbe (ne hasaljon el)
tryLoginAs :: (Logintype m) => String -> m ()
tryLoginAs usr = do
  local (const usr) login `catchError` (\e -> tell ["Sikertelen belépés", "Error:", e])



-- lift ::  
-- ∀ (t :: (Type -> Type) -> Type -> Type) -- monadtrafó
--   (m :: Type -> Type)                   -- monád
--   a .                                   -- érték
-- (MonadTrans t, Monad m) => m a -> t m a

-- Lifttel érjük el a többi state-et, de inkább érdemes egybe fuse-olni őket
type TripleState a = StateT Int (StateT [String] (State [Int])) a
type SingleState a = State (Int, [String], [Int]) a

whatIsGet :: TripleState ()
whatIsGet = do
  i <- lift $ lift get
  return ()

test :: TripleState ()
test = do
  t <- get
  -- liftIO getLine
  t' <- lift get
  t'' <- lift $ lift get
  --put [10]
  put 10
  lift $ put ["alma"]
  lift $ lift $ put [10]
  return ()

runTripleState m i ss is = runState (runStateT (runStateT m is) ss) i
-- runTripleState test [] [] 1