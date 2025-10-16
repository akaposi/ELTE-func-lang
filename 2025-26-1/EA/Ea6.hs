{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module EA.Ea6 where
import Control.Monad.State
import Control.Monad.Reader
import qualified Control.Monad.Trans as T
import Control.Monad.Writer
import Control.Monad.Cont

-- Előadás
-- Reader monád
-- newtype Reader r a = Reader { runReader :: r -> a }
-- State monád
-- newtype State s a = State { runState :: s -> (a,s) }
-- Except monád
-- newtype Except e a = Except { runExcept :: Either e a }
-- Writer monád
-- newtype Writer w a = Writer { runWriter :: (a, w) }

-- Négy db monádot
-- Magukban ezek a monádok annyira nem hasznosak
-- Hogyan tudnánk többet együtt használni?

-- Első kísérlet

f :: State Int (Reader [Int] Int)
f = do
  x <- get -- tök jól működik
  --y <- ask -- ask :: Reader r r
  return $ do
    y <- ask
    --x <- get -- a stateből már kiléptem
    return 0

-- Második kísérlet
-- Injectáljunk más típusokat Higher-Kinded Polymorphism

-- newtype State s a = State { runState :: s -> (a,s) }
-- INJECT
-- newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
-- m :: * -> *

ask' :: Reader r r
ask' = ask

f' :: StateT Int (Reader [Int]) Int
f' = do
  x <- get
  -- y <- ask'
  StateT $ \_ -> do
      y <- ask
      return (1, 1)
  return 0

-- Félsiker
-- Jó lenne ha tudnánk egy szinten kezelni őket
-- Ad-hoc polymorphism to the rescue
-- MonadTrans
{-
type MonadTrans :: ((* -> *) -> * -> *) -> Constraint
class (forall (m :: * -> *). Monad m => Monad (t m)) =>
      MonadTrans t where
  lift :: Monad m => m a -> t m a
  {-# MINIMAL lift #-}
-}


f'' :: StateT Int (Reader [Int]) Int
f'' = do
  x <- get
  -- y <- ask'
  lift ask' 
  return 0

-- Következő
-- get, put, tell, local, stb konkrét típusokra van megírva
-- Jó lenne ha ezeket is ki tudnánk általánosítani
-- Ad-hoc polymorphism to the rescue, again
-- MonadReader, MonadWriter, MonadError, MonadState

f''' :: StateT Int (Reader [Int]) Int
f''' = do
  x <- get
  ask 
  return 0

-- Tudjuk kombinálni az összes menő effektünket
-- IO mindig a legbelső
-- StateT s IO a
-- IO esetén kell liftet használni
-- MonadIO-val lehet liftIO-t használni

g :: StateT s IO ()
g = lift $ putStrLn "hello world" -- vagy liftIO


-- Ha van nekünk egy olyan monád stackünk, hogy
-- m (n (k ...)) a
-- és létezik runMT, runNT és runKT
-- (runKT (runNT (runMT ...) ...) ...)
-- runKT ... . runNT ... . runMT ...

-- Példa: ELTE DBMS admin vagyok
-- Az adatbázisom 3 dolgot tud
-- írni/olvasni (State)
-- loggolni (Writer)
-- emailt küldeni (IO)

type ElteDBMS a = StateT [Int] (WriterT [String] IO) a

elteAdatbázis :: ElteDBMS Int
elteAdatbázis = do
  x <- get
  tell (map show x)
  liftIO $ putStrLn "Kifogytál a disk quotádból (mind az 500 MB-ból)"
  return 404

runElteDBMS :: ElteDBMS a -> [Int] -> IO ((a, [Int]), [String])
runElteDBMS eltedbms intl = runWriterT (runStateT eltedbms intl)

-- Milyen más monádok vannak még?
-- transformers / mtl
-- transformers = StateT, ReaderT stb
-- mtl = MonadState, MonadReader stb

-- Pl.: Cont, Accum, RWS, Select
-- Accum = State felülírás nélkül
-- RWS = Reader + Writer + State
-- Cont =


-- Sidetrack: CPS
-- Tail/Vég rekurzió: Az utolsó instrukció mindig a rekurzív hivás
-- Tail-call optimization
-- CPS = az utolsó instrukció mindig a folytatás / continuation

add :: Int -> Int -> Int
add x y = x + y

cps_add :: forall k. (Int -> k) -> Int -> Int -> k
cps_add cont x y = cont (x + y)


malloc :: IO Int
malloc = undefined

free :: Int -> IO ()
free = undefined

{-

do
...
  x <- malloc
  ...
  free x

-}
-- bracketing

malloc' :: (Int -> IO a) -> IO a
malloc' cont = do
  ptr <- malloc
  r <- cont ptr
  free ptr
  return r

-- using C#-ban
{-

using (Stream a = ...) {
   ....
}


using Stream a = ...;

-}

h :: ContT Int IO ()
h = do
  x <- ContT malloc'
  _

{-

  x <- ContT x'
  y <- ContT y'


  x' (\x ->
     y' (\y ->
        ...
     )
  )



-}
