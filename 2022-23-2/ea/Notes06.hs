{-# language
   RankNTypes
  ,TypeApplications
  ,ImpredicativeTypes
  ,KindSignatures
  ,ScopedTypeVariables
  ,ConstraintKinds
  ,AllowAmbiguousTypes
  #-}

import Data.Kind
import Control.Monad.ST
import Data.STRef
import Data.IORef
import Control.Monad.State

-- IO példa, Monad
--
--   vizsgán Monad instance: Maybe, State, IO, Parser
--   további gyakran használt: lista, Reader, Either,
--   további: Continuation, ST, STM

--      Maybe  : hiba
--      State  : 1 db mutable változó (adott típusú)
--      IO     : input-output "sin-bin" (+ GHC runtime system mellékhatásai)
--      Parser : parser-t lehet benne írni
--               f :: newtype Parser a = Parser (String -> Maybe (a, String))
--      lista  : (>>=) = flip concatMap;  return a = [a]  ("nem-determinizmus")
--      Reader : newtype Reader r a = Reader (r -> a)
--                 "r" típusú "konfiguráció"
--                ask :: Reader r r   (implicit módon átadott paraméter)
--      Either : ugyanaz mint a Maybe, csak a Left a hiba
--                 catch :: Either e a -> (e -> Either e a) -> Either e a
                 -- do {.....} `catch` (\error -> ....)
--      Continuation : "callcc" (control flow feature-öket támogat)
--                               (goto, break, continue, yield (coroutine))

--      ST : hasonló az IO-hoz, viszont: csak mutációt lehet benne használni,
--           IO-ban unsafePerform kell lokálisan
--      ST: (kívülről tiszta függvények, ami belül mutációt használhat)
--          IO : nincsen: (IO a -> a)
--          ST-ben: runST :: (forall s. ST s a) -> a
--                           "higher-rank" polimorfizmus

--  id  :: a -> a
--  foo :: (forall a. a -> a) -> Int

------------------------------------------------------------

-- template <typename a> id (x : a)
-- id<A>(x : A) { x; }

id :: forall a. a -> a
id x = x

-- id 100
-- id True

foo :: (forall a. a -> a) -> (Int, Bool)
foo f = (f @Int 100, f @Bool True)         -- id<Bool>(x);

-- bar :: forall a. (a -> a) -> (Int, Bool)
-- bar f = (f 100, f True)

-- foo2 :: forall a. Eq a => (a -> a) -> (Int, Bool)
-- foo2 f = (f 100, f True)

-- ImpredicativeTypes
foo3 :: forall a. a -> Maybe (Eq a => a -> a -> a) -> Int
foo3 = undefined

-- f :: Int -> Int -> (Bool -> Int) -> Int

foo4 :: [forall a. a -> a]
foo4 = [\x -> x, \x -> x]

foo5 = foo3 ((+10) :: Int -> Int)
            (Just (\f g -> if f == g then f else g))

-- foo6 :: Eq (Int -> Int) => (Int -> Int) -> (Int -> Int) -> (Int -> Int)
-- foo6 f g = if f == g then f else g

loop :: forall a. a
loop = loop

foo6 :: [forall a. a]
foo6 = [loop, loop, undefined, undefined, error "foo"]

const' :: forall a b. a -> b -> a
const' x y = x

-- const'' :: forall (f :: * -> *) (a :: *) (b :: *). f a -> f b -> f a
-- const'' x y = x

-- const'' (Just Bool) (Just Bool)

mapTwice :: forall (f :: * -> *) (a :: *). Functor f => (a -> a) -> f a -> f a
mapTwice f fa = fmap g fa where
  g :: a -> a
  g = f . f

-- foo7 :: (forall a. (forall b. _) -> Bool) -> Int

foo7 :: forall (c :: * -> Constraint)(a :: *). c a => a -> a
foo7 x = x

-- runST :: (forall (s :: *). ST s a) -> a
-- newSTRef :: s -> ST s (STRef s a)
-- readSTRef :: STRef s a -> ST s a
-- writeSTRef :: STRef s a -> a -> ST s ()
-- (IORef a)

-- STRef s a   :  mutable referencia, ami "a" típusú értéket tárol
--                s: "régió" megjelöli  (Rust: lifetime paraméter)

-- newtype IO a   = IO (State# RealWorld -> (# a, State# RealWorld #))
-- newtype ST s a = ST (State# s -> (# a, State# s #))

-- foo8 :: forall s. Int -> STRef s Int
-- foo8 x = runST $ do  -- "s"
--   myref <- newSTRef x
--   modifySTRef myref (+1000)
--   val <- readSTRef myref
--   return myref

foo8 :: Int -> Int
foo8 x = runST $ do     -- runST :: (forall s. ST s a) -> a
  myref <- newSTRef x
  _ <- modifySTRef myref (+1000)
  val <- readSTRef myref
  return val

-- id {Bool}
-- id @Bool
-- id = \@a (x :: a) -> x

foo9 :: Int -> IO Int
foo9 x = do
  myref <- newIORef x
  modifyIORef myref (+1000)
  val <- readIORef myref
  return val

------------------------------------------------------------

-- newtype State s a = State {runState :: s -> (a, s)}

-- get :: State s s                      -- olvasás
-- put :: s -> State s ()                -- írás
-- runState :: State s a -> s -> (a, s)  -- futtatás

-- pure a = State (\s -> (a, s))
-- get = State (\s -> (s, s))
-- put s = State (\_ -> ((), s))
-- (>>=) (State f) g = State (\s -> case f s of
--   (a, s') -> runState (f a) s')

-- foo10 :: Int -> ((), Int)
-- foo10 x = runState (do {modify (+100); modify (+2000); return ()}) x
