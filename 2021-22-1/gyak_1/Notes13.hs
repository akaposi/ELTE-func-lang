{-# LANGUAGE EmptyDataDeriving #-}
{-# language
  DeriveFunctor, StandaloneDeriving,
  UndecidableInstances, InstanceSigs,
  TypeOperators
  #-}

import Control.Monad

-- 2 vizsga közül jobbik jegy

-- parser + jó hibaüzenetek
-- monád transzformerek
--   - algebraic effects
-- szofisztikáltabb típusok, típus feature-ök (típusszintű programozás)
   -- higher-rank típusok, GADT, type family-k, stb.

-- algebraic effects, típusszintű prog

--------------------------------------------------------------------------------

-- Algebraic effects (intro)

--  program: szintaxisfa + bizonyos műveletek elérhetők
--           írhatunk handler függvényeket a mellékhatásokhoz

-- Monad:             típus def, instance def (le van rögzítve a hatás ezen a ponton)
-- Algebraic Effect:  több különböző effect handler-t lehet írni ugyanahhoz a programhoz


-- free Monad : ha van egy Functor f, akkor ebből automatikusan egy Monad-ot
-- példa: "a"-típus, akkor "[a]" az a free Monoid.

data Free f a = Pure a | Free (f (Free f a))

-- f-elágazású, a-leveles fák típusa

data Twice a = Twice a a deriving (Eq, Show, Functor)

type BinTree a = Free Twice a

t1 :: BinTree Int
t1 = Pure 10

t2 :: BinTree Int
t2 = Free (Twice t1 t1)

t3 = Free (Twice (Free (Twice t1 t1)) (Free (Twice t1 t1)))

type RoseTree a = Free [] a

--------------------------------------------------------------------------------

deriving instance (Eq a, Eq (f (Free f a))) => Eq (Free f a)
deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)
deriving instance (Functor f) => Functor (Free f)

--------------------------------------------------------------------------------

instance Functor f => Applicative (Free f) where
  pure  = return
  (<*>) = ap

instance Functor f => Monad (Free f) where
  return :: a -> Free f a
  return = Pure

  -- minden levélre meghívom a függvényt, az eredmény fát a Pure helyére
  -- beszúrom.
  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  Pure a   >>= f = f a
  Free ffa >>= f = Free (fmap (\fa -> fa >>= f) ffa)
     -- ffa :: f (Free f a)      Functor f

-- Legyen az "f" Functor legyen 0 vagy több Functor összege
infixr 3 :+:
data (:+:) f g a = Inl (f a) | Inr (g a) deriving (Eq, Show, Functor)

type MyFunctor = Maybe :+: [] :+: Empty

-- Minden Funktor egy hatást specifikál, konstruktorként felsorolja a műveleteket

data State s k = Get (s -> k) | Put s k deriving Functor

-- get :: Free (State s) s
-- get = Free (Get (\s -> pure s))

-- put :: s -> Free (State s) ()
-- put s = Free (Put s (pure ()))

-- p :: Free (State Int) ()
-- p = do
--   n <- get
--   put $ n + 20

-- runState :: Free (State s) a -> s -> (a, s)
-- runState (Pure a)   s = (a, s)
-- runState (Free sfa) s = case sfa of
--   Get k    -> runState (k s) s
--   Put s' k -> runState k s'

-- p1 :: Free IO a

data MyIO k =
    PutStrLn String k
  | GetLine (String -> k)
  deriving Functor

-- myPutStrLn :: String -> Free MyIO ()
-- myPutStrLn str = Free (PutStrLn str (pure ()))

-- myGetLine :: Free MyIO String
-- myGetLine = Free (GetLine pure)

-- p2 :: Free MyIO ()
-- p2 = do
--   l <- myGetLine
--   myPutStrLn (l ++ l)

-- runMyIO :: Free MyIO a -> IO a
-- runMyIO (Pure a)   = pure a
-- runMyIO (Free ioa) = case ioa of
--   PutStrLn str k -> putStrLn str >> runMyIO k
--   GetLine k      -> do {l <- getLine; runMyIO (k l)}

-- runInSandbox :: Free MyIO a -> [String] -> [String] -> (a, [String])
-- runInSandbox (Pure a)   inp out = (a, out)
-- runInSandbox (Free ioa) inp out = case ioa of
--   PutStrLn str k -> runInSandbox k inp (str:out)
--   GetLine k      -> case inp of
--     []      -> error "not enough input"
--     str:inp -> runInSandbox (k str) inp out


-- Több hatást lehet kombinálni a :+:
------------------------------------------------------------

data Empty a deriving (Eq, Show, Functor)

type MyMonad = Free (MyIO :+: State Int :+: State Bool :+: Empty)

runState :: Functor f => Free (State s :+: f) a -> s -> Free f (a, s)
runState (Pure a)   s = pure (a, s)
runState (Free cmd) s = case cmd of
  Inl (Get k)    -> runState (k s) s
  Inl (Put s' k) -> runState k s'
  Inr cmd        -> Free (fmap (\fsf -> runState fsf s) cmd)

-- runMyIO :: Functor f => Free (MyIO :+: f) a -> Free
-- runMyIO (Pure a)   = pure a
-- runMyIO (Free ioa) = case ioa of
--   PutStrLn str k -> putStrLn str >> runMyIO k
--   GetLine k      -> do {l <- getLine; runMyIO (k l)}

--------------------------------------------------------------------------------


-- Az alábbihoz kell már típusszintű programozás is:

-- myProg :: Free (MyIO :+: State Int :+: MyException String :+: Empty) ()
-- myProg = do
--   l <- getLine
--   putStrLn (l ++ l)
--   catch ... (\e -> ....)
--   pure ()

-- mit kaptunk ("Algebraic effects")
--  - kényelmes, tömör szintaxissal (do notáció)
--  - mellékhatásos programokat, több hatás műveleteit keverhetjük
--  - minden hatás több külböző "handler" függvénnyel is értelmezhetünk

--  - Effect system : nagyon rugalmas, viszont
--  -   naiv implementáció: interpreter overhead minden programon
--  -   "Koka", "Eff" nyelv : prognyelv, ami alapból az algebraic effects-et használja

-- - akit érdekel: kulcsszavak: "algebraic effects"
--              nyelvek: "Eff", "Koka"
--      Haskell library-k: freer-simple, polysemy, fused-effects

--------------------------------------------------------------------------------
