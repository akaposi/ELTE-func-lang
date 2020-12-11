{-# language DeriveFunctor, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

import Control.Monad
-- import Control.Monad.State

-- Monád transzformer + adatszerkezetek (Set, Map, stb.)
--------------------------------------------------------------------------------

-- Monad transformer : egy bizonyos "effect system"  (és van más is)
-- effect system: mellékhatásokat strukturáljuk és kezeljük
--     (típusszinten)
--     (futásidejű megvalósítás)


--------------------------------------------------------------------------------

-- Monad transformer : Haskell-ben a leginkább elterjedt rendszer
-- Sima Monad class : effect system (nem elég rugalmas)


-- Alapötlet: több olyan példa, hogy két Monad komponálása újra Monad
--       generikusan: Maybe monád transzformer verziója:

-- Either típus transzformer változata:
-- e : hiba
-- m : belső monád
-- a : visszatérési típus
newtype ExceptT e m a = ExceptT {runExceptT :: m (Either e a)}

instance Functor m => Functor (ExceptT e m) where
  fmap f (ExceptT mea) = ExceptT (fmap (fmap f) mea)

instance Monad m => Applicative (ExceptT e m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (ExceptT e m) where
  return a = ExceptT $ pure $ pure a
  ExceptT ma >>= f = ExceptT $ do
    ea <- ma
    case ea of
      Left e  -> pure $ Left e
      Right a -> runExceptT (f a)

throw :: Applicative m => e -> ExceptT e m a
throw e = ExceptT $ pure (Left e)

-- liftEx :: Monad m => m a -> ExceptT e m a
-- liftEx ma = ExceptT $ fmap Right ma

-- type M a = ExceptT String (State Int) a

-- f1 :: M Int
-- f1 = do
--   lift' $ modify (+10)
--   n <- lift' $ get
--   if n < 100 then throw "hiba"
--              else pure ()
--   pure $ n + 20

-- lift-et overload-ja
class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadTrans (ExceptT e) where
  lift ma = ExceptT $ fmap Right ma


-- State transzformer:
newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance Functor m => Functor (StateT s m) where
  fmap f (StateT g) = StateT $ \s -> fmap (\(a, s) -> (f a, s)) (g s)

instance Monad m => Applicative (StateT s m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (StateT s m) where
  return a = StateT $ \s -> pure (a, s)
  StateT f >>= g = StateT $ \s -> do
    (a, s') <- f s
    runStateT (g a) s'

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> fmap (\a -> (a, s)) ma

-- put :: Applicative m => s -> StateT s m ()
-- put s = StateT $ \_ -> pure ((), s)

-- get :: Applicative m => StateT s m s
-- get = StateT $ \s -> pure (s, s)



-- Transformer stack aljára kell mindig egy "kezdő" Monad
--------------------------------------------------------------------------------

-- identitás Functor (egyben identitás Monad) (nincs semmi hatása)
newtype Id a = Id {runId :: a}
  deriving Functor

instance Applicative Id where
  pure = return
  (<*>) = ap

instance Monad Id where
  return = Id
  (Id a) >>= f = f a

-- importált State monád:
type State s a = StateT s Id a
-- StateT s Id a   ~   s -> Id (a, s)  ~   s -> (a, s)  ~   State s a

type M = ExceptT String (StateT Int (StateT Bool Id))

-- f1 :: M Int
-- f1 = do
--   lift $ modify (+10)
--   lift $ lift $ modify not
--   s <- lift get
--   if s < 100 then throw "hiba"
--              else pure ()
--   pure $ s + 10

type Parser a = StateT String Maybe  -- ingyen jön a Functor, Applicative, Monad instance

-- Reader monád        (read-only state, van "get", nincs put)

-- reader transzformer: kvázi zero-cost egy transzformer stack-ben
newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

-- s -> Maybe (a, s)    ~   StateT String Maybe      p1 <|> p2
-- s -> (Maybe a, s)    ~   MaybeT (State String)    -- p1 <|>' p2

-- newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a}

-- két különböző monád:
--   StateT String Maybe   : hiba eldobja az állapotot
--   MaybeT (State String) : hiba esetén ismert az utolsó állapot

-- Általánosan: nem kommutatív a transzformerek stack-elése
--    (bizonyos esetben kommutatív)

-- Ehhez kapcsolódó : Monad m1, Monad m2        nem feltétlenül : Monad (Compose m1 m2)
-- newtype Compose f g a = Compose (f (g a))

-- Applicative f1, Applicative f2      mindig: Applicative (Compose f1 f2)


-- Alternatív effect system: algebraic effects: hatásoknak "halmaza", sorrend mindegy, kommutatív
--                           (nem minden Monad írható le algebraic effect-el)
--                           (ugyanolyan hatásból többet használunk, State Int-ből 2-t használok)

-- Bármilyen effect system-et használunk: egy mellékhatás csomag az majdnem mindig Monad

-- mtl : monad transformer library
--------------------------------------------------------------------------------

-- Minden transzformerhez van egy extra class,
--  segítségével lift-eket el lehet hagyni

-- MonadState s m ~ m egy olyan Monad, amiben elérhető get/put/state "s" állapottal
class Monad m => MonadState s m | m -> s where   -- m -> s jelentése: m determinálja s-t
  get   :: m s
  put   :: s -> m ()
-- (egy stack-ben ugyanolyan hatásból max 1)

-- class Monad m => MonadError e m | m -> e where
--   catch :: ...
--   throw :: ...

instance Monad m => MonadState s (StateT s m) where
  put s = StateT $ \_ -> pure ((), s)
  get   = StateT $ \s -> pure (s, s)

modify :: MonadState s m => (s -> s) -> m ()
modify f = do
  s <- get
  put $ f s

instance MonadState s m => MonadState s (ExceptT e m) where
  get   = lift get
  put s = lift $ put s

-- instance MonadState s m => MonadState s (ReaderT r m) where
--   get   = lift get
--   put s = lift $ put s

-- instance MonadState s m => ....

-- (N^2 darab liftelő instance kell, ha N darab monád transzformer van)

-- -- (bizonyos esetben rosszul viselkedik az alábbi instance:)
-- instance (MonadState s m, MonadTrans t, Monad (t m)) => MonadState s (t m) where
--   get   = lift get
--   put s = lift $ put s

-- ContT r (ExceptT e m)   --    try/finally


f1 :: ExceptT String (StateT Int Id) Int
f1 = do
  modify (+10)
  n <- get
  if n < 100 then throw "hiba"
             else pure ()
  pure $ n + 10

-- f1' :: (MonadState Int m, MonadError String m) => m Int
-- f1' = do
--   modify (+10)
--   n <- get
--   if n < 100 then throw "hiba"
--              else pure ()
--   pure $ n + 10
