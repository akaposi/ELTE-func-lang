{-# LANGUAGE UndecidableInstances #-}

module Ea06 where

import Data.List
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Functor.Identity

-- monad transformers

data Expr
  = Lit Int
  | Var String
  | Add Expr Expr
  deriving (Show)

data Stmt
  = Decl String
  | Assign String Expr
  | Print Expr
  deriving (Show)

type Program = [Stmt]

-- interpreter
-- state
-- error for undeclared variable

-- State monad
-- Either monad

-- interp :: Program -> [(String, Int)]
-- interp p = snd (runState (interp' p) [])

{-
modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put (f s)
-}

-- newtype Interp a = Interp ([(String, Int)] -> (Either String a, [(String, Int)]))

-- interp' :: Program -> State [(String, Int)] ()
-- interp' [] = return ()
-- interp' (Decl v : rest) = do
--   modify $ \s -> (v, 0) : s
--   interp' rest
-- interp' (Assign v e : rest) = do
--   _

-- interpExpr :: Expr -> State [(String, Int)] Int
-- interpExpr (Lit n) = return n
-- interpExpr (Var v) = do
--   s <- get
--   case lookup v s of
--     Nothing -> return $ Left $ v ++ " not declared"
--     Just n -> return n

-- interpExpr (Add e1 e2) = do
--   x <- interpExpr e1
--   y <- interpExpr e2
--   return (x + y)

-- newtype Compose f g a = Compose {runCompose :: f (g a)}

-- -- Compose (State [(String, Int)]) (Either String)

-- instance Functor (Compose f g)
-- instance Applicative (Compose f g)

-- instance (Monad f, Monad g) => Monad (Compose f g) where
--   return :: a -> Compose f g a
--   return a = Compose $ return $ return a

--   (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
--   Compose fga >>= h = Compose $ do
--     ga <- fga
--     gfgb <- return $ do
--       a <- ga
--       return $ runCompose (h a)
--     _

-- No monad instance for Compose

{-
newtype State s a = State (s -> (a, s))
newtype StateT s m a = StateT (s -> m (a, s))

instance Monad m => Monad (StateT s m) where
  return a = StateT $ \s -> return (a, s)
  StateT f >>= g = StateT $ \s -> do
    (a, s') <- f s
    (b, s'') <- runStateT (g a) s'
    return (b, s'')

newtype ExceptT e m a = ExceptT (m (Either e a))

instance Monad m => Monad (ExceptT e m)

newtype ReaderT r m a = ReaderT (r -> m a)
newtype WriterT w m a = WriterT (m (a, w))

lift :: Monad m => m a -> StateT s m a

type State s a = StateT s Identity a

newtype Identity a = Identity a

-- t :: (Type -> Type) -> Type -> Type
class MonadTrans t where
  lift :: m a -> t m a

instance MonadTrans (StateT s)
  lift :: Monad m => m a -> StateT s m a
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)
-}

interp :: Program -> IO ()
interp p = do
  e <- runExceptT $ runStateT (interp' p) []
  case e of
    Left err -> putStrLn err
    Right (_, _) -> return ()

-- modify :: Monad m => (s -> s) -> StateT s m a
-- throwError :: Monad m => e -> ExceptT e m a

-- no IOT

type M = StateT [(String, Int)] (ExceptT String IO)
-- [(String, Int)] -> IO (Either String (a, [(String, Int)]))

-- type M = ExceptT String (StateT [(String, Int)] IO)
-- [(String, Int)] -> IO (Either String a, [(String, Int)])

-- catchError :: ExceptT e m a -> (e -> ExceptT e m a) -> ExceptT e m a
-- different behavior depending on the order with StateT

interp' :: Program -> M ()
interp' [] = return ()
interp' (Decl v : rest) = do
  modify $ \s -> (v, 0) : s
  interp' rest
interp' (Assign v e : rest) = do
  n <- interpExpr e
  s <- get
  case lookup v s of
    Nothing -> throwError $ v ++ " not declared"
    Just n -> return ()
  modify $ \s -> (v, n) : s
  interp' rest
interp' (Print e : rest) = do
  n <- interpExpr e
  liftIO $ print n
  interp' rest

interpExpr :: Expr -> M Int
interpExpr (Lit n) = return n
interpExpr (Var v) = do
  s <- get
  case lookup v s of
    Nothing -> throwError $ v ++ " not declared"
    Just n -> return n
interpExpr (Add e1 e2) = do
  x <- interpExpr e1
  y <- interpExpr e2
  return (x + y)

example :: Program
example =
  [ Decl "x",
    Decl "y",
    Assign "x" (Lit 4),
    Print (Var "x"),
    Assign "y" (Add (Var "z") (Var "x")),
    Print (Var "y")]




newtype Unique = Unique Int
  deriving (Show, Eq)

newtype UniqueT m a = UniqueT {runUniqueT :: Int -> m (a, Int)}

instance Monad m => Functor (UniqueT m) where
  fmap = liftM

instance Monad m => Applicative (UniqueT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (UniqueT m) where
  return a = UniqueT $ \i -> return (a, i)
  UniqueT f >>= g = UniqueT $ \i -> do
    (a, i') <- f i
    runUniqueT (g a) i'

getUnique' :: Monad m => UniqueT m Unique
getUnique' = UniqueT $ \i -> do
  return (Unique i, i + 1)

instance MonadTrans UniqueT where
  lift m = UniqueT $ \i -> do
    a <- m
    return (a, i)

{-
-- multi-parameter type classes

class Monad m => MonadState s m where
  get :: m s
  put :: s -> m ()

instance Monad m => MonadState s (StateT s m)

instance MonadState s m => MonadState s (ExceptT e m)
instance MonadState s m => MonadState s (ReaderT r m)
-}

example' :: UniqueT (State Int) (Unique, Int)
example' = do
  u <- getUnique
  modify (+ 1)
  s <- get
  return (u, s)

instance MonadState s m => MonadState s (UniqueT m) where
  -- get :: UniqueT m s
  get = lift get
  -- put :: s -> UniqueT m ()
  put s = lift $ put s


example'' :: StateT Int (UniqueT Identity) (Unique, Int)
example'' = do
  u <- getUnique
  modify (+ 1)
  s <- get
  return (u, s)


class Monad m => MonadUnique m where
  getUnique :: m Unique

instance Monad m => MonadUnique (UniqueT m) where
  getUnique = getUnique'

instance MonadUnique m => MonadUnique (StateT s m) where
  getUnique = lift getUnique

instance MonadUnique m => MonadUnique (ExceptT e m) where
  getUnique = lift getUnique


example3 :: StateT Int (State Bool) (Int, Bool)
example3 = do
  s1 <- get
  s2 <- lift get
  return (s1, s2)

-- n*k problem

-- alternative to monad transformers: algebraic effect system
