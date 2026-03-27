{-# LANGUAGE UndecidableInstances #-}

module Ea07 where

import Control.Monad
import Data.Functor.Identity
import Control.Monad.Reader
import Control.Monad.Except

-- State -> StateT s m a = StateT (s -> m (a, s))
-- Except -> ExceptT e m a = ExceptT (m (Either e a))
-- Reader -> ReaderT r m a = ReaderT (r -> m a)
-- Writer -> WriterT w m a = WriterT (m (w, a))

newtype UniqueT m a = UniqueT (Int -> m (a, Int))

newUnique' :: Monad m => UniqueT m Int
newUnique' = UniqueT $ \i -> return (i, i + 1)

runUniqueT :: Monad m => UniqueT m a -> m a
-- runUniqueT (UniqueT f) = do
--   (a, _) <- f 0
--   return a
runUniqueT (UniqueT f) = fmap fst (f 0)

instance Monad m => Functor (UniqueT m) where
  fmap = liftM

instance Monad m => Applicative (UniqueT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (UniqueT m) where
  return :: a -> UniqueT m a
  return a = UniqueT $ \i -> return (a, i)

  (>>=) :: UniqueT m a -> (a -> UniqueT m b) -> UniqueT m b
  UniqueT f >>= g = UniqueT $ \i -> do
    (a, i') <- f i
    let UniqueT h = g a
    h i'

-- newtype Identity a = Identity {runIdentity :: a}
type Unique a = UniqueT Identity a

runUnique :: Unique a -> a
runUnique = runIdentity . runUniqueT

instance MonadTrans UniqueT where
  lift :: Monad m => m a -> UniqueT m a
  lift m = UniqueT $ \i -> do
    a <- m
    return (a, i)


-- scope checker for lambda calculus

data Lambda v
  = Var v                        -- x
  | Lam v (Lambda v)             -- \x -> body
  | App (Lambda v) (Lambda v)    -- f e
  deriving (Show)

-- \x -> \y -> (\x -> x y) x
example :: Lambda String
example =
  Lam "x" $ Lam "y" $ App (Lam "x" $ App (Var "x") (Var "y")) (Var "x")

scopeCheck :: Lambda String -> Either String (Lambda Int)
scopeCheck e = runExcept (runUniqueT (runReaderT (helper e) []))
  where
    helper ::
      Lambda String ->
      ReaderT [(String, Int)] (UniqueT (Except String)) (Lambda Int)
    helper (Var v) = do
      env <- ask
      case lookup v env of
        Nothing -> lift $ lift $ throwError $ v ++ " is not in scope"
        Just i -> return $ Var i
    helper (Lam v e) = do
      i <- lift newUnique'
      e' <- local ((v, i):) $ helper e
      return $ Lam i e'
    helper (App f e) = do
      f' <- helper f
      e' <- helper e
      return $ App f' e'

-- transformers
--   lifts

-- mtl
--   typeclasses

class Monad m => MonadUnique m where
  newUnique :: m Int

instance Monad m => MonadUnique (UniqueT m) where
  newUnique = newUnique'

instance MonadUnique m => MonadUnique (ReaderT r m) where
  newUnique = lift newUnique

instance MonadError e m => MonadError e (UniqueT m) where
  throwError e = lift $ throwError e

scopeCheck' :: Lambda String -> Either String (Lambda Int)
scopeCheck' e = runExcept (runUniqueT (runReaderT (helper e) []))
  where
    helper ::
      (MonadReader [(String, Int)] m, MonadUnique m, MonadError String m) =>
      Lambda String ->
      m (Lambda Int)
    helper (Var v) = do
      env <- ask
      case lookup v env of
        Nothing -> throwError $ v ++ " is not in scope"
        Just i -> return $ Var i
    helper (Lam v e) = do
      i <- newUnique
      e' <- local ((v, i):) $ helper e
      return $ Lam i e'
    helper (App f e) = do
      f' <- helper f
      e' <- helper e
      return $ App f' e'
