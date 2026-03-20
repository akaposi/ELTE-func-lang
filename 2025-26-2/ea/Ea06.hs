module Ea06 where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Data.Functor.Identity
import Data.List
import Data.Monoid

{-
Monads:

Maybe
State
IO
[]

-- record syntax
newtype State s a = State { runState :: s -> (a, s) }
-- runState :: State s a -> s -> (a, s)

-- newtype IO a = MkIO (RealWorld -> (a, RealWorld))
-}

-- data Either a b = Left a | Right b
-- newtype Except e a = Except {runExcept :: Either e a}

{-
instance Monad (Either e) where
  return :: a -> Either e a
  return a = Right a

  (>>=) :: Either e a -> (a -> Either e b) -> Either e b
  Left e >>= f = Left e
  Right a >>= f = f a
-}

-- (->)

{-
instance Functor ((->) r) where
  fmap :: (a -> b) -> (->) r a -> (->) r b
  fmap :: (a -> b) -> (r -> a) -> (r -> b)
  fmap = (.)

instance Monad ((->) r) where
  return :: a -> r -> a
  return = const

  (>>=) :: (r -> a) -> (a -> r -> b) -> r -> b
  f >>= g = \r -> g (f r) r
-}

-- newtype Reader r a = Reader { runReader :: r -> a }

-- ask :: Reader r r
-- ask = Reader id

-- local :: (r -> r) -> Reader r a -> Reader r b
-- local f (Reader g) = Reader (g . f)

data Expr
  = Var String
  | Lit Int
  | Add Expr Expr
  | Let String Expr Expr -- let x = e1 in e2
  deriving (Show)

example :: Expr
example =
  Let "x" (Lit 10) $ Let "y" (Lit 100) $
    Add (Var "x") (Add (Lit 2) (Var "y"))

interp :: Expr -> Reader [(String, Int)] Int
interp (Var v) = do
  env <- ask
  case lookup v env of
    Nothing -> return 0
    Just n -> return n
interp (Lit n) = return n
interp (Add e1 e2) = do
  x <- interp e1
  y <- interp e2
  return (x + y)
interp (Let v e1 e2) = do
  x <- interp e1
  local ((v, x) :) $ interp e2

interp' :: Expr -> [(String, Int)] -> Int
interp' (Var v) env = case lookup v env of
  Nothing -> 0
  Just n -> n
interp' (Lit n) env = n
interp' (Add e1 e2) env = interp' e1 env + interp' e2 env
interp' (Let v e1 e2) env =
  let x = interp' e1 env
  in interp' e2 ((v, x) : env)




{-
instance Monoid m => Monad ((,) m) where
  return :: a -> (m, a)
  return a = (mempty, a)

  (>>=) :: (m, a) -> (a -> (m, b)) -> (m, b)
  (m1, a) >>= f =
    let (m2, b) = f a
    in (m1 <> m2, b)
-}

-- newtype Writer m a = Writer { runWriter :: (a, m) }

-- tell :: Monoid m => m -> Writer m ()
-- listen :: Writer m a -> Writer m (a, m)

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

exampleTree :: Tree Int
exampleTree = Node (Node Leaf 3 (Node Leaf 4 Leaf)) 2 (Node Leaf 1 Leaf)

-- each node is replaced with the sum of its children and itself
sumTree :: Tree Int -> Tree Int
sumTree t = fst $ runWriter (helper t)
  where
    helper :: Tree Int -> Writer (Sum Int) (Tree Int)
    helper Leaf = return Leaf
    helper (Node l x r) = do
      ((l', r'), Sum sum) <- listen $ do
        l' <- helper l
        tell $ Sum x
        r' <- helper r
        return (l', r')
      return $ Node l' sum r'



-- State s (Maybe a)

-- monad transformers

-- newtype ExceptT e m a = ExceptT {runExceptT :: m (Either e a)}

{-
instance Monad m => Monad (ExceptT e m) where
  return a = ExceptT $ return $ Right a
  ExceptT m >>= f = do
    e <- m
    case e of
      Left e -> return $ Left e
      Right a -> runExceptT (f a)
-}

-- newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

{-
instance Monad m => Monad (StateT s m) where
  return a = StateT $ \s -> return (a, s)
  StateT m >>= f = StateT $ \s -> do
    (a, s') <- m s
    runState (f a) s'
-}

-- newtype Identity a = Identity {runIdentity :: a}

-- -- s -> Either e (a, s)
-- type M s e a = StateT s (ExceptT e Identity) a

-- Except e = ExceptT e Identity

{-
class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadTrans (ExceptT e) where
  lift m = ExceptT $ fmap Right m
-}

data Instr
  = Decl String Int
  | AssignAdd String String String -- Add x y z ~ x := y + z
  | Print String

type Program = [Instr]

-- -- String -> Either String (a, [(String, Int)])
-- type M a = StateT [(String, Int)] (Except String) a

-- -- String -> (Either String a, [(String, Int)])
-- type M a = ExceptT String (State [(String, Int)]) a

-- String -> IO (Either String a, [(String, Int)])
type M a = ExceptT String (StateT [(String, Int)] IO) a

-- there's no IOT

run :: Program -> M ()
run [] = return ()
run (Decl v n : rest) = do
  st <- get
  put $ (v, n) : st
  run rest
run (AssignAdd v x y : rest) = do
  st <- get
  case (lookup x st, lookup y st) of
    (Just m, Just n) -> do
      let k = m + n
      put $ map (\(v', x) -> if v == v' then (v', k) else (v', x)) st
      run rest
    _ -> throwError "variable not declared"
run (Print v : rest) = do
  st <- get
  case lookup v st of
    Nothing -> throwError "variable not declared"
    Just n -> do
      lift $ lift $ print n
      run rest

exampleProgram :: Program
exampleProgram =
  [Decl "x" 10, Decl "y" 5, Decl "z" 3,
   Print "z", AssignAdd "z" "x" "y", Print "z"]
