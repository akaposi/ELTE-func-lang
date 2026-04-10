module Ea08 where

import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.Async

-- class Applicative m => Monad m

{-
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b -- ap

  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
-}

-- pure = return

ap' :: Monad m => m (a -> b) -> m a -> m b
ap' mf ma = do
  f <- mf
  a <- ma
  return (f a)

-- every monad is an applicative functor

liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- liftA2' g fa fb = (pure g <*> fa) <*> fb
liftA2' g fa fb = pure g <*> fa <*> fb
{-
g :: a -> (b -> c)
pure g :: f (a -> (b -> c))
fa :: f a
pure g <*> fa :: f (b -> c)
fb :: f b
(pure g <*> fa) <*> fb :: f c
-}

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 g fa fb fc = pure g <*> fa <*> fb <*> fc

liftA1 :: Applicative f => (a -> b) -> f a -> f b -- fmap
liftA1 g fa = pure g <*> fa

-- every applicative functor is a functor

liftA0 :: Applicative f => a -> f a
liftA0 = pure

-- applicative functor: "fmap for functions with any number of arguments"

-- monoid

{-
class Functor f => Monoidal f where
  unit :: f ()
  mult :: f a -> f b -> f (a, b)
-}

-- associativity, unit

{-
fmap  :: Functor f     =>   (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
(=<<) :: Monad f       => (a -> f b) -> f a -> f b -- effect can depend on value
-}


-- [] is a monad so it is an applicative
-- a different applicative instance which is not monad:

newtype ZipList a = ZipList [a]
  deriving (Show, Functor)

instance Applicative ZipList where
  liftA2 :: (a -> b -> c) -> ZipList a -> ZipList b -> ZipList c
  liftA2 f (ZipList as) (ZipList bs) = ZipList $ zipWith f as bs

  pure :: a -> ZipList a
  pure a = ZipList $ repeat a

-- liftA2 (\a b -> b) (pure a) bs == bs
-- liftA2 (\a b -> a) as (pure b) == as




data Validation e a = Error e | Ok a
  deriving (Show, Functor)

instance Semigroup e => Applicative (Validation e) where
  pure a = Ok a

  -- same as Either
  -- Error e <*> _ = Error e
  -- _ <*> Error e = Error e
  -- Ok f <*> Ok a = Ok $ f a

  Error e1 <*> Error e2 = Error (e1 <> e2)
  Error e <*> Ok _ = Error e
  Ok _ <*> Error e = Error e
  Ok f <*> Ok a = Ok $ f a

bindValidation :: Validation e a -> (a -> Validation e b) -> Validation e b
bindValidation (Error e) f = Error e
bindValidation (Ok a) f = f a

data Op = Add | Sub | Mul | Div
  deriving (Show)

data Expr
  = Lit Int
  | Op Op Expr Expr
  deriving (Show)

example1 :: Expr
example1 = Op Add (Lit 3) (Op Mul (Lit 4) (Lit 5))

example2 :: Expr
example2 = Op Div (Lit 5) (Op Sub (Lit 3) (Lit 3))

example3 :: Expr
example3 =
  Op Add (Op Div (Lit 5) (Op Sub (Lit 3) (Lit 3))) (Op Div (Lit 7) (Lit 0))

applyOp :: Op -> Int -> Int -> Validation [String] Int
applyOp Add x y = Ok (x + y)
applyOp Sub x y = Ok (x - y)
applyOp Mul x y = Ok (x * y)
applyOp Div x y
  | y == 0 = Error ["cannot divide " ++ show x ++ " by zero"]
  | otherwise = Ok (x `div` y)

calc :: Expr -> Validation [String] Int
calc (Lit n) = pure n
calc (Op op e1 e2) =
  bindValidation (liftA2 (,) (calc e1) (calc e2)) $
    \(x, y) -> applyOp op x y




{-
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

mapM :: Monad m => (a -> m b) -> t a -> m (t b)

instance Traversable []
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-}

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Functor, Foldable)

-- (<$>) = fmap
-- (<*>) = "ap"

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g (Leaf a) = Leaf <$> g a
  -- g a :: f b
  -- Leaf :: b -> Tree b
  traverse g (Node l r) = Node <$> traverse g l <*> traverse g r
  -- traverse g l :: f (Tree b)
  -- traverse g r :: f (Tree b)

exampleTree :: Tree Int
exampleTree =
  Node (Node (Leaf 1) (Leaf 2)) (Node (Node (Leaf 5) (Leaf 6)) (Leaf 3))

printElems :: (Traversable t, Show a) => t a -> IO ()
printElems t = do
  traverse print t
  return ()

cumsum :: Traversable t => t Int -> t Int
cumsum t = fst $ runState (traverse helper t) 0
  where
    helper x = do
      oldSum <- get
      let newSum = oldSum + x
      put newSum
      return newSum




waitThenPrint :: Int -> String -> IO ()
waitThenPrint n s = do
  threadDelay (n * 10^6)
  putStrLn s


-- newtype Concurrently a = Concurrently (IO a)
-- applicative instance runs threads concurrently


