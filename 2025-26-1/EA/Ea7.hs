module Ea7 where

import Control.Monad.State

{-
class Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}

newtype ZipList a = ZipList [a]
  deriving Functor

instance Applicative ZipList where
  pure x = ZipList $ repeat x
  ZipList xs <*> ZipList ys = ZipList $ zipWith ($) xs ys

{-
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  ff <*> fa = liftA2 ($) ff fa

  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  liftA2 g fa fb = fmap g fa <*> fb
-}

ap :: Applicative f => f (a -> b) -> f a -> f b
ap ff fa = liftA2 ($) ff fa

liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' g fa fb = fmap g fa <*> fb

fmap' :: Applicative f => (a -> b) -> f a -> f b
fmap' g fa = pure g <*> fa

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 g fa fb fc = g <$> fa <*> fb <*> fc

-- (<$>) = fmap

{-
class Monoids m where
  mempty :: m
  (<>) :: m -> m -> m
-}
-- equivalent to Applicative
class Functor f => Monoidal f where
  unit :: f ()
  mult :: f a -> f b -> f (a, b)
{-
fmap (\(_, a) -> a) (mult unit fa) == fa
fmap (\(a, _) -> a) (mult fa unit) == fa
fmap (\((a, b), c) -> (a, (b, c))) (mult (mult fa fb) fc) == mult fa (mult fb fc)
-}

class Functor f => Applicative' f where
  pure' :: f (a -> a)
  ap'' :: f (b -> c) -> f (a -> b) -> f (a -> c)

{-
ap'' pure' ff = ff
ap'' ff pure' = ff
ap'' (ap'' ff fg) fh = ap'' ff (ap'' fg fh)
-}
{-
pure f <*> a = f a
ff <*> pure a = ($ a) <$> ff
(ff <*> fa) <*> fb = uncurry <$> ff <*> ((,) <$> fa <*> fb)
-}

ap' :: Monad f => f (a -> b) -> f a -> f b
ap' ff fa = do
  f <- ff
  a <- fa
  return $ f a

-- class Applicative m => Monad m where

{-
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-}

-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]

-- instance Traversable [] where

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Foldable, Show)

instance Traversable Tree where
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l r) = Node <$> traverse f l <*> traverse f r

  mapM f (Leaf a) = do
    b <- f a
    return $ Leaf b
  mapM f (Node l r) = do
    l' <- mapM f l
    r' <- mapM f r
    return (Node l' r')

example :: Tree Int
example = Node (Node (Leaf 1) (Leaf 2)) (Node (Node (Leaf 3) (Leaf 4)) (Leaf 5))

printStructure :: Traversable t => t Int -> IO ()
printStructure t = do
  traverse print t
  return ()

cumSum :: Traversable t => t Int -> t Int
cumSum t = evalState (traverse step t) 0
  where
  step :: Int -> State Int Int
  step n = do
    oldSum <- get
    let newSum = oldSum + n
    put newSum
    return newSum


data Validation e a = Errors e | Ok a
  deriving (Show, Functor)

instance Semigroup e => Applicative (Validation e) where
  pure a = Ok a
  Ok f <*> Ok a = Ok (f a)
  Ok _ <*> Errors e = Errors e
  Errors e <*> Ok _ = Errors e
  Errors e1 <*> Errors e2 = Errors (e1 <> e2)

bindValidation :: Validation e a -> (a -> Validation e b) -> Validation e b
bindValidation (Ok a) f = f a
bindValidation (Errors errs) f = Errors errs

data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr -- integer division
  deriving (Show)

example1 :: Expr
example1 = Add (Lit 5) (Mul (Lit 3) (Lit 4))

example2 :: Expr
example2 = Div (Lit 3) (Sub (Lit 1) (Lit 1))

example3 :: Expr
example3 = Add (Div (Lit 7) (Lit 0)) (Div (Lit 3) (Sub (Lit 1) (Lit 1)))

calc :: Expr -> Validation [String] Int
calc (Lit n) = pure n
calc (Add e1 e2) = (+) <$> calc e1 <*> calc e2
-- liftA2 (+) (calc e1) (calc e2)
calc (Sub e1 e2) = (-) <$> calc e1 <*> calc e2
calc (Mul e1 e2) = (*) <$> calc e1 <*> calc e2
calc (Div e1 e2) =
  bindValidation ((,) <$> calc e1 <*> calc e2) $ \(x, y) ->
    if y == 0
      then Errors ["cannot divide " ++ show x ++ " by 0"]
      else pure (x `div` y)

-- newtype Concurrently a = Concurrently (IO a)

-- (,) <$> m1 <*> m2
-- m1 and m2 run concurrently
-- return result when both terminate

-- traverse can run an effect for each element in a list concurrently

-- (>>=) :: m a -> (a -> m b) -> m b
