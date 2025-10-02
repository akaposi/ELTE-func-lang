module Ea4 where

import Data.List
import Data.Maybe
import Control.Monad.State

data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr -- integer division

instance Show Expr where
  show (Lit n) = show n
  show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Sub e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (Div e1 e2) = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"

example :: Expr
example = Add (Lit 5) (Mul (Lit 3) (Lit 4))

example2 :: Expr
example2 = Div (Lit 3) (Sub (Lit 1) (Lit 1))

{-
calc :: Expr -> Int
calc (Lit n) = n
calc (Add e1 e2) = calc e1 + calc e2
calc (Sub e1 e2) = calc e1 - calc e2
calc (Mul e1 e2) = calc e1 * calc e2
calc (Div e1 e2)
  | y == 0 = error "you can't divide by zero"
  | otherwise = calc e1 `div` calc e2
  where
  y = calc e2
-}

-- data Maybe a = Nothing | Just a

{-
calc :: Expr -> Maybe Int
calc (Lit n) = Just n
calc (Add e1 e2) = case calc e1 of
  Nothing -> Nothing
  Just x -> case calc e2 of
    Nothing -> Nothing
    Just y -> Just (x + y)
calc (Sub e1 e2) = case calc e1 of
  Nothing -> Nothing
  Just x -> case calc e2 of
    Nothing -> Nothing
    Just y -> Just (x - y)
calc (Mul e1 e2) = case calc e1 of
  Nothing -> Nothing
  Just x -> case calc e2 of
    Nothing -> Nothing
    Just y -> Just (x * y)
calc (Div e1 e2) = case calc e1 of
  Nothing -> Nothing
  Just x -> case calc e2 of
    Nothing -> Nothing
    Just y
      | y == 0 -> Nothing
      | otherwise -> Just (x `div` y)
-}

andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing f = Nothing
andThen (Just x) f = f x

calc :: Expr -> Maybe Int
calc (Lit n) = Just n
calc (Add e1 e2) =
  andThen (calc e1) $ \x ->
    andThen (calc e2) $ \y ->
      Just (x + y)
calc (Sub e1 e2) =
  andThen (calc e1) $ \x ->
    andThen (calc e2) $ \y ->
      Just (x - y)
calc (Mul e1 e2) =
  andThen (calc e1) $ \x ->
    andThen (calc e2) $ \y ->
      Just (x * y)
calc (Div e1 e2) =
  andThen (calc e1) $ \x ->
    andThen (calc e2) $ \y ->
      if y == 0
        then Nothing
        else Just (x `div` y)

calc' :: Expr -> String
calc' e = case calc e of
  Nothing -> "you can't divide by zero"
  Just x -> show x


data Expr2
  = Lit2 Int
  | Var2 String
  | Add2 Expr2 Expr2
  | Incr2 String -- x++

incrVar :: [(String, Int)] -> String -> [(String, Int)]
incrVar [] v' = []
incrVar ((v, x) : xs) v'
  | v == v' = (v, x + 1) : xs
  | otherwise = (v, x) : incrVar xs v'
{-
interp :: [(String, Int)] -> Expr2 -> (Int, [(String, Int)])
interp vars (Lit2 n) = (n, vars)
interp vars (Var2 v) = (fromJust $ lookup v vars, vars)
interp vars (Add2 e1 e2) =
  let (x, vars') = interp vars e1
      (y, vars'') = interp vars' e2
  in (x + y, vars'')
interp vars (Incr2 v) = (fromJust $ lookup v vars, incrVar vars v)
-}
{-
type State a = [(String, Int)] -> (a, [(String, Int)])

andThen' :: State a -> (a -> State b) -> State b
andThen' f g vars =
  let (x, vars') = f vars
      (y, vars'') = g x vars'
  in (y, vars'')

return' :: a -> State a
return' a vars = (a, vars)

get :: State [(String, Int)]
get vars = (vars, vars)

set :: [(String, Int)] -> State ()
set newVars _ = ((), newVars)

interp :: Expr2 -> State Int
interp (Lit2 n) = return' n
interp (Var2 v) =
  andThen' get $ \vars ->
    return' (fromJust $ lookup v vars)
interp (Add2 e1 e2) =
  andThen' (interp e1) $ \x ->
    andThen' (interp e2) $ \y ->
      return' $ x + y
interp (Incr2 v) =
  andThen' get $ \vars ->
    andThen' (set $ incrVar vars v) $ \() ->
      return' (fromJust $ lookup v vars)
-}
example3 :: Expr2
example3 = Add2 (Incr2 "x") (Incr2 "x") -- result should be 5
-- x := 2

{-
-- Monad :: (Type -> Type) -> Constraint
class Monad m where
  return :: a -> m a
  -- bind
  (>>=) :: m a -> (a -> m b) -> m b
-}

-- Monad Maybe
-- Monad State
{-
calc'' :: Expr -> Maybe Int
calc'' (Lit n) = return n
calc'' (Add e1 e2) =
  calc'' e1 >>= \x ->
    calc'' e2 >>= \y ->
      return (x + y)
calc'' (Sub e1 e2) =
  calc'' e1 >>= \x ->
    calc'' e2 >>= \y ->
      return (x - y)
calc'' (Mul e1 e2) =
  calc'' e1 >>= \x ->
    calc'' e2 >>= \y ->
      return (x * y)
calc'' (Div e1 e2) =
  calc'' e1 >>= \x ->
    calc'' e2 >>= \y ->
      if y == 0
        then Nothing
        else return (x `div` y)
-}
calc'' :: Expr -> Maybe Int
calc'' (Lit n) = return n
calc'' (Add e1 e2) = do
  x <- calc'' e1
  y <- calc'' e2
  return (x + y)
calc'' (Sub e1 e2) = do
  x <- calc'' e1
  y <- calc'' e2
  return (x - y)
calc'' (Mul e1 e2) = do
  x <- calc'' e1
  y <- calc'' e2
  return (x * y)
calc'' (Div e1 e2) = do
  x <- calc'' e1
  y <- calc'' e2
  if y == 0
    then Nothing
    else return (x `div` y)

-- newtype State s a = State (s -> (a, s))
-- instance Monad State

interp :: Expr2 -> State [(String, Int)] Int
interp (Lit2 n) = return n
interp (Var2 v) = do
  vars <- get
  return (fromJust $ lookup v vars)
interp (Add2 e1 e2) = do
  x <- interp e1
  y <- interp e2
  return (x + y)
interp (Incr2 v) = do
  vars <- get
  put $ incrVar vars v
  return (fromJust $ lookup v vars)

-- IO

main :: IO ()
main = do
  line <- getLine
  putStrLn line

main' =
  getLine >>= \line -> putStrLn line

-- newtype IO = IO (RealWorld -> (a, RealWorld))

{-
-- Mon-oid and Mon-ad

class Functor m => Applicative where
  ...

class Applicative m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

return x >>= f = f x                          -- mempty <> x = x
t >>= \x -> return x = t   -- t :: m a        -- x <> mempty = x
(t >>= f) >>= g = t >>= (\x -> f x >>= g)     -- (x <> y) <> z = x <> (y <> z)

do
  y <- return x
  f y
=
  f x

do
  x <- t
  return x
=
  t

do
  y <- do
    x <- t
    f x
  g y
=
do
  x <- t
  y <- f x
  g y

-- Kleisli
class Monad m where
  return :: a -> m a
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

return >=> f = f
f >=> return = f
(f >=> g) >=> h = f >=> (g >=> h)

-- monads are just a monoid in the category of endofunctors
-- what's the problem?
class Functor m => Monad m where
  return :: a -> m a
  join :: m (m a) -> m a

return :: m a -> m (m a)
fmap return :: m a -> m (m a)

join (return t) = t
join (fmap return t) = t
join (fmap join t) = join (join t) :: m (m (m a)) -> m a
-}
