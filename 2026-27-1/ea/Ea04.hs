module Ea04 where

import Control.Monad
import Data.Char

data Expr
  = Lit Int
  | Add Expr Expr
  | Div Expr Expr
  deriving (Show)

example1 :: Expr
example1 = Div (Lit 25) (Div (Lit 4) (Lit 2))

eval :: Expr -> Int
eval (Lit n) = n
eval (Div e1 e2) = eval e1 `div` eval e2

example2 :: Expr
example2 = Div (Lit 5) (Div (Lit 1) (Lit 2))


safeDiv :: Int -> Int -> Maybe Int
safeDiv x y
  | y == 0 = Nothing
  | otherwise = Just (x `div` y)


eval2 :: Expr -> Maybe Int
eval2 (Lit n) = Just n
eval2 (Div e1 e2) = case eval2 e1 of
  Nothing -> Nothing
  Just x -> case eval2 e2 of
    Nothing -> Nothing
    Just y -> safeDiv x y

-- e1, e2 :: Expr
-- eval2 e1 :: Maybe Int
-- safeDiv :: Int -> Int -> Maybe Int

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing f = Nothing
bind (Just a) f = f a

eval3 :: Expr -> Maybe Int
eval3 (Lit n) = Just n
eval3 (Div e1 e2) =
  bind (eval3 e1) $ \x ->   -- x = eval3(e1)
  bind (eval3 e2) $ \y ->   -- y = eval3(e2)
  safeDiv x y               -- safeDiv(x, y)


eval3' :: Expr -> Maybe Int
eval3' (Lit n) = return n
eval3' (Add e1 e2) =
  eval3' e1 >>= \x ->
  eval3' e2 >>= \y ->
  return (x + y)
eval3' (Div e1 e2) =
  eval3' e1 >>= \x ->
  eval3' e2 >>= \y ->
  safeDiv x y



{-
class Monad (m :: Type -> Type) where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b   -- bind

instance Monad Maybe where
  return a = Just a
  (>>=) = bind
-}

eval4 :: Expr -> Maybe Int
eval4 (Lit n) = return n
eval4 (Add e1 e2) = do
  x <- eval4 e1
  y <- eval4 e2
  return (x + y)
eval4 (Div e1 e2) = do
  x <- eval4 e1
  y <- eval4 e2
  safeDiv x y





data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

example3 :: Tree String
example3 = Node (Leaf "a") (Node (Leaf "b") (Leaf "c"))
-- relabel example3 = Node (Leaf 0) (Node (Leaf 1) (Leaf 2))

example4 :: Tree String
example4 =
  Node
    (Node (Leaf "d") (Leaf "a"))
    (Node (Node (Leaf "e") (Leaf "f")) (Leaf "c"))

relabel :: Tree a -> Tree Int
relabel t = fst $ helper 0 t
  where
    helper :: Int -> Tree a -> (Tree Int, Int)
    helper c (Leaf x) = (Leaf c, c + 1)
    helper c (Node l r) =
      let (l', c') = helper c l
          (r', c'') = helper c' r
      in (Node l' r', c'')



newtype State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) = f

{-
class Functor f => Applicative f
class Applicative m => Monad m
-}

instance Functor (State s) where
  -- fmap = liftM
  fmap :: (a -> b) -> State s a -> State s b
  fmap f m = do
    a <- m
    return (f a)

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return :: a -> State s a
  return a = State $ \s -> (a, s)

  (>>=) :: State s a -> (a -> State s b) -> State s b
  State g >>= f = State $ \s ->
    let (a, s') = g s
        (b, s'') = runState (f a) s'
    in (b, s'')

relabel2 :: Tree a -> Tree Int
relabel2 t = fst $ runState (helper t) 0
  where
    helper :: Tree a -> State Int (Tree Int)
    helper (Leaf x) = State $ \c -> (Leaf c, c + 1)
    helper (Node l r) = do
      l' <- helper l
      r' <- helper r
      return $ Node l' r'


get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

relabel3 :: Tree a -> Tree Int
relabel3 t = fst $ runState (helper t) 0
  where
    helper :: Tree a -> State Int (Tree Int)
    helper (Leaf x) = do
      c <- get
      put $ c + 1
      return $ Leaf c
    helper (Node l r) = do
      l' <- helper l
      r' <- helper r
      return $ Node l' r'



{-
instance Monad IO
-}

cat :: IO ()
cat = do
  s <- getLine
  putStrLn s

-- getLine :: IO String
-- putStrLn :: String -> IO ()





safeDigitToInt :: Char -> Maybe Int
safeDigitToInt c
  | isDigit c = Just $ digitToInt c
  | otherwise = Nothing

-- "1245" -> Just [1,2,4,5]
-- "34asd" -> Nothing
digitsToInts :: String -> Maybe [Int]
digitsToInts [] = return []
digitsToInts (c:cs) = do
  n <- safeDigitToInt c
  ns <- digitsToInts cs
  return (n:ns)


-- [1,2,4,6] -> [1,3,7,13]
cumSum :: [Int] -> [Int]
cumSum xs = fst $ runState (helper xs) 0
  where
    helper :: [Int] -> State Int [Int]
    helper [] = return []
    helper (x:xs) = do
      c <- get
      let y = c + x
      put y             -- _ <- put y
      ys <- helper xs
      return (y:ys)



mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = do
  y <- f x
  ys <- mapM' f xs
  return (y:ys)

digitsToInts2 :: String -> Maybe [Int]
digitsToInts2 s = mapM safeDigitToInt s

cumSum2 :: [Int] -> [Int]
cumSum2 xs = fst $ runState (mapM action xs) 0
  where
    action :: Int -> State Int Int
    action x = do
      c <- get
      let y = c + x
      put y
      return y



{-
Functor laws:
fmap id x == x
fmap f (fmap g x) == fmap (f . g) x




Monad laws:
m >>= return == m
return x >>= f == f x
(m >>= f) >>= g == m >>= (\x -> f x >>= g)



return x >>= f == f x
{-
do
  y <- return x
  f y
==
  f x
-}



(m >>= f) >>= g == m >>= (\x -> f x >>= g)
{-
do
  y <- do
    x <- m
    f x
  g y
==
do
  x <- m
  y <- f x
  g y
-}


-}
