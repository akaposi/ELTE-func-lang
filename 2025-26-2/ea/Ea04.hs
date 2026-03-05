module Ea04 where

import Data.List
import Control.Monad

-- monads

-- examples

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

-- applyOp :: Op -> Int -> Int -> Int
-- applyOp Add x y = x + y
-- applyOp Sub x y = x - y
-- applyOp Mul x y = x * y
-- applyOp Div x y
--   | y == 0 = error "cannot divide by zero"
--   | otherwise = x `div` y

-- calc :: Expr -> Int
-- calc (Lit x) = x
-- calc (Op op e1 e2) = applyOp op (calc e1) (calc e2)

-- printCalc :: Expr -> String
-- printCalc e = show (calc e)

applyOp :: Op -> Int -> Int -> Maybe Int
applyOp Add x y = Just $ x + y
applyOp Sub x y = Just $ x - y
applyOp Mul x y = Just $ x * y
applyOp Div x y
  | y == 0 = Nothing
  | otherwise = Just $ x `div` y

calc :: Expr -> Maybe Int
calc (Lit x) = Just x
calc (Op op e1 e2) =
  case calc e1 of
    Nothing -> Nothing
    Just x -> case calc e2 of
      Nothing -> Nothing
      Just y -> applyOp op x y

printCalc :: Expr -> String
printCalc e = case calc e of
  Nothing -> "cannot divide by zero"
  Just x -> show x


andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing f = Nothing
andThen (Just x) f = f x

calc' :: Expr -> Maybe Int
calc' (Lit x) = Just x
calc' (Op op e1 e2) =
  andThen (calc' e1) $ \x ->
    andThen (calc' e2) $ \y ->
      applyOp op x y


data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

-- data Tree' a = Leaf a | Node (Tree a) (Tree a)
--   deriving (Show)

-- data Rational = Integer :% Integer
recip' :: Rational -> Maybe Rational
recip' x
  | x == 0 = Nothing
  | otherwise = Just (1 / x)

-- take the reciprocal of all elements of the tree
recipTree :: Tree Rational -> Maybe (Tree Rational)
recipTree Leaf = Just Leaf
recipTree (Node l x r) =
  andThen (recipTree l) $ \l' ->
    andThen (recip' x) $ \x' ->
      andThen (recipTree r) $ \r' ->
        Just (Node l' x' r')

  -- case recipTree l of
  --   Nothing -> Nothing
  --   Just l' -> case recip' x of
  --     ...

exampleTree :: Tree Rational
exampleTree = Node (Node Leaf 2 (Node Leaf (1 / 3) Leaf)) 10 (Node Leaf 2.5 Leaf)

exampleTree2 :: Tree String
exampleTree2 =
  Node (Node Leaf "hello" (Node Leaf "c" Leaf)) "a" (Node Leaf "c" Leaf)

-- relabel exampleTree2
-- = Node (Node Leaf 0 (Node Leaf 1 Leaf)) 2 (Node Leaf 1 Leaf)

-- replace each element with an Int
-- identical elements are replaced with the same integer

-- wrong:
-- relabel :: Eq a => Tree a -> Tree Int
-- relabel t = helper [] t
--   where
--     helper :: Eq a => [(a, Int)] -> Tree a -> Tree Int
--     helper acc Leaf = Leaf
--     helper acc (Node l x r) = case lookup x acc of
--       Nothing ->
--         Node
--           (helper ((x, length acc) : acc) l)
--           (length acc)
--           (helper ((x, length acc) : acc) r)
--       Just n -> Node (helper acc l) n (helper acc r)

relabel :: Eq a => Tree a -> Tree Int
relabel t = fst $ helper [] t
  where
    helper :: Eq a => [(a, Int)] -> Tree a -> (Tree Int, [(a, Int)])
    helper acc Leaf = (Leaf, acc)
    helper acc (Node l x r) =
      let (l', acc1) = helper acc l
          (x', acc2) = case lookup x acc1 of
            Nothing -> (length acc1, (x, length acc1) : acc1)
            Just n -> (n, acc1)
          (r', acc3) = helper acc2 r
      in (Node l' x' r', acc3)

type State' s a = s -> (a, s)

andThen2 :: State' s a -> (a -> State' s b) -> State' s b
andThen2 f g s =
  let (a, s1) = f s
      (b, s2) = g a s1
  in (b, s2)

noChange :: a -> State' s a
noChange a s = (a, s)

get' :: State' s s
get' s = (s, s)

-- data () = ()

put' :: s -> State' s ()
put' s' s = ((), s')

relabel' :: Eq a => Tree a -> Tree Int
relabel' t = fst $ helper t []
  where
    -- helper :: Eq a => Tree a -> [(a, Int)] -> (Tree Int, [(a, Int)])
    helper :: Eq a => Tree a -> State' [(a, Int)] (Tree Int)
    helper Leaf = noChange Leaf
    helper (Node l x r) =
      andThen2 (helper l) $ \l' ->
        andThen2 checkElem $ \x' ->
          andThen2 (helper r) $ \r' ->
            noChange (Node l' x' r')
      where
        checkElem =
          andThen2 get' $ \acc ->
            case lookup x acc of
              Nothing ->
                andThen2 (put' $ (x, length acc) : acc) $ \_ ->
                  noChange (length acc)
              Just n -> noChange n


{-
-- actually Applicative m => Monad m
class Functor m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

instance Monad Maybe where
  return = Just
  Nothing >>= f = Nothing
  Just x >>= f = f x
-}

newtype State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) = f

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  State f >>= g = State $ \s ->
    let (a, s') = f s in runState (g a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s' = State $ \_ -> ((), s')

calc'' :: Expr -> Maybe Int
calc'' (Lit x) = return x
calc'' (Op op e1 e2) =
  calc'' e1 >>= \x ->
    calc'' e2 >>= \y ->
      applyOp op x y

calc''' :: Expr -> Maybe Int
calc''' (Lit x) = return x
calc''' (Op op e1 e2) = do
  x <- calc''' e1
  y <- calc''' e2
  applyOp op x y

relabel'' :: Eq a => Tree a -> Tree Int
relabel'' t = fst $ runState (helper t) []
  where
    helper :: Eq a => Tree a -> State [(a, Int)] (Tree Int)
    helper Leaf = return Leaf
    helper (Node l x r) = do
      l' <- helper l
      x' <- checkElem
      r' <- helper r
      return $ Node l' x' r'
      where
        checkElem = do
          acc <- get
          case lookup x acc of
            Nothing -> do
              put $ (x, length acc) : acc
              return $ length acc
            Just n -> return n

{-
-- IO
instance Monad IO
-}

cat :: IO ()
cat = do
  line <- getLine
  putStrLn line
