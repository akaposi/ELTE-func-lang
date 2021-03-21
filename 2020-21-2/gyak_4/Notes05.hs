{-# options_ghc -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DeriveFunctor #-}
module Notes05 where
import Prelude hiding (mapM)

-- Evaluation of expressions
data IntExpr = Value Int
             | Plus  IntExpr IntExpr
             | Div   IntExpr IntExpr
             deriving (Show, Eq, Ord)

expr1 :: IntExpr
expr1 = Value 10 `Plus` Value 5 -- (10 + 5)

expr2 :: IntExpr
expr2 = Value 10 `Div` Value 5 -- (10 `div` 5)

expr3 :: IntExpr
expr3 = Value 10 `Div` Value 0 -- (10 `div` 0)

expr4 :: IntExpr
expr4 = Div (Value 1) (Plus (Value 10) (Value (-10))) -- 1 / (10 - 10)

-- Define `evalIntExpr :: IntExpr -> Int`
-- Examples: 
--   evalIntExpr expr1 == 15
--   evalIntExpr expr2 == 2
--   evalIntExpr expr3 == ???
--   evalIntExpr expr4 == ???

evalIntExpr :: IntExpr -> Int
evalIntExpr (Value x)  = x
evalIntExpr (Plus x y) = evalIntExpr x + evalIntExpr y
evalIntExpr (Div x y)  = evalIntExpr x `div` evalIntExpr y

-- Define `evalIntExprMaybe :: IntExpr -> Maybe Int`
-- Examples: 
--   evalIntExprMaybe expr1 == Just 15
--   evalIntExprMaybe expr2 == Just 2
--   evalIntExprMaybe expr3 == Nothing
--   evalIntExprMaybe expr4 == Nothing

-- Hint: first define
--   safeDiv :: Int -> Int -> Maybe Int

evalIntExprMaybe :: IntExpr -> Maybe Int
evalIntExprMaybe (Value x)  = pure x -- undefined -- x
-- evalIntExprMaybe (Plus x y) = case (evalIntExprMaybe x, evalIntExprMaybe y) of
--   (Just x', Just y') -> Just (x' + y')
--   _                  -> Nothing 

-- evalIntExprMaybe (Plus x y) = do
--   x' <- evalIntExprMaybe x
--   y' <- evalIntExprMaybe y
--   pure (x' + y')

evalIntExprMaybe (Plus x y) = liftM2 (+) (evalIntExprMaybe x) (evalIntExprMaybe y)
evalIntExprMaybe (Div x y)  = do
  x' <- evalIntExprMaybe x
  y' <- evalIntExprMaybe y
  safeDiv x' y'

safeDiv :: Integral a => a -> a -> Maybe a
safeDiv x y = if y == 0 then Nothing else Just (x `div` y)  

-- General monadic operations (in Control.Monad)


liftM :: Monad m => (a -> b) -> m a -> m b
liftM f x = fmap f x

-- liftM f x = do 
--   x' <- x
--   return (f x')

-- liftM f x = f <$> x

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f x y = do
  x' <- x
  y' <- y
  return (f x' y')

-- liftM2 f x y = (f <$> x) <*> y
-- f <$> x  :: m (b -> c)
-- (<*>) :: m (b -> c) -> m b -> mc
-- f <$> x <*> y :: m c

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f x y z = f <$> x <*> y <*> z

-- forever ma  in Haskell   ~    while(true) { ma }   in C / ...
forever :: Monad m => m a -> m b
forever ma = do
  ma
  forever ma

-- whileM cond ma        ~    while(cond) { ma }
whileM :: Monad m => m Bool -> m a -> m [a]
whileM cond ma = do
  b <- cond 
  case b of
    False -> pure []
    True  -> do
      a  <- ma
      as <- whileM cond ma
      pure (a:as)

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f []     = pure []
mapM f (x:xs) = (:) <$> f x <*> mapM f xs

-- forM xs f       ~    for(x in xs) { f x }    in C / ...
forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM = flip mapM

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f e [] = e
foldr' f e (x:xs) = 
  let b = foldr' f e xs
  in f x b

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM f e []     = pure e
foldrM f e (x:xs) = do
  b <- foldrM f e xs
  f x b

-- Other monad exercises:
f2 :: Monad m => m a -> m b -> m (a, b)
f2 = undefined

f3 :: Monad m => m (m a) -> m a
f3 = undefined

f4 :: Monad m => m (a -> b) -> m a -> m b
f4 = undefined

f5 :: Monad m => (a -> m b) -> m a -> m b
f5 = undefined

f6 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
f6 = undefined

f7 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
f7 = undefined

f8 :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f8 = undefined

