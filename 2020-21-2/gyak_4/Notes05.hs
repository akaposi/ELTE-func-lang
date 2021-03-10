{-# options_ghc -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DeriveFunctor #-}
module Notes05 where

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
expr4 = Div (Value 1) (Plus (Value 10) (Value (-10)))

-- Define `evalIntExpr :: IntExpr -> Int`
-- Examples: 
--   evalIntExpr expr1 == 15
--   evalIntExpr expr2 == 2
--   evalIntExpr expr3 == ???
--   evalIntExpr expr4 == ???

evalIntExpr :: IntExpr -> Int
evalIntExpr = undefined

-- Define `evalIntExprMaybe :: IntExpr -> Maybe Int`
-- Examples: 
--   evalIntExprMaybe expr1 == Just 15
--   evalIntExprMaybe expr2 == Just 2
--   evalIntExprMaybe expr3 == Nothing
--   evalIntExprMaybe expr4 == Nothing
-- Hint: first define
--   safeDiv :: Int -> Int -> Maybe Int
evalIntExprMaybe :: IntExpr -> Maybe Int
evalIntExprMaybe = undefined

-- General monadic operations (in Control.Monad)
liftM :: Monad m => (a -> b) -> m a -> m b
liftM = undefined

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 = undefined

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 = undefined

forever :: Monad m => m a -> m b
forever = undefined

mapM :: Monad m => (a -> m b) -> [a] -> m [a]
mapM = undefined

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM = undefined

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

