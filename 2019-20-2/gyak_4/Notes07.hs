{-# LANGUAGE MonadComprehensions #-}
module Notes07 where

data Expr = Value Int
          | Add Expr Expr
          | Sub Expr Expr
          deriving (Show, Eq, Ord)

e1 :: Expr
e1 = Value 10

e2 :: Expr
e2 = Value 10 `Add` (Value 5 `Sub` Value 7)

-- Define `evalExpr` !
--   evalExpr e1 == 10
--   evalExpr e2 == 10 + (5 - 7)
evalExpr :: Expr -> Int
evalExpr (Value x) = x
evalExpr (Add x y) = evalExpr x + evalExpr y
evalExpr (Sub x y) = evalExpr x - evalExpr y

data Expr2 = Value2 Int
           | Add2 Expr2 Expr2
           | Sub2 Expr2 Expr2
           | Div2 Expr2 Expr2
           deriving (Show, Eq, Ord)

e3 :: Expr2
e3 = (Value2 10 `Add2` Value2 9) `Div2` (Value2 5 `Sub2` Value2 5)

evalExpr2 :: Expr2 -> Int
evalExpr2 (Value2 x) = x
evalExpr2 (Add2 x y) = evalExpr2 x + evalExpr2 y
evalExpr2 (Sub2 x y) = evalExpr2 x - evalExpr2 y
evalExpr2 (Div2 x y) = let y' = evalExpr2 y in
                         if y' == 0
                         then error "divide by zero"
                         else evalExpr2 x `div` evalExpr2 y

e4 :: Expr2
e4 = Value2 1 `Add2` (Value2 2 `Div2` Value2 0)

-- Define evalExpr2Maybe
--   It should return Nothing if the expression contains a division by zero.
--   evalExpr2Maybe e3 == Nothing
--   evalExpr2Maybe e4 == Nothing
evalExpr2Maybe :: Expr2 -> Maybe Int
evalExpr2Maybe (Value2 x) = Just x
evalExpr2Maybe (x `Add2` y) = case (evalExpr2Maybe x, evalExpr2Maybe y) of
  (Just a, Just b) -> Just (a + b)
  _                -> Nothing
evalExpr2Maybe (x `Sub2` y) = case (evalExpr2Maybe x, evalExpr2Maybe y) of
  (Just a, Just b) -> Just (a - b)
  _                -> Nothing
evalExpr2Maybe (x `Div2` y) = case (evalExpr2Maybe x, evalExpr2Maybe y) of
  (Just a, Just b) | b /= 0 -> Just (a `div` b)
  _                         -> Nothing


bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe f x' = case x' of
  Just a -> f a
  _      -> Nothing

-- in Prelude:
-- bindMaybe f x = x >>= f

-- bindMaybe f Nothing  = Nothing
-- bindMaybe f (Just a) = f a


-- Define evalExpr2Maybe using bindMaybe,
--   or monad comprehensions,
--   or do notation.
evalExpr2Maybe' :: Expr2 -> Maybe Int
evalExpr2Maybe' (Value2 x) = Just x
evalExpr2Maybe' (x `Div2` y) =
  evalExpr2Maybe' x >>= \a ->
  evalExpr2Maybe' y >>= \b ->
  if b /= 0
  then Just (a `div` b)
  else Nothing

evalExpr2Maybe'' :: Expr2 -> Maybe Int
evalExpr2Maybe'' (Value2 x) = Just x
evalExpr2Maybe'' (x `Add2` y) = do
  a <- evalExpr2Maybe' x
  b <- evalExpr2Maybe' y
  return (a + b)
evalExpr2Maybe'' (x `Sub2` y) = do
  a <- evalExpr2Maybe' x
  b <- evalExpr2Maybe' y
  return (a - b)
evalExpr2Maybe'' (x `Div2` y) = do
  a <- evalExpr2Maybe' x
  b <- evalExpr2Maybe' y
  if b /= 0
    then Just (a `div` b)
    else Nothing

-- This needs to be at the top of the file: {-# LANGUAGE MonadComprehensions #-}
evalExpr2Maybe''' :: Expr2 -> Maybe Int
evalExpr2Maybe''' (Value2 x) = Just x
evalExpr2Maybe''' (x `Add2` y) = [ a + b | a <- evalExpr2Maybe''' x, b <- evalExpr2Maybe''' y]
evalExpr2Maybe''' (x `Sub2` y) = [ a - b | a <- evalExpr2Maybe''' x, b <- evalExpr2Maybe''' y]
evalExpr2Maybe''' (x `Div2` y) = [ a `div` b
                                 | a <- evalExpr2Maybe' x
                                 , b <- evalExpr2Maybe' y
                                 , b /= 0 ]
