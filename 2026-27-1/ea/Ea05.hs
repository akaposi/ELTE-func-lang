module Ea05 where

import Data.List
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

-- []

{-
instance Monad [] where
  return :: a -> [a]
  return a = [a]

  (>>=) :: [a] -> (a -> [b]) -> [b]
  xs >>= f = concat $ map f xs

  xs >>= f = [y | x <- xs, y <- f x]
-}

type Nondet = []

data Flip = Heads | Tails
  deriving (Show, Eq)

flipCoin :: Nondet Flip
flipCoin = [Heads, Tails]

flipTwice :: Nondet (Flip, Flip)
flipTwice = do
  flip1 <- flipCoin
  flip2 <- flipCoin
  return (flip1, flip2)

-- flipTwice = [(flip1, flip2) | flip1 <- flipCoin, flip2 <- flipCoin]

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m
  | n <= 0 = return []
  | otherwise = do
    a <- m
    as <- replicateM (n - 1) m
    return (a:as)


guard :: Bool -> Nondet ()
guard b = if b then return () else []

-- all possible five flips where exactly 2 are heads
example :: Nondet [Flip]
example = do
  flips <- replicateM 5 flipCoin
  -- if length (filter (== Heads) flips) == 2
  --   then return flips
  --   else []
  guard $ length (filter (== Heads) flips) == 2
  return flips

-- example =
--   [flips | flips <- replicateM 5 flipCoin,
--            length (filter (== Heads) flips) == 2]





-- knight's tour
type Coord = (Int, Int)
type Size = (Int, Int)
type Path = [Coord]
type RevPath = [Coord]

inBound :: Size -> Coord -> Bool
inBound (width, height) (x, y) =
  0 <= x && x < width && 0 <= y && y < height

knightMove :: Coord -> Nondet Coord
knightMove (x, y) = do
  dx <- [-2, -1, 1, 2]
  dy <- [-2, -1, 1, 2]
  guard $ abs dx + abs dy == 3
  return (x + dx, y + dy)

knightMoveValid :: Size -> RevPath -> Nondet Coord
knightMoveValid size path@(curr:past) = do
  next <- knightMove curr
  guard $ inBound size next
  guard $ next `notElem` path
  return next

knightMoveSorted :: Size -> RevPath -> Nondet Coord
knightMoveSorted size path =
  sortOn (\next -> length $ knightMoveValid size (next:path)) $
    knightMoveValid size path

tour :: Size -> Coord -> Path
tour size@(width, height) init = reverse $ head $ helper [init]
  where
    helper :: RevPath -> Nondet RevPath
    helper path@(curr:past)
      | length path == width * height = return path
      | otherwise = do
        next <- knightMoveSorted size path
        helper (next:path)





data Expr
  = Lit Int
  | Add Expr Expr
  | Div Expr Expr
  deriving (Show)

example1 :: Expr
example1 = Div (Lit 25) (Div (Lit 4) (Lit 2))

example2 :: Expr
example2 = Div (Lit 5) (Div (Lit 1) (Lit 2))

-- instance Monad Maybe
{-
instance Monad (Either e) where
  return :: a -> Either e a
  return a = Right a

  (>>=) :: Either e a -> (a -> Either e b) -> Either e b
  Left e >>= f = Left e
  Right a >>= f = f a

-}

safeDiv :: Int -> Int -> Either String Int
safeDiv x y
  | y == 0 = Left $ "division of " ++ show x ++ " with zero"
  | otherwise = return (x `div` y)

eval :: Expr -> Either String Int
eval (Lit n) = return n
eval (Add e1 e2) = do
  x <- eval e1
  y <- eval e2
  return (x + y)
eval (Div e1 e2) = do
  x <- eval e1
  y <- eval e2
  safeDiv x y

evalShowError :: Expr -> IO ()
evalShowError e = case eval e of
  Left s -> putStrLn s
  Right n -> print n





{-
instance Monad ((->) r) where  -- (r -> _)
  return :: a -> (r -> a)
  return a = \_ -> a

  (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
  f >>= g = \r -> g (f r) r
-}

data Expr'
  = Lit' Int
  | Add' Expr' Expr'
  | Var String
  | Let String Expr' Expr' -- let x = e1 in e2
  deriving (Show)

example3 :: Expr'
example3 = Add' (Lit' 3) (Let "x" (Lit' 2) (Add' (Lit' 5) (Var "x")))
-- 3 + (let x = 2 in 5 + x)

example4 :: Expr'
example4 =
  Add' (Lit' 3)
    (Let "x" (Lit' 2) (Add' (Let "x" (Lit' 5) (Var "x")) (Var "x")))
-- 3 + (let x = 2 in (let x = 5 in x) + x)

eval' :: Expr' -> Int
eval' e = fst $ runState (helper e) []
  where
    helper :: Expr' -> State [(String, Int)] Int
    helper (Lit' n) = return n
    helper (Add' e1 e2) = do
      x <- helper e1
      y <- helper e2
      return (x + y)
    helper (Var v) = do
      vars <- get
      case lookup v vars of
        Nothing -> return 0
        Just x -> return x
    helper (Let v e1 e2) = do
      x <- helper e1
      vars <- get
      put $ (v,x) : vars
      y <- helper e2
      put vars
      return y


-- newtype Reader r a = Reader (r -> a)
-- runReader :: Reader r a -> r -> a
{-
ask :: Reader r r
ask = Reader $ \r -> r

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader g) = Reader $ \r -> g (f r)
-}


eval'' :: Expr' -> Int
eval'' e = runReader (helper e) []
  where
    helper :: Expr' -> Reader [(String, Int)] Int
    -- helper :: Expr' -> [(String, Int)] -> Int
    helper (Lit' n) = return n
    helper (Add' e1 e2) = do
      x <- helper e1
      y <- helper e2
      return (x + y)
    helper (Var v) = do
      vars <- ask
      case lookup v vars of
        Nothing -> return 0
        Just x -> return x
    helper (Let v e1 e2) = do
      x <- helper e1
      y <- local (\vars -> (v, x) : vars) $ helper e2
      return y



{-
-- (,) :: Type -> Type -> Type
-- Monad :: (Type -> Type) -> Constraint

-- () :: ()
-- data Unit = MkUnit
-- MkUnit :: Unit

instance Monoid w => Monad ((,) w) where
  return :: a -> (w, a)
  return a = (mempty, a)

  (>>=) :: (w, a) -> (a -> (w, b)) -> (w, b)
  (w, a) >>= f = (w <> w', b)
    where
      (w', b) = f a

-- (w, a) >>= return == (w <> mempty, a) == (w, a)
-- return a >>= f == f a
-}

-- newtype Writer w a = Writer (a, w)
-- runWriter :: Writer w a -> (a, w)
-- tell :: w -> Writer w a

data ExprA
  = LitA Int
  | AddA ExprA ExprA
  deriving (Show)

-- logging

evalA :: ExprA -> Writer [String] Int
evalA (LitA n) = return n
evalA (AddA e1 e2) = do
  x <- evalA e1
  y <- evalA e2
  tell $ ["Adding " ++ show x ++ " and " ++ show y]
  return (x + y)


evalA' :: ExprA -> (Int, [String])
evalA' (LitA n) = (n, [])
evalA' (AddA e1 e2) =
  let
    (x, log1) = evalA' e1
    (y, log2) = evalA' e2
  in
    (x + y, log1 ++ log2 ++ ["Adding " ++ show x ++ " and " ++ show y])

example5 :: ExprA
example5 = AddA (AddA (LitA 1) (LitA 2)) (AddA (LitA 3) (LitA 4))



-- difference list
-- type DList a = [a] -> [a]
{-
instance Monoid (DList a) where
  mempty = id
  f <> g = f . g
-}
-- singleton :: a -> DList a
-- singleton x = \xs -> x:xs

-- evalA :: ExprA -> Writer (DList String) Int

-- writer can be problematic with the wrong `w`
-- -- also lazy writer is bad
