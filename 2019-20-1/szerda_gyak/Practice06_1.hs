{-# LANGUAGE InstanceSigs #-}
module Practice06_1 where

import Control.Monad

newtype State s a = State (s -> (a,s))

instance Functor (State s) where 
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State $ \s -> let (x, s') = g s in (f x, s')
    -- State $ \s -> (f (fst $ g s), snd $ g s)

instance Applicative (State s) where 
  pure  = return
  (<*>) = ap

instance Monad (State s) where 
  return :: a -> State s a 
  return x = State $ \s -> (x,s) 
  
  (>>=) :: State s a -> (a -> State s b) -> State s b 
  (>>=) (State f) k = State $ \s -> let (x, s')   = f s  in 
                                    let (State g) = k x  in
                                        g s'

runState :: s -> State s a -> (a,s)
runState s (State f) = f s 

evalState :: s -> State s a -> a 
evalState s = fst . runState s 

execState :: s -> State s a -> s 
execState s = snd . runState s

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = do 
  s <- get 
  let s' = f s 
  put s'

data BinTree a
  = Nil 
  | Branch a (BinTree a) (BinTree a)
  deriving (Eq, Ord, Show)

numberNodes :: BinTree a -> BinTree (Int, a)
numberNodes = fst . numberNodes' 0  where 
  numberNodes' :: Int -> BinTree a -> (BinTree (Int, a), Int)
  numberNodes' n Nil = (Nil, n) 
  numberNodes' n (Branch x l r) = (Branch (n,x) l' r', n'') where 
    (l', n')  = numberNodes' (n+1) l
    (r', n'') = numberNodes' n'    r

reverseList :: [a] -> [a]
reverseList xs = evalState (Stack []) $ do 
  pushAllM xs
  popAllM

reverseList' :: [a] -> [a]
reverseList' xs = unStack $ execState (Stack []) $ pushAllM xs

newtype Stack a = Stack { unStack :: [a] } 
  deriving (Eq, Ord, Show) 

push :: a -> Stack a -> Stack a 
push x (Stack xs) = Stack (x:xs) 

top :: Stack a -> Maybe a 
top (Stack [])     = Nothing
top (Stack (x:xs)) = Just x

pop :: Stack a -> Stack a
pop (Stack [])     = Stack []
pop (Stack (x:xs)) = Stack xs

pushM :: a -> State (Stack a) ()
pushM x = modify (push x)
-- pushM x = do 
--   s <- get 
--   let s' = push x s
--   put s'

popM :: State (Stack a) (Maybe a)
popM = do
  s <- get 
  let x  = top s 
      s' = pop s 
  put s' 
  pure x

pushAllM :: [a] -> State (Stack a) [a]
pushAllM [] = pure [] 
pushAllM (x:xs) = do 
  pushM x 
  pushAllM xs 

popAllM :: State (Stack a) [a]
popAllM = do 
  mX <- popM 
  case mX of 
    Nothing -> pure [] 
    Just  x -> do 
      xs <- popAllM
      pure (x:xs)   