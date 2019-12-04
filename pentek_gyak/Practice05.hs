module Practice05 where

import Control.Monad.State

-- reverseList :: [a] -> [a]
-- reverseList [] = []
-- reverseList (x:xs) = reverseList xs ++ [x]

newtype Stack a = Stack { unStack :: [a] }
  deriving (Eq, Ord, Show)

push :: a -> Stack a -> Stack a 
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> Stack a 
pop (Stack [])     = Stack []
pop (Stack (x:xs)) = Stack xs

top :: Stack a -> Maybe a 
top (Stack [])    = Nothing
top (Stack (x:_)) = Just x

reverseList :: [a] -> [a]
reverseList xs = revList [] xs where 
  revList :: [a] -> [a] -> [a]
  revList acc [] = acc 
  revList acc (x:xs) = revList (x:acc) xs
  
get' :: State s s
get' = undefined

put' :: s -> State s () 
put' = undefined

runState' :: State s a -> s -> (a, s)
runState' = undefined

evalState' :: State s a -> s -> a
evalState' = undefined 

execState' :: State s a -> s -> s 
execState' = undefined

count :: [a] -> Int 
count []     = 0
count (_:xs) = 1 + count xs

countM :: [a] -> State Int Int 
countM xs = do
  c <- get  
  if null xs then do
    pure c 
  else do
    put (c+1)
    countM (tail xs)

modify' :: (s -> s) -> State s () 
modify' f = do 
  s <- get 
  let s' = f s
  put s' 

pushM :: a -> State (Stack a) () 
pushM x = modify (push x)
  -- do  
  -- s <- get
  -- let s' = push x s 
  -- put s'

-- state
popM :: State (Stack a) (Maybe a)
popM = do 
  s <- get 
  let s' = pop s
      mX = top s 
  put s'
  pure mX

-- topM :: State (Stack a) (Maybe a)

popAllM :: State (Stack a) [a]
popAllM = do 
  mX <- popM
  case mX of 
    Nothing -> pure [] 
    Just x  -> do 
      xs <- popAllM
      pure (x:xs) 

pushAllM :: [a] -> State (Stack a) ()
pushAllM [] = pure ()
pushAllM (x:xs) = do 
  pushM x
  pushAllM xs 

reverseListM :: [a] -> State (Stack a) [a]
reverseListM xs = do 
  pushAllM xs 
  popAllM 

reverseList' :: [a] -> [a]
reverseList' xs = evalState (reverseListM xs) (Stack [])

-- State'
newtype State' s a = State' (s -> (a,s))

-- Functor :: * -> *
instance Functor (State' s) where 
  -- fmap :: (a -> b) -> State' s a -> State' s b 
  fmap f (State' g) = State' $ \s -> let (x,   s') = g s in 
                                         (f x, s')