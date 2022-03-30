{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module Lesson07 where

import Text.Read

replicate' :: Int -> a -> [a]
replicate' n a
    | n <= 0 = []
    | otherwise = a : replicate' (n-1) a

replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' n ma
    | n <= 0    = pure []
    | otherwise = do
        a <- ma
        as <- replicateM' (n-1) ma
        pure $ a : as

replicateM'' :: Monad m => Int -> m a -> m [a]
replicateM'' n ma
    | n <= 0    = pure []
    | otherwise = ma >>= (\a -> replicateM'' (n-1) ma >>= (\as -> pure (a : as)))
-- promptUntilCorrect, replicateM, replicateM_, numOfCharsInNLines

type PromptMessage = String
type PredicateWrongMessage = String
type ParseErrorMessage = String

promptUntilCorrect :: Read a 
                   => PromptMessage
                   -> (a -> Bool) 
                   -> PredicateWrongMessage
                   -> ParseErrorMessage
                   -> IO a
promptUntilCorrect pm p pwm pem = do
    putStr pm 
    x <- getLine
    case readMaybe x of
        Nothing -> do 
            putStrLn pem 
            promptUntilCorrect pm p pwm pem
        Just a -> case p a of
            True -> pure a
            False -> do
                putStrLn pwm
                promptUntilCorrect pm p pwm pem

promptUntilCorrect' :: Read a 
                   => PromptMessage
                   -> (a -> Bool) 
                   -> PredicateWrongMessage
                   -> ParseErrorMessage
                   -> IO a
promptUntilCorrect' pm p pwm pem = do
    putStr pm 
    x <- getLine
    case readMaybe x of
        Nothing -> do 
            putStrLn pem 
            promptUntilCorrect' pm p pwm pem
        Just a | p a -> pure a
        _ -> do
            putStrLn pwm
            promptUntilCorrect' pm p pwm pem
-- State: runState, Functor, Applicative, Monad, evalState, execState, get, put, modify

-- Definiálj egy függvényt, ami kicseréli egy fa leveleiben tárolt értékeket
-- balról jobbra haladva egy megadott lista elemeire. Használj State monádot!

{-
newtype State s a = State (s -> (s,a))

runState :: State s a -> (s -> (s,a))
runState (State f) = f
-}
newtype State s a = State {runState :: s -> (s,a)}

evalState :: State s a -> s -> a
evalState (State f) = snd . f

execState :: State s a -> s -> s
execState (State f) = fst . f

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    -- (a -> b) -> (s -> (s,a)) -> (s -> (s,b))
    fmap f (State g) = State $ \s -> let (s',a) = g s in (s', f a)

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State $ \s -> (s,a)

    (<*>) :: State s (a -> b) -> State s a -> State s b
    -- f :: s -> (s,a -> b)
    -- g :: s -> (s,a)
    (State f) <*> (State g) = State $ \s ->
        let (s',aToB) = f s; (s'',a) = g s' in (s'',aToB a)

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Show, Foldable)

-- pl: replaceLeaves [10, 20, 30] (   Node (Leaf 2) (Leaf 3))
--                                 == Node (Leaf 10) (Leaf 20)
--     replacereplaceLeaves [5] (Leaf 10) == Leaf 5
--     replacereplaceLeaves [5]
--        (Node (Leaf 0) (Node (Leaf 0) (Leaf 0))) ==
--        (Node (Leaf 5) (Node (Leaf 0) (Leaf 0)))

replaceLeaves :: [a] -> Tree a -> Tree a
replaceLeaves = undefined


-- Definiáld a függvényt, ami megfordítja a fa leveleiben tárolt értékek
-- sorrendjét!  tipp: használhatod a replaceLeaves függvényt.
reverseElems :: Tree a -> Tree a
reverseElems = undefined