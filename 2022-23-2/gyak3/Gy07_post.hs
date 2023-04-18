{-# language InstanceSigs, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# options_ghc -Wincomplete-patterns #-}

module Gy07 where

import Control.Monad
import Debug.Trace


{- State -}

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))
  State f >>= g = State (\s -> case f s of (a, s') -> runState (g a) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do { st <- get ; put (f st) }

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma


{- Stack -}

-- Add an element to the top of the stack by extending the state list!
push :: a -> State [a] ()
push a = modify (a:)

-- Remove and return the top element of the stack if it is not empty!
pop :: State [a] (Maybe a)
pop = do
  st <- get
  case st of
    [] -> return Nothing
    (a:as) -> do
      put as
      return (Just a)

-- Írj egy függvényt, ami kizárólag push, pop és rekurzió felhasználásával
-- map-eli az állapot listát.
mapPushPop :: (a -> a) -> State [a] ()
mapPushPop f = do
  ma <- pop
  case ma of
    Nothing -> return ()
    Just a -> do
      mapPushPop f
      push (f a)



-- Trace + Memoization
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib' :: Int -> Int
fib' = (map fib' [0..] !!)
  where
    fib'' :: Int -> Int
    fib'' 0 = 0
    fib'' 1 = 1
    fib'' n = fib' (n - 1) + fib' (n - 2)


-- type Traversable :: (* -> *) -> Constraint
-- class (Functor t, Foldable t) => Traversable t where


--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- traverse (Just) [1,2,3]
-- a :: Int
-- (a -> f b) :: Int -> Maybe Int
-- b :: Int
-- f :: Maybe
-- t :: []

-- traverse id [Right 1, Right 2, Right 3]
-- id :: a -> a
-- a :: Either Int
-- f b :: Either Int
-- (a -> f b) :: Either Int -> Either Int
-- b :: Int
-- f :: Either
-- t :: []

-- traverse :: sequence . fmap


-- [1,4,3,2,3,15,4,6,...] - [1,4,4,4,4,15,15,15,15,15]
-- Írj egy függvényt, ami egy Int listában minden elemet kicserél az addigi
-- elemek maximumára.  Tegyük fel, hogy a lista nem-negatív számokat tartalmaz.
-- Használd a (State Int)-et. Legyen az állapot a jelenlegi maximális Int.
maxs :: [Int] -> [Int]
maxs ns = evalState (go ns) 0 where
  go :: [Int] -> State Int [Int]
  go [] = return []
  go (a:as) = do
    m <- get
    if m > a
      then do
        as' <- go as
        return (m : as')
      else do
        put a
        as' <- go as
        return (a : as')

    -- go (a:as) = do
    --   m <- get
    --   when (m < a) (put a)
    --   a' <- get
    --   as' <- go as
    --   return (a' : as')


    -- let rM = max a m
    -- put rM
    -- as' <- go as
    -- return (rM : as')

-- Traverse
-- go :: [Int] -> State Int [Int]

-- [1,2,7,4,2,10,8,9,15]
-- [go 1, go 2, go 7, go 4, ....]
maxs' :: [Int] -> [Int]
maxs' ns = evalState (traverse go ns) 0 -- []
  where
    go :: Int -> State Int Int
    go x = do
      m <- get
      put (max x m)
      return (max x m)



data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq, Functor, Foldable)

data Maybe' a = Nothing' | Just' a | Nemtudom a (Maybe' a)
  deriving (Foldable, Show, Eq)

instance Functor Maybe' where
  fmap f Nothing' = Nothing'
  fmap f (Just' a) = Just' (f a)
  fmap f (Nemtudom a ma) = Nemtudom (f a) (fmap f ma)

instance Traversable Maybe' where
  traverse f Nothing' = pure Nothing'
  traverse f (Just' a) = Just' <$> (f a)
  traverse f (Nemtudom a ma) = Nemtudom <$> (f a) <*> (traverse f ma)

instance Traversable Tree where
  traverse :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l r) = Node <$> traverse f l <*> traverse f r

  -- 1. lépés = fmap-et cseréld le traverse-ra
  -- 2. lépés = Első paramatér elé <$>
  -- 3. 

  -- fmap f (Leaf a) = Leaf (f a)
  -- fmap f (Node l r) = Node (fmap f l) (fmap f r)

exampleTree :: Tree String
exampleTree = Node
                (Node (Leaf "the") (Leaf "cake"))
                (Node (Leaf "is") (Node (Leaf "a") (Leaf "lie")))

{-
             ∙
            / \
           /   \
          /     \
         ∙       \
        / \       \
       /   \       \
   "the"   "cake"   ∙
                   / \
                  /   \
               "is"    ∙
                      / \
                     /   \
                   "a"   "lie"
-}

exampleTree' :: Tree Int
exampleTree' = Node (Node (Leaf 2) (Leaf 3)) (Leaf 5)

{-
        ∙
       / \
      ∙   5
     / \
    2   3
-}

-- Definiálj egy függvényt, ami kicseréli egy fa leveleiben tárolt értékeket
-- balról jobbra haladva egy megadott lista elemeire. Használj State monádot!
-- pl: replaceLeaves [10, 20, 30] (Node (Leaf 2)  (Leaf 3))
--                              == Node (Leaf 10) (Leaf 20)
--     replaceLeaves [5] (Leaf 10)
--                    == (Leaf 5)
--     replaceLeaves [5] (Node (Leaf 0) (Node (Leaf 0) (Leaf 0)))
--                    == (Node (Leaf 5) (Node (Leaf 0) (Leaf 0)))

replaceLeaves :: [a] -> Tree a -> Tree a
replaceLeaves = undefined

replaceLeavesSt :: [a] -> Tree a -> Tree a
replaceLeavesSt as t = evalState (go t) as where
  go :: Tree a -> State [a] (Tree a)
  go (Leaf a) = do
    st <- get
    case st of
      [] -> return (Leaf a)
      (a':as) -> put as >> return (Leaf a')
  go (Node l r) = do
    l' <- go l
    r' <- go r
    return (Node l' r')


replaceLeavesTr :: [a] -> Tree a -> Tree a
replaceLeavesTr as t = evalState (traverse go t) as where
  go :: a -> State [a] a
  go a = do
    st <- get
    case st of
      [] -> return a
      (a':as) -> put as >> return a'


-- Definiáld a függvényt, ami megfordítja a fa leveleiben tárolt értékek
-- sorrendjét!  tipp: használhatod a replaceLeaves függvényt.
reverseElems :: Tree a -> Tree a
reverseElems = undefined

reverseElems' :: Tree a -> Tree a
reverseElems' = undefined


-- Írd át a következő függvényeket úgy, hogy *csak* a (State :: (s -> (a, s)) ->
-- State s a) konstruktort használd, monád/funktor instance-t és
-- get/put/modify-t ne használj.
--------------------------------------------------------------------------------

modify' :: (s -> s) -> State s ()
modify' f = do
  s <- get
  put (f s)

modify'' :: (s -> s) -> State s ()
modify'' f = State $ \s -> ((), f s)

-- Művelet végrehajtása lokálisan: az állapot visszaáll a művelet után.
locally :: State s a -> State s a
locally ma = do
  s <- get        -- "elmentjük" az állapotot
  a <- ma         -- futtatjuk ma-t
  put s           -- visszaállítjuk
  pure a

locally' :: State s a -> State s a
locally' ma = State $ \s -> case runState ma s of
  (a, _) -> (a, s)

-- Állapot módosítás n-szer
modifyNTimes :: Int -> (s -> s) -> State s ()
modifyNTimes 0 f = pure ()
modifyNTimes n f = modify f >> modifyNTimes (n - 1) f


--------------------------------------------------------------------------------

-- Értelmezd a következő utasítások listáját. Minden utasítás
-- egy Int-et módosító művelet. Az "Add n" adjon n-et a jelenlegi
-- állapothoz, a "Subtract n" vonjon ki n-t, és a "Mul" értelemszerűen.
data Op = Add Int | Subtract Int | Mul Int

evalOps :: [Op] -> State Int ()
evalOps = undefined

-- Add meg ennek segítségével az állapotot módosító (Int -> Int) függvényt.
runOps :: [Op] -> Int -> Int
runOps = undefined