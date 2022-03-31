{-# LANGUAGE DeriveFunctor #-}

module Lesson08 where

import Text.Read hiding (get)
import Control.Monad

newtype State s a = State {runState :: s -> (a, s)}

evalState :: State s a -> s -> a
evalState sa = fst . runState sa

execState :: State s a -> s -> s
execState sa = snd . runState sa

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    -- (a -> b) -> (s -> (a,s)) -> (s -> (b,s))
    fmap f (State g) = State $ \s -> let (a,s') = g s in (f a, s')

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State $ \s -> (a,s)

    (<*>) :: State s (a -> b) -> State s a -> State s b
    -- f :: s -> (a -> b,s)
    -- g :: s -> (a,s)
    (State f) <*> (State g) = State $ \s ->
        let (aToB, s') = f s; (a,s'') = g s' in (aToB a, s'')

sumIntegers :: IO Int -> IO Integer
-- fmap :: (a -> b) -> IO a -> IO b
-- fmap :: ([String] -> [Maybe Integer]) -> IO [String] -> IO [Maybe Integer]
-- map :: (a -> b) -> [a] -> [b]
-- map :: (String -> Maybe Integer) -> [String] -> [Maybe Integer]
sumIntegers ioN = do
    n <- ioN
    l <- replicateM n getLine
    pure $ sum [x | Just x <- l]

{-
sumIntegers ioN = ioN >>= \n ->
    replicateM n getLine >>= \l ->
        pure (sum [x | Just x <- l])

sumIntegers ioN = ioN >>= \n -> fmap (map readMaybe) (replicateM n getLine) >>= \l -> pure (sum [x | Just x <- l])
sumIntegers ioN = ioN >>= \n -> fmap ((\l -> sum [x | Just x <- l]) . map readMaybe) (replicateM n getLine)
-}
instance Monad (State s) where
    (State f) >>= g = State $ \s -> case f s of (a,s') -> runState (g a) s'

-- get, put, modify
get :: State s s
get = State $ \s -> (s,s)

-- data () = ()
-- data One = One

put :: s -> State s ()
put s = State $ \_ -> ((),s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((),f s)

modify' :: (s -> s) -> State s ()
modify' f = do
    s <- get
    put (f s)
--------------------------------------------------------------------------------

-- típus (2 paraméteres):   State :: Type -> Type -> Type
--  State s a  :   mellékhatás: egy darab "s" típus írható/olvasható referencia
--     instance Monad (State s)
--     "a": visszatérési érték típusa (mint minden monádban)

-- futtatás:
--------------------------------------------------------------------------------

--                       kezdő állapot      (érték, végső állapotot)
-- runState :: State s a ->      s          -> (a, s)

-- (kényelmi függvények)
-- evalState :: State s a ->      s          -> a
-- execState :: State s a ->      s          -> s


-- állapothoz kapcs műveletek:
--------------------------------------------------------------------------------

-- írjunk egy adott értéket az állapotba
-- put :: s -> State s ()

f1 :: State Int (Int,Int,Int)
f1 = do
  put 10   -- állapot := 10
  n <- get
  put 20   -- állapot := 20
  m <- get
  put 30   -- állapot := 30
  k <- get
  pure (n,m,k)

-- runState f1 :: Int -> ((), Int)
-- evalState f1 :: Int -> ()
-- execState f1 :: Int -> Int

-- execState f1 0 == 30

-- kiolvassuk a jelenlegi állapotot.

-- get egy művelet, ami értékként visszadja
-- a jelenlegi állapotot
-- get :: State s s

f2 :: State Int ()
f2 = do
  n <- get     -- get :: State Int Int   -- n :: Int
  if n < 10 then put n
            else put (n * 10)

-- imperatív pszeudokód:
-- f2 =
--    var state;
--    var n := state;
--    if (n < 10) { state := n ;} else {state := n * 10}
--    return ();

-- execState f2 20 == 200
-- execState f2 0  == 0

-- -- alkalmazzunk egy függvényt az állapotra
-- modify :: (s -> s) -> State s ()
-- modify f = do
--    s <- get
--    put (f s)

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Show)

-- írj egy függvényt, ami balról-jobbra bejárási sorrendben
-- beszámozza egy fa leveleit
-- State monád használatával!
label :: Tree a -> Tree (a, Int)
label t = evalState (helperState t) 0 where
    helperState :: Tree a -> State Int (Tree (a,Int))
    helperState (Leaf a) = do
        n <- get
        put (n + 1)
        pure (Leaf (a,n))
    helperState (Node l r) = do
        l' <- helperState l
        r' <- helperState r
        pure (Node l' r')

-- állapot a szám, ahol éppen tartunk
-- State Int (Tree (a, Int))

-- példa
t1 :: Tree Bool
t1 = Node (Node (Leaf True) (Leaf False)) (Leaf False)

-- label t1 == Node (Node (Leaf (True,0)) (Leaf (False,1))) (Leaf (False,2))


-- további motiváló példa State-re:
--    - gráf algoritmusok
--    - fa algoritmus
--    - interpreter olyan nyelvre, amiben van mutáció
--    -   interpreter minimális (pl. Brainfuck (lásd Wikipedia))

------------------------------------------------------------

-- Írj egy függvényt, ami egy Int listában minden elemet kicserél az addigi
-- elemek maximumára.
-- Használd a (State Int)-et. Legyen az állapot a jelenlegi maximális Int.
maxs :: [Int] -> [Int]
maxs l = evalState (helperState l) minBound where
    helperState :: [Int] -> State Int [Int]
    helperState [] = pure []
    helperState (x:xs) = do
        n <- get
        if x > n then do 
                    put x
                    l <- helperState xs
                    pure (x:l)
                 else do
                    l <- helperState xs
                    pure (n:l)

-- maxs [2,1,3,4,1,0,9,5,4] == [2,2,3,4,4,4,9,9,9]

-- Definiálj egy függvényt, ami kicseréli egy fa leveleiben tárolt értékeket
-- balról jobbra haladva egy megadott lista elemeire. Használj State monádot!

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

-- Értelmezd a következő utasítások listáját. Minden utasítás
-- egy Int-et módosító művelet. Az "Add n" adjon n-et a jelenlegi
-- állapothoz, a "Subtract n" vonjon ki n-t, és a "Mul" értelemszerűen.
data Op = Add Int | Subtract Int | Mul Int

evalOps :: [Op] -> State Int ()
evalOps = undefined

-- Add meg ennek segítségével az állapotot módosító (Int -> Int) függvényt.
runOps :: [Op] -> Int -> Int
runOps = undefined