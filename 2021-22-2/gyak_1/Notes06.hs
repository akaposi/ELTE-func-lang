

{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Monad


--------------------------------------------------------------------------------
-- Következő canvas feladat:

-- State feladat, nincs rekurzíó, nincs fa/lista
--   get/put/modify/evalState-el valamilyen egyszerűbb függvényt
--   megadni.


--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen,
-- de típushelyesen.

f4 :: Monad m => m (a -> b) -> m a -> m b
f4 mf ma = do
  f <- mf  -- f :: a -> b
  a <- ma  -- a :: a
  return (f a)
-- Control.Monad.ap
-- Applicative (<*>) metódus újradefiniálása

-- (>>=) :: m a -> (a -> m b) -> m b
f5 :: Monad m => (a -> m b) -> m a -> m b
f5 = flip (>>=)

f6 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
f6 f ma mb = do
  a <- ma -- a :: a
  b <- mb -- b :: b
  return (f a b)

-- f6 f ma mb = f <$> ma <*> mb    -- (<$>) : fmap mint operátor

--  n-aritású fmap:  f <$> arg1 <*> arg2 <*> ..... <*> argN

--   fmapN :: Applicative f =>
--            (a1 -> a2 -> ... -> aN -> res)
--         -> f a1 -> f a2 -> ... -> f aN -> f res
--   fmapN f arg1 ... argN = f <$> arg1 <*> ... <*> argN

f7 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
f7 f ma mb mc = f <$> ma <*> mb <*> mc

-- f8 = (>=>)

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- (.) f g x = f (g x)

f8 :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f8 f g a = do
  b <- f a    -- b   :: b
  g b         -- g b :: m c

-- State monád definíció
--------------------------------------------------------------------------------

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
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--------------------------------------------------------------------------------

-- típus (2 paraméteres):   State :: * -> * -> *
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

f1 :: State Int ()
f1 = do
  put 10   -- állapot := 10
  put 20   -- állapot := 20
  put 30   -- állapot := 30

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
label :: Tree a -> Tree (a, Int)
label t = fst (go t 0) where

  go :: Tree a -> Int ->  (Tree (a, Int), Int)
  go (Leaf a)   n = (Leaf (a, n), n+1)
  go (Node l r) n = case go l n of
    (l', n') -> case go r n' of
      (r', n'') -> (Node l' r', n'')

  -- data Tree a = Node (Tree a) (Tree a) (Tree a) (Tree a)

  -- lásd Maybe monád:
  --     case x of
  --        Nothing -> Nothing
  --        Just a -> case y of
  --          Nothing -> Nothing
  --          Just b -> ...

label' :: Tree a -> Tree (a, Int)
label' t = evalState (go t) 0 where

     -- go t :: State Int (Tree (a, Int))
     -- evalState (go t) :: Int -> Tree (a, Int)
     -- evalState (go t) 0 :: Tree (a, Int)

  go :: Tree a -> State Int (Tree (a, Int))
  go (Leaf a) = do
    n <- get     -- n := state
    put (n + 1)  -- state := n + 1
    return (Leaf (a, n))
  -- go (Node l r) =
  --   Node <$> go l <*> go r

  go (Node l r) = do
    l' <- go l
    r' <- go r
    return (Node l' r')

-- példa
t1 :: Tree Bool
t1 = Node (Node (Leaf True) (Leaf False)) (Leaf False)

-- label' t1 == Node (Node (Leaf (True,0)) (Leaf (False,1))) (Leaf (False,2))

-- State monád: mutábilis interface immutábilis implementáció a háttérben
--    (viszont Haskell-ben van: tényleg mutáció a háttérben (hatékonyság miatt))
--       (tömbök írása)

maximum :: [Int] -> Int
maximum ns = go ns 0 where
  go []    max = max
  go (n:ns) max | n < max   = go ns max
                | otherwise = go ns n
 -- (optimális kódra fordul)
 --   (amit lehet, definiáljunk vég-rekurzívan)

-- további motiváló példa State-re:
--    - gráf algoritmusok
--    - fa algoritmus
--    - interpreter olyan nyelvre, amiben van mutáció
--    -   interpreter minimális (pl. Brainfuck (lásd Wikipedia))

------------------------------------------------------------


-- Definiálj egy függvényt, ami a lista állapotot kiegészíti egy elemmel
push :: a -> State [a] ()
push a = do
  as <- get
  put (a:as)

-- push a = modify (\as -> a:as)
-- push a = modify (a:)              -- emlékezzünk: (+10) :: Int -> Int

-- példák:
-- runState (push 10) [] == ((), [10])
-- runState (push 10 >> push 10 >> push 20) [] == ((), [20, 10, 10])


-- Ha az állapot lista nem üres, akkor a következő függvény leveszi az első
-- elemet és visszaadja Just értékként, egyébként Nothing-ot ad.
pop :: State [a] (Maybe a)
pop = do
  as <- get
  case as of
    [] -> return Nothing
    a:as -> do
      put as
      return (Just a)

-- Írj egy függvényt, ami egy Int listában minden elemet kicserél az addigi
-- elemek maximumára.  Tegyük fel, hogy a lista nem-negatív számokat tartalmaz.
-- Használd a (State Int)-et. Legyen az állapot a jelenlegi maximális Int.
maxs :: [Int] -> [Int]
maxs = undefined

-- Írj egy függvényt, ami kizárólag push, pop és rekurzió felhasználásával
-- map-eli az állapot listát.
mapPushPop :: (a -> a) -> State [a] ()
mapPushPop = undefined


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



-- Írd át a következő függvényeket úgy, hogy *csak* a (State :: (s -> (a, s)) ->
-- State s a) konstruktort használd, monád/funktor instance-t és
-- get/put/modify-t ne használj.
--------------------------------------------------------------------------------

modify' :: (s -> s) -> State s ()
modify' f = do
  s <- get
  put (f s)

  -- Művelet végrehajtása lokálisan: az állapot visszaáll a művelet után.
locally :: State s a -> State s a
locally ma = do
  s <- get        -- "elmentjük" az állapotot
  a <- ma         -- futtatjuk ma-t
  put s           -- visszaállítjuk
  pure a

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
