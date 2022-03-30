
{-# language InstanceSigs, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Monad

-- Következő canvas feladat
------------------------------------------------------------

-- State feladat + rekurzió vagy bejárás


------------------------------------------------------------

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

-- canvas feladat

f :: State [Int] ()
f = do
  l <- get
  if length l > 5
    then put $ tail l
    else put $ l ++ l

f' :: State [Int] ()
f' = modify $ \l -> if length l > 5 then tail l else l ++ l

f'' :: State [Int] ()
f'' = State $ \l -> if length l > 5 then ((), tail l)
                                    else ((), l ++ l)
   -- [Int] -> ((), [Int])
   -- newtype State s a = State {runState :: s -> (a, s)}

--------------------------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Functor, Foldable, Traversable)


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

-- Írj egy függvényt, ami egy Int listában minden elemet kicseréli az elem
-- előtti értékek maximumára. Tegyük fel, hogy a lista nem-negatív számokat
-- tartalmaz. Használd a (State Int)-et. Legyen az állapot a jelenlegi
-- maximális Int.
maxs :: [Int] -> [Int]
maxs ns = evalState (go ns) 0 where

  go :: [Int] -> State Int [Int]
  go []     = pure []
  go (n:ns) = do
    m <- get
    put $ max n m
    ns <- go ns     -- beárnyékolom a régi ns-t
    pure (m:ns)

-- maxs [] = []
-- maxs ns = let m = maximum ns in map (const m) ns
-- maximum' ns = foldl max 0 ns

-- használjuk a mapM függvényt!
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
maxs' :: [Int] -> [Int]
maxs' ns = evalState (mapM go ns) 0 where

  -- mapM go ns :: State Int [Int]
  -- ns :: [Int]
  -- go :: a -> m b              valamilyen a, m, és b-re
  --    :: Int -> m b            mert [Int]-t map-elek
  --    :: Int -> m Int          mert [Int] a végeredmény
  --    :: Int -> State Int Int  mert State Int a mellékhatás

  go :: Int -> State Int Int
  go n = do
    m <- get
    put $ max n m
    pure m

-- vesd össze (tiszta eset), rekurzió helyett map
--   go f [] = ...
--   go f (n:ns) = ... : go ns

-- class Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- traverse: általánosabb mapM:
--                 1. nem csak listára működik
--                 2. nem kell neki Monad, csak Applicative mellékhatás

-- {-# language DeriveTraversable #-}


-- Írj egy függvényt, ami kizárólag push, pop és rekurzió felhasználásával
-- map-eli az állapot listát. Nem szabad: lista mintaillesztés.
mapPushPop :: (a -> a) -> State [a] ()
mapPushPop f = do
  ma <- pop   -- mas :: Maybe a
  case ma of
    Nothing -> pure ()
    Just a  -> do
      mapPushPop f
      push $ f a

map' :: (a -> a) -> [a] -> [a]
map' f = execState (mapPushPop f)


-- Írj egy függvényt, ami minden levélbe az adott levéltől balra levő (bal-jobb
-- bejárási sorrendben) levelek összegét teszi be!

-- pl: treeSums (Node (Node (Leaf 2) (Leaf 2)) (Leaf 2)) ==
--        Node (Node (Leaf 0) (Leaf 2)) (Leaf 4)

-- traverse definíciója Tree-re
traverseTree :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
traverseTree f (Leaf a)   = Leaf <$> f a
traverseTree f (Node l r) = Node <$> traverseTree f l <*> traverseTree f r

-- traverse :: (a -> m b) -> Tree a -> m (Tree b)
-- traverse :: (Int -> State Int Int) -> Tree Int -> State Int (Tree Int)
treeSums :: Tree Int -> Tree Int
treeSums t = evalState (traverse go t) 0 where

  go :: Int -> State Int Int
  go n = do
    sum <- get
    put $ sum + n
    pure sum

-- traverse nélkül:
treeSums' :: Tree Int -> Tree Int
treeSums' t = evalState (go t) 0 where

  go :: Tree Int -> State Int (Tree Int)
  go (Leaf n) = do
    sum <- get
    put $ sum + n
    pure (Leaf sum)
  go (Node l r) =
    Node <$> go l <*> go r  -- bináris fmap
  -- go (Node l r) = do
  --   l <- go l
  --   r <- go r
  --   pure (Node l r)


-- Definiálj egy függvényt, ami kicseréli egy fa leveleiben tárolt értékeket
-- balról jobbra haladva egy megadott lista elemeire. Használj State monádot!

-- pl: replaceLeaves [10, 20, 30] (   Node (Leaf 2) (Leaf 3))
--                                 == Node (Leaf 10) (Leaf 20)
--     replacereplaceLeaves [5] (Leaf 10) == Leaf 5
--     replacereplaceLeaves [5]
--        (Node (Leaf 0) (Node (Leaf 0) (Leaf 0))) ==
--        (Node (Leaf 5) (Node (Leaf 0) (Leaf 0)))

replaceLeaves :: [a] -> Tree a -> Tree a
replaceLeaves as t = evalState (traverse go t) as where

  go :: a -> State [a] a
  go a = do
    ma' <- pop
    case ma' of
      Nothing -> pure a
      Just a' -> pure a'

replaceLeaves' :: [a] -> Tree a -> Tree a
replaceLeaves' as t = evalState (go t) as where

  go :: Tree a -> State [a] (Tree a)
  go (Leaf a) = do
    ma' <- pop
    case ma' of
      Nothing -> pure (Leaf a)
      Just a' -> pure (Leaf a')
  go (Node l r) =
    Node <$> go l <*> go r

-- opcionális házi: írd meg a replaceLeaves-et úgy
-- üres lista esetén ne járja be a maradék fát.
-- (nem működik ekkor a traverse!)


-- Definiáld a függvényt, ami megfordítja a fa leveleiben tárolt értékek
-- sorrendjét!  tipp: használhatod a replaceLeaves függvényt.
reverseElems :: Tree a -> Tree a
reverseElems t =
  let treeElems = foldr (:) [] t
  in replaceLeaves (reverse treeElems) t

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
