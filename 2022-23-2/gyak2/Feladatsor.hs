{-# language DeriveFunctor, InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Monad

-- State
--------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure a  = State (\s -> (a, s))
  (<*>) = ap

instance Monad (State s) where
  return = pure
  (>>=) (State f) g = State $ \s -> case f s of
    (a, s) -> runState (g a) s

put :: s -> State s ()
put s = State $ \_ -> ((), s)

get :: State s s
get = State $ \s -> (s, s)

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState sta s = fst (runState sta s)

execState :: State s a -> s -> s
execState sta s = snd (runState sta s)


-- FELADATOK (1. sor)
-- Összesen: 20 pont
-- ~8-10 kis feladat: összesen 10-12 pont

--------------------------------------------------------------------------------

data Either' a b = Left' a | Right' b | Both a b
  deriving (Show)

-- Definiáld a következő instance-okat

instance Functor (Either' c) where
  fmap :: (a -> b) -> Either' c a -> Either' c b
  fmap = undefined

instance (Eq a, Eq b) => Eq (Either' a b) where
  (==) :: Either' a b -> Either' a b -> Bool
  (==) = undefined

instance Foldable (Either' c) where
  -- választhatok foldr és foldMap implementáció között
  -- foldr :: (a -> b -> b) -> b -> Either' c a -> b
  -- foldr = undefined

  -- foldMap :: Monoid m => (a -> m) -> Either' c a -> m
  -- foldMap = undefined

instance Traversable (Either' c) where
  traverse :: Applicative f => (a -> f b) -> Either' c a -> f (Either' c b)
  traverse = undefined

-- Bontsuk szét a listát a három lehetséges Either' konstruktor szerint!
-- Példa:
--   partition [Left' 0, Left' 2, Right' True, Both 10 False]
--      == ([0, 2], [True], [(10, False)])

partition :: [Either' a b] -> ([a], [b], [(a, b)])
partition = undefined

-- Írj olyan zipWith' függvényt, ami kezelni tudja azokat az eseteket, amikor
-- valamelyik input lista üres (és a másik nemüres). Példa a működésre:
--
--  go :: Either' Int Int -> Int
--  go (Left' x) = x
--  go (Right' x) = x
--  go (Both x y) = x + y
--
--  zipWith' go [1, 2, 3] [10] == [11, 2, 3]
--  zipWith' go [10, 20] [20] == [30, 20]
--  zipWith' go [] [0, 1, 2] == [0, 1, 2]

zipWith' :: (Either' a b -> c) -> [a] -> [b] -> [c]
zipWith' = undefined


-- Alkalmazzunk egy (a -> Maybe b) függvényt a listaelemekre.  Nothing-ot
-- kapunk, legyen a végeredmény Nothing, egyébként Just-ban a map-elt lista.
mapMaybeLeft :: (a -> Maybe b) -> [Either' a c] -> Maybe [Either' b c]
mapMaybeLeft = undefined


data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Functor, Foldable, Traversable)


-- Tegyük be egy fában az összes levélbe az adott levéltől balra levő Int-ek összegét.
-- Pl: treeSums (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1)) ==
--              (Node (Node (Leaf 0) (Leaf 1)) (Leaf 2))
treeSums :: Tree Int -> Tree Int
treeSums = undefined


-- Feladatok (2.sor)
--------------------------------------------------------------------------------

-- (nehezebb, mint ami átlagosan vizsgán várható)
data RoseTree a = Branch a [RoseTree a]
  deriving (Eq, Ord, Show)

ex1 :: RoseTree Int
ex1 = Branch 2 $
      [ Branch 3 $
          [ Branch 11 []
          ]
      , Branch 5 $ []
      , Branch 7 $
          [ Branch 13 []
          ]
      ]

-- Definiáld az instance-okat.

instance Functor RoseTree where
  fmap :: (a -> b) -> RoseTree a -> RoseTree b
  fmap = undefined

instance Foldable RoseTree where
  -- elég az egyiket
  -- foldr :: (a -> b -> b) -> b -> RoseTree a -> b
  -- foldr = undefined

  -- foldMap :: Monoid m => (a -> m) -> RoseTree a -> m
  -- foldMap = undefined

instance Traversable RoseTree where
  traverse :: Applicative f => (a -> f b) -> RoseTree a -> f (RoseTree b)
  traverse = undefined

-- Add vissza a tárolt "a" típusú értékek számát!
countElems :: RoseTree a -> Int
countElems = undefined

-- Add vissza az elemek maximumát.
maxElem :: Ord a => RoseTree a -> a
maxElem = undefined

-- Számozd be a fában tártolt értékeket balról-jobbra bejárási sorrendben.
label :: RoseTree a -> RoseTree (a, Int)
label = undefined


-- A fában tárolt minden N szám helyére tedd be a kapott [a] lista N-edik
-- értékét. Ha az N index bárhol kimutat a listából, akkor legyen a végeredmény
-- Nothing. Példák a működésre:
--  transformWithList [2, 3, 4] (Branch 0 [Branch 2 [Branch 1 []]])
--                          ==  Just (Branch 2 [Branch 4 [Branch 3 []]])
--  transformWithList [2, 3, 4] (Branch 3 []) == Nothing

transformWithList :: [a] -> RoseTree Int -> Maybe (RoseTree a)
transformWithList = undefined
