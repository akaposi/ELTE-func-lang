
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
--------------------------------------------------------------------------------

data Either' a b = Left' a | Right' b | Both a b
  deriving (Show)

-- Definiáld a következő instance-okat

instance Functor (Either' c) where
  fmap :: (a -> b) -> Either' c a -> Either' c b
  fmap f (Left' a) = Left' a
  fmap f (Right' b) = Right' (f b)
  fmap f (Both a b) = Both a (f b)

instance (Eq a, Eq b) => Eq (Either' a b) where
  (==) :: Either' a b -> Either' a b -> Bool
  (==) (Left' a) (Left' a') = a == a'
  (==) (Right' b) (Right' b') = b == b'
  (==) (Both a b) (Both a' b') = a == a' && b == b'
  (==) _ _ = False

instance Foldable (Either' c) where
  -- választhatok foldr és foldMap implementáció között
  foldr :: (a -> b -> b) -> b -> Either' c a -> b
  foldr f b (Left' _) = b
  foldr f b (Right' a) = f a b
  foldr f b (Both c a) = f a b

  foldMap :: Monoid m => (a -> m) -> Either' c a -> m
  foldMap f (Left' _) = mempty
  foldMap f (Right' a) = f a
  foldMap f (Both c a) = f a


instance Traversable (Either' c) where
  traverse :: Applicative f => (a -> f b) -> Either' c a -> f (Either' c b)
  traverse f (Left' c) = pure (Left' c)
  traverse f (Right' a) = Right' <$> f a
  traverse f (Both c a) = Both c <$> f a

-- Bontsuk szét a listát a három lehetséges Either' konstruktor szerint!
-- Példa:
--   partition [Left' 0, Left' 2, Right' True, Both 10 False]
--      == ([0, 2], [True], [(10, False)])

partition :: [Either' a b] -> ([a], [b], [(a, b)])
partition = foldr
  (\eab (as, bs, abs) -> case eab of
      Left' a -> (a:as, bs, abs)
      Right' b -> (as, b:bs, abs)
      Both a b -> (as, bs, (a, b):abs))
  ([], [], [])

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
zipWith' f (a:as) (b:bs) = f (Both a b) : zipWith' f as bs
zipWith' f []     (b:bs) = f (Right' b) : zipWith' f [] bs
zipWith' f (a:as) []     = f (Left' a)  : zipWith' f as []
zipWith' f []     []     = []


-- Alkalmazzunk egy (a -> Maybe b) függvényt a listaelemekre.  Nothing-ot
-- kapunk, legyen a végeredmény Nothing, egyébként Just-ban a map-elt lista.
mapMaybeLeft :: (a -> Maybe b) -> [Either' a c] -> Maybe [Either' b c]
mapMaybeLeft f = traverse (go f) where
  go :: (a -> Maybe b) -> Either' a c -> Maybe (Either' b c)
  go f (Left' a)  = Left' <$> f a
  go f (Right' c) = pure (Right' c)
  go f (Both a c) = (\b -> Both b c) <$> f a

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Functor, Foldable, Traversable)


-- Tegyük be egy fában az összes levélbe az adott levéltől balra levő Int-ek összegét.
-- Pl: treeSums (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1)) ==
--              (Node (Node (Leaf 0) (Leaf 1)) (Leaf 2))
treeSums :: Tree Int -> Tree Int
treeSums t = evalState (traverse go t) 0 where
  go n = do
    s <- get
    put (s + n)
    pure s


-- Feladatok (2.sor)
--------------------------------------------------------------------------------

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
  fmap f (Branch a ts) = Branch (f a) (fmap (fmap f) ts)

instance Foldable RoseTree where
  -- elég az egyiket
  foldr :: (a -> b -> b) -> b -> RoseTree a -> b
  foldr f b (Branch a ts) = f a (foldr (\t b -> foldr f b t) b ts)

  foldMap :: Monoid m => (a -> m) -> RoseTree a -> m
  foldMap f (Branch a ts) = f a <> foldMap (foldMap f) ts

instance Traversable RoseTree where
  traverse :: Applicative f => (a -> f b) -> RoseTree a -> f (RoseTree b)
  traverse f (Branch a ts) = Branch <$> f a <*> traverse (traverse f) ts

-- Add vissza a tárolt "a" típusú értékek számát!
countElems :: RoseTree a -> Int
countElems = length

-- Add vissza az elemek maximumát.
maxElem :: Ord a => RoseTree a -> a
maxElem = maximum

-- Számozd be a fában tártolt értékeket balról-jobbra bejárási sorrendben.
label :: RoseTree a -> RoseTree (a, Int)
label t = evalState (traverse go t) 0 where
  go a = do {n <- get; put (n + 1); pure (a, n)}

-- A fában tárolt minden N szám helyére tedd be a kapott [a] lista N-edik
-- értékét. Ha az N index bárhol kimutat a listából, akkor legyen a végeredmény
-- Nothing. Példák a működésre:
--  transformWithList [2, 3, 4] (Branch 0 [Branch 2 [Branch 1 []]])
--                          ==  Just (Branch 2 [Branch 4 [Branch 3 []]])
--  transformWithList [2, 3, 4] (Branch 3 []) == Nothing

-- transformWithList :: [a] -> RoseTree Int -> Maybe (RoseTree a)
-- transformWithList as t = evalState (traverse
