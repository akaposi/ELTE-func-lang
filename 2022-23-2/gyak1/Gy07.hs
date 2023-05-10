{-# LANGUAGE InstanceSigs, DeriveFunctor, DeriveFoldable #-}
module Gy07 where


import Control.Monad
import Data.Traversable
import Prelude hiding (NonEmpty(..), Maybe(..), Either(..))

newtype State s a = State { runState :: s -> (a, s) } deriving Functor

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) = ap

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State sa) >>= f = State $ \s -> let (a, s') = sa s in runState (f a) s'

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= put . f


guessingGameIO :: Int -> IO ()
guessingGameIO n = do
  k <- readLn :: IO Int
  if k > n then do
    putStrLn "Kisebb"
    guessingGameIO n
  else if k < n then do
    putStrLn "Nagyobb"
    guessingGameIO n
  else do
    putStrLn "Talált"


guessingGameSt :: Int -> State [Int] String
guessingGameSt n = do
  list <- get
  case list of
    [] -> do
      put []
      pure "Nem nyer"
    (x:xs) -> help x xs
      where
        help x xs
          | x == n = do
              put xs
              pure "Hurrá"
          | otherwise = do
              put xs
              guessingGameSt n


-- Definiáljunk egy olyan áll. vált.-t amely megcímzkézi egy lista összes elemét
labelList :: [a] -> State Integer [(Integer, a)]
labelList [] = pure []
labelList (x:xs) = do
  i <- get
  put (i + 1)
  xs' <- labelList xs
  pure ((i, x) : xs')

-- Futtassuk le a fenti állapotváltozást 0 kezedeti label-el!
labelList' :: [a] -> [(Integer, a)] -- runState :: State s a -> s -> (a,s)
labelList' xs = fst $ runState (labelList xs) 0


-- Segédműveletek
execState :: State s a -> s -> s
execState st s = let (a, s') = runState st s in s'

evalState :: State s a -> s -> a
evalState st s = let (a, s') = runState st s in a


-- preorder = először középső, majd bal jobb
-- postorder = először bal, majd jobb és középső
-- inorder = először bal, majd középső és jobb

-- A fenti séma szerint címkézzünk megy egy fát!
-- Csináljuk meg mind preorder, inorder és postorder bejárással
data Tree a = Leaf a | Node (Tree a) a  (Tree a) deriving (Eq, Show, Functor, Foldable)

labelTreePRE, labelTreeIN, labelTreePOST :: Tree a -> Tree (Integer, a)
labelTreePRE tr = evalState (labelTreePre' tr) 0
labelTreeIN tr = evalState (labelTreeIn'  tr) 0
labelTreePOST tr = evalState (labelTreePost' tr) 0

                            --- állapot = számláló, a mellékhatás = az eredmény
labelTreePre' :: Tree a -> State Integer (Tree (Integer, a))
labelTreePre' (Leaf a) = do
  i <- get
  put (i + 1)
  pure $ Leaf (i, a)
labelTreePre' (Node l a r) = do
  i <- get
  put (i + 1)
  l' <- labelTreePre' l
  r' <- labelTreePre' r
  pure $ Node l' (i, a) r'

labelTreePost' :: Tree a -> State Integer (Tree (Integer, a))
labelTreePost' (Leaf a) = do
  i <- get
  put (i + 1)
  pure $ Leaf (i, a)
labelTreePost' (Node l a r) = do
  l' <- labelTreePost' l
  r' <- labelTreePost' r
  i <- get
  put (i + 1)
  pure $ Node l' (i, a) r'

labelTreeIn' :: Tree a -> State Integer (Tree (Integer, a))
labelTreeIn' (Leaf a) = do
  i <- get
  put (i + 1)
  pure $ Leaf (i, a)
labelTreeIn' (Node l a r) = do
  l' <- labelTreePost' l
  i <- get
  put (i + 1)
  r' <- labelTreePost' r
  pure $ Node l' (i, a) r'

---- Írjuk meg az alábbi műveleteket

-- Végezzük el az első paraméterként kapott IO műveletet minden listabeli elemre
ioList :: (a -> IO b) -> [a] -> IO [b]
ioList f [] = return []
ioList f (x:xs) = (:) <$> f x <*> ioList f xs

--- Cons $ a₁ $ a₂ $  a₃  ... k = Cons <$> f a₁ <*> f a₂ <*> f a₃ ... <*> rekurziv f k

-- Csináljuk megy ugyanezt fára
ioTree :: (a -> IO b) -> Tree a -> IO (Tree b)
ioTree f (Leaf a) = Leaf <$> f a
ioTree f (Node l a r) = do -- Node <$> ioTree f l <*> f a <*> ioTree f r
  l' <- ioTree f l
  a' <- f a
  r' <- ioTree f r
  pure (Node l' a' r')

-- Csináljuk meg ugyanezeket state-el!
stateList :: (a -> State s b) -> [a] -> State s [b]
stateList f [] = return []
stateList f (x:xs) = (:) <$> f x <*> stateList f xs

stateTree :: (a -> State s b) -> Tree a -> State s (Tree b)
stateTree f (Leaf a) = Leaf <$> f a
stateTree f (Node l a r) = Node <$> stateTree f l <*> f a <*> stateTree f r


-- Ezeke a műveletek tetszőleges applikatív struktúrával elvégezhető, pl:
appList :: Applicative f => (a -> f b) -> [a] -> f [b]
appList f [] = pure []
appList f (x:xs) = (:) <$> f x <*> appList f xs

appTree :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
appTree f (Leaf a) = Leaf <$> f a
appTree f (Node l a r) = Node <$> appTree f l <*> f a <*> appTree f r

-- A függvények hasonlóak => új tulajdonság
{-
        appList :: Applicative f => (a -> f b) -> List a -> f (List b)
        appTree :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
-}

-- Ezt a tulajdonságot a Traversable típusosztály fogja reprezentálni
{-
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  {-# MINIMAL traverse | sequenceA #-}
-}
                  --  V V
-- data Dual a = Dual a a

data List a = Nil | Cons a (List a) deriving (Eq, Show, Foldable, Functor)

instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse f Nil = pure Nil
  traverse f (Cons a as) = Cons <$> f a <*> traverse f as

  -- Alkalmazzuk a konstruktort
  -- Végigiterálunk a paramétereken
  -- Ha nem rekurzív akkor f a
  -- Ha rekurzív akkor traverse f a

  sequenceA :: Applicative f => List (f a) -> f (List a)
  sequenceA Nil = pure Nil
  sequenceA (Cons a as) = Cons <$> a <*> sequenceA as

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse = appTree
  sequenceA :: Applicative f => Tree (f a) -> f (Tree a)
  sequenceA (Leaf a) = Leaf <$> a
  sequenceA (Node l a r) = Node <$> sequenceA l <*> a <*> sequenceA r

-- Írjunk Traversable instance-okat!


data Id a = Id a deriving (Eq, Show, Foldable, Functor)
data Dual a = Dual a a deriving (Eq, Show, Foldable, Functor)
data BiList a = BiCons a a (BiList a) | BiNill deriving (Eq, Show, Foldable, Functor)
data NonEmpty a = Last a | NECons a (NonEmpty a) deriving (Eq, Show, Foldable, Functor)
data Maybe a = Just a | Nothing deriving (Eq, Show, Foldable, Functor)
data Either e a = Left e | Right a deriving (Eq, Show, Foldable, Functor)
data SplitList e a = LeftCons (SplitList e a) e | RightCons a (SplitList e a) | SNill deriving (Eq, Show, Functor, Foldable)
data Wrap f a = Wrap (f a) deriving (Eq, Show, Foldable, Functor)
data WrapList f a = WrapCons (f a) (WrapList f a) | WrapNill deriving (Eq, Show, Functor, Foldable)
data Compose f g a = Compose (f (g a)) deriving (Eq, Show, Functor, Foldable)
data Tree2 a = Leaf2 | Node2 (Tree2 a) a (Tree2 a) deriving (Eq, Show, Functor, Foldable)

instance Traversable Id where

  sequenceA :: Applicative f => Id (f a) -> f (Id a)
  sequenceA (Id a) = Id <$> a

instance Traversable Dual where

  traverse :: Applicative f => (a -> f b) -> Dual a -> f (Dual b)
  traverse f (Dual a1 a2) = Dual <$> f a1 <*> f a2

instance Traversable BiList where

  traverse :: Applicative f => (a -> f b) -> BiList a -> f (BiList b)
  traverse f BiNill = pure BiNill
  traverse f (BiCons a1 a2 bs) = BiCons <$> f a1 <*> f a2 <*> traverse f bs

instance Traversable NonEmpty where

  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (Last a) = Last <$> f a
  traverse f (NECons a as) = NECons <$> f a <*> traverse f as

instance Traversable Maybe where

  sequenceA :: Applicative f => Maybe (f a) -> f (Maybe a)
  sequenceA Nothing = pure Nothing
  sequenceA (Just a) = Just <$> a

instance Traversable (Either q) where

  sequenceA (Left e) = pure (Left e)
  sequenceA (Right a) = Right <$> a

instance Traversable (SplitList q) where

  traverse f (LeftCons as q) = LeftCons <$> traverse f as <*> pure q
  traverse f (RightCons a as) = RightCons <$> f a <*> traverse f as

data JaggedList a = JaggedCons [[a]] (JaggedList a) | JNill deriving (Functor, Foldable)

instance Traversable JaggedList where
  traverse :: Applicative f => (a -> f b) -> JaggedList a -> f (JaggedList b)
  traverse f JNill = pure JNill
  traverse f (JaggedCons as ja) = JaggedCons <$> traverse (traverse f) as <*> traverse f ja

instance Traversable f => Traversable (Wrap f) where

instance Traversable f => Traversable (WrapList f) where

instance (Traversable f, Traversable g) => Traversable (Compose f g) where

instance Traversable Tree2 where


-- Írjunk az alábbi datára Traversable instance-ot!
-- Deriving semmilyen formában nem használható!

data InfoTree e a = Leaf' e | Node' (InfoTree e a) e a (InfoTree e a) deriving (Foldable, Functor)

instance Traversable (InfoTree e) where
  traverse :: Applicative f => (a -> f b) -> InfoTree e a -> f (InfoTree e b)
  traverse f (Leaf' e) = pure (Leaf' e) -- Leaf' <$> pure e
  traverse f (Node' l e a r) = Node' <$> traverse f l <*> pure e <*> f a <*> traverse f r
