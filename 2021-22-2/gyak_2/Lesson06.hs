{-# LANGUAGE InstanceSigs, DeriveFoldable #-}

module Lesson06 where

import Control.Applicative

data List a = Nil | Cons a (List a) deriving (Foldable, Show)
infixr 5 `Cons`

(+++) :: List a -> List a -> List a
xs +++ ys = foldr Cons ys xs
infixr 5 +++

data BinaryTree a = Leaf a | Node (BinaryTree a) a (BinaryTree a)
data RoseTree a = RoseNode a [RoseTree a]

data    Foo3 a      = Foo3 a a a a a deriving Show
data    Tree1 a     = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show
data    Pair a b    = Pair a b deriving Show
data    Either' a b = Left' a | Right' b deriving Show
data    Tree3 i a   = Leaf3 a | Node3 (i -> Tree3 i a)  -- i-szeres elágazás
newtype Id a        = Id a deriving Show
newtype Const a b   = Const a deriving Show

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap f Nil = Nil
    fmap f (Cons a as) = Cons (f a) $ fmap f as 

instance Functor BinaryTree where
    fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node t1 a t2) = Node (fmap f t1) (f a) (fmap f t2)

instance Functor Foo3 where
    fmap :: (a -> b) -> Foo3 a -> Foo3 b
    fmap f (Foo3 a1 a2 a3 a4 a5) = Foo3 (f a1) (f a2) (f a3) (f a4) (f a5)

instance Functor Tree1 where
    fmap :: (a -> b) -> Tree1 a -> Tree1 b
    fmap f (Leaf1 a) = Leaf1 $ f a
    fmap f (Node1 t1 t2) = Node1 (fmap f t1) (fmap f t2)

instance Functor (Pair c) where
    fmap :: (a -> b) -> Pair c a -> Pair c b
    fmap f (Pair c a) = Pair c $ f a

instance Functor RoseTree where
    fmap :: (a -> b) -> RoseTree a -> RoseTree b
    fmap f (RoseNode a as) = RoseNode (f a) (map (fmap f) as)
{-
instance Functor Tree3 where
    fmap = undefined

instance Functor Either' where
    fmap = undefined

instance Functor Id where
    fmap = undefined

instance Functor Const where
    fmap = undefined

instance Functor Fun where
    fmap = undefined
-}

-- Applicative: Minden "mellékhatás" statikusan ismert
{-
class Functor' f => Applicative' f where
    pure' :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    -- f :: Type -> Type
-}
-- Törvények:
{-
1. identitás:
pure id <*> x = x

2. homomorfizmus:
pure f <*> pure x = pure (f x)

3. felcserélhetőség (interchangability):
f <*> pure x = pure ($ x) <*> f

4. kompozíció
pure (.) <*> x <*> y <*> z = x <*> (y <*> z)

5.
pure f <*> x = fmap f x
-}

{-
data    Foo1 a      = Foo1 Int a a a deriving Show
data    Foo2 a      = Foo2 Bool a Bool deriving Show
data    Foo3 a      = Foo3 a a a a a deriving Show
data    Tree1 a     = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show
data    Pair a b    = Pair a b deriving Show
data    Either' a b = Left' a | Right' b deriving Show
data    Tree3 i a   = Leaf3 a | Node3 (i -> Tree3 i a)  -- i-szeres elágazás
newtype Id a        = Id a deriving Show
newtype Const a b   = Const a deriving Show
newtype Fun a b     = Fun (a -> b)
-}

instance Applicative List where
    pure :: a -> List a
    pure x = Cons x Nil
    (Cons f fs) <*> xs = foldr Cons (fs <*> xs) (fmap f xs)
    _           <*> _  = Nil

instance Applicative BinaryTree where
    pure x = Leaf x
    
    Leaf f <*> Leaf x = Leaf $ f x
    Leaf f <*> Node l x r = Node (Leaf f <*> l) (f x) (Leaf f <*> r)
    Node l f r <*> Leaf x = Node (l <*> Leaf x) (f x) (r <*> Leaf x)
    Node l1 f r1 <*> Node l2 x r2 = Node (l1 <*> l2) (f x) (r1 <*> r2)

instance Applicative Foo3 where
    pure x = Foo3 x x x x x 
    (Foo3 f1 f2 f3 f4 f5) <*> (Foo3 x1 x2 x3 x4 x5) = Foo3 (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5)

f :: [[a]] -> [a]
f [] = []
f (x:xs) = x ++ f xs
-- class Applicative' m => Monad' m where
    -- m :: Type -> Type
--    (>>=) :: m a -> (a -> m b) -> m b

-- join :: Monad m => m (m a) -> m a
-- Monad: "Mellékhatások" dinamikusak, nem tudni, hogy mi történik addig, amíg meg nem történik.

-- Törvények:
{-
bal identitás:
    pure a >>= f = f a

jobb identitás:
    x >>= pure = x

asszociativitás:
    (a >>= b) >>= c = a >>= (\x -> b x >>= c)

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)

asszociativitás V2:
    (a >=> b) >=> c = a >=> (b >=> c)
-}

instance Monad List where
    (>>=) :: List a -> (a -> List b) -> List b
    Nil >>= _ = Nil
    Cons x xs >>= f = f x +++ (xs >>= f)

instance Monad BinaryTree where
    (>>=) = undefined

instance Semigroup (Foo3 a) where
    (<>) = undefined

instance Monad Foo3 where
    Foo3 a1 a2 a3 a4 a5 >>= f = f a1 <> f a2 <> f a3 <> f a4 <> f a5

data Maybe' a = Nothing' | Just' a deriving Show

instance Functor Maybe' where
    fmap f Nothing' = Nothing'
    fmap f (Just' a) = Just' $ f a

instance Applicative Maybe' where
    pure = Just'
    Just' f <*> Just' x = Just' $ f x
    _       <*> _       = Nothing'

instance Monad Maybe' where
    Nothing' >>= _ = Nothing'
    Just' x  >>= f = f x

{-
instance Monad Tree1 where
    (>>=) = undefined

instance Monad Pair where
    (>>=) = undefined

instance Monad RoseTree where
    (>>=) = undefined

instance Monad Tree3 where
    (>>=) = undefined

instance Monad Either' where
    (>>=) = undefined

instance Monad Id where
    (>>=) = undefined

instance Monad Const where
    (>>=) = undefined
-}

f1 :: Monad m => (a -> b) -> m a -> m b
f1 f ma = ma >>= (\a -> pure $ f a)

-- f1' = fmap

f2 :: Monad m => m a -> m b -> m (a, b)
f2 ma mb = ma >>= (\a -> mb >>= (\b -> pure (a,b)))

-- f2' = liftA2 (,)

-- liftA2' f x y = fmap f x <*> y
-- liftA2'' f x y = f <$> x <*> y

f2'' :: Monad m => m a -> m b -> m (a, b)
f2'' ma mb = do
    a <- ma
    b <- mb
    pure (a,b)

f3 :: Monad m => m (m a) -> m a
f3 = undefined

f4 :: Monad m => m (a -> b) -> m a -> m b
f4 = undefined

f5 :: Monad m => (a -> m b) -> m a -> m b
f5 = undefined

f6 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
f6 = undefined

f7 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
f7 = undefined

f8 :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f8 = undefined

-----------------------------------------------------------------------
-- putStrLn :: String -> IO ()
helloWorld :: IO ()
helloWorld = putStrLn "Hello World!"
-- data () = ()

-- getLine :: IO String
greet :: IO ()
greet = getLine >>= (\s -> putStrLn $ "Szia " ++ s ++ "!")

greet' :: IO ()
greet' = do
    s <- getLine
    putStrLn $ "Szia " ++ s ++ "!"

-- putStr :: String -> IO ()
prompt :: String -> IO String
prompt s = putStr s >>= (\_ -> getLine)

prompt' :: String -> IO String
prompt' s = do
    _ <- putStr s
    getLine

prompt'' :: String -> IO String
prompt'' s = putStr s >> getLine

prompt''' :: String -> IO String
prompt''' s = do
    putStr s
    getLine