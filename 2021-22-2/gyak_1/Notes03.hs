
{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

import Prelude hiding (Either(..), Functor(..))

-- canvas feladat
------------------------------------------------------------

f :: [Maybe Int] -> Int
f []           = 0
f (Nothing:xs) = f xs
f (Just x :xs) = x + f xs

-- f' :: [Maybe Int] -> Int
-- f' []     = 0
-- f' (x:xs) = case x of
--   Nothing -> f' xs
--   Just x  -> x + f' xs

-- f' :: [Maybe Int] -> Int
-- f' []     = 0
-- f' (x:xs) =
--   let sumxs = f' xs
--   in case x of
--     Nothing -> sumxs
--     Just x  -> x + sumxs

-- f' :: [Maybe Int] -> Int
-- f' []     = 0
-- f' (x:xs) = f' xs + (case x of Nothing -> 0; Just x -> x)

-- f' :: [Maybe Int] -> Int
-- f' = sum . map (\x -> case x of Nothing -> 0; Just x -> x)

-- f' :: [Maybe Int] -> Int
-- f' xs = sum [case x of Nothing -> 0; Just x -> x | x <- xs]


--------------------------------------------------------------------------------

-- köv canvas feladat: Functor instance nem túl bonyolult fa típusra
--                       - nincs egymásba ágyazott típus (pl [Tree])
--                       - nincs függvényes elágazás

--------------------------------------------------------------------------------

class Functor f where                   -- f    : 1 paraméteres típus
  fmap :: (a -> b) -> f a -> f b        -- fmap : általános "map" függvény f-re

-- [] Int
-- Maybe Int

data    Foo1 a      = Foo1 Int a a a deriving Show
data    Foo2 a      = Foo2 Bool a Bool deriving Show
data    Foo3 a      = Foo3 a a a a a deriving Show
data    Foo4 a b    = Foo4A a b a | Foo4B Int Int b deriving Show
data    Stream a    = Cons a (Stream a)
data    Tree1 a     = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show
data    Tree2 a     = Node2 a [Tree2 a] deriving Show
data    Tree3 i a   = Leaf3 a | Node3 (i -> Tree3 i a)  -- i-szeres elágazás
data    Tree4 a b   = Node4 a (Tree4 a b) (Tree4 a b) | Leaf4 a b
data    Pair a b    = Pair a b deriving Show
data    Either a b  = Left a | Right b deriving Show

newtype Id a        = Id a deriving Show
newtype Const a b   = Const a deriving Show
newtype Fun a b     = Fun (a -> b)

-- data Foo1 a = Foo1 Int a a a deriving Show

instance Functor Foo1 where
  fmap :: (a -> b) -> Foo1 a -> Foo1 b
  fmap f (Foo1 n a1 a2 a3) = Foo1 n (f a1) (f a2) (f a3)

-- fmap (+10) (Foo1 20 30 40 50) == fmap (+10) $ Foo1 10 20 30 40
-- fmap (\x -> [x]) (Foo1 20 30 40 50) == Foo1 20 [30] [40] [50]

-- futásidejű típusinformáció: Data.Typeable

instance Functor Foo2 where
  fmap = undefined

instance Functor Foo3 where
  fmap = undefined

-- data Foo4 a b = Foo4A a b a | Foo4B Int Int b deriving Show
-- mindig az *utolsó* típusparaméter fölött tudunk fmap-elni!

-- Foo4   :: * -> * -> *
-- Foo4 c :: * -> *

instance Functor (Foo4 c) where
  fmap :: (a -> b) -> Foo4 c a -> Foo4 c b
  fmap f (Foo4A c1 a c2) = Foo4A c1 (f a) c2
  fmap f (Foo4B n1 n2 a) = Foo4B n1 n2 (f a)


-- vagy:
-- class Bifunctor f where
--   bimap :: (a -> a') -> (b -> b') -> f a b -> f a' b'
-- mapFoo41 :: (a -> b) -> Foo4 a c -> Foo4 b c

instance Functor Stream where
  fmap = undefined

instance Functor Tree1 where
  fmap = undefined

instance Functor (Pair a) where
  fmap = undefined

-- data Tree2 a = Node2 a [Tree2 a] deriving Show
--   tetszőlegesen elágazó fa

instance Functor Tree2 where
  fmap :: (a -> b) -> Tree2 a -> Tree2 b
  fmap f (Node2 a ts) = Node2 (f a) (map (fmap f) ts)
     -- ts :: [Tree2 a]
     -- _  :: [Tree2 b]
     -- map _ ts       _ :: Tree2 a -> Tree2 b


-- data Tree3 i a = Leaf3 a | Node3 (i -> Tree3 i a)  -- i-szeres elágazás
instance Functor (Tree3 i) where
  fmap f (Leaf3 a)  = Leaf3 (f a)
  fmap f (Node3 ts) = Node3 (\i -> fmap f (ts i))
      -- i-edik fa a map-elt sorozatban = map-elt (i-edik fa a régi sorozatban)

  -- fmap f (Node3 ts) = Node3 (fmap f . ts)

-- konstruktor belsejében függvény típusú mező
--    Node3: i-szeres elágazás

t1 :: Tree3 Bool Int   -- bináris fa
t1 = Node3 (\b -> if b then Leaf3 10 else Leaf3 20)

t2 :: Tree3 Int Int
t2 = Node3 (\n -> Leaf3 (n + 10))

t3 :: Tree3 Int Int
t3 = Node3 (\n -> Node3 (\m -> (Leaf3 (n + m))))




-- data Tree4 a b = Node4 a (Tree4 a b) (Tree4 a b) | Leaf4 a b

instance Functor (Tree4 c) where
  fmap :: (a -> b) -> Tree4 c a -> Tree4 c b
  fmap f (Leaf4 c a)   = Leaf4 c (f a)
  fmap f (Node4 c l r) = Node4 c (fmap f l) (fmap f r)

instance Functor (Either a) where
  fmap = undefined

instance Functor Id where
  fmap = undefined

instance Functor (Const a) where
  fmap = undefined

instance Functor (Fun a) where
  fmap = undefined


-- Bónusz feladatok
--------------------------------------------------------------------------------

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = undefined

apply :: Functor f => f (a -> b) -> a -> f b
apply = undefined

first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first = undefined

second :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
second = undefined

data Sum f g a = Inl (f a) | Inr (g a) deriving Show
data Product f g a = Product (f a) (g a) deriving Show
newtype Compose f g a = Compose (f (g a)) deriving Show

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap = undefined

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap = undefined

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap = undefined


-- bónusz bónusz: mire használható ez a függvény? Tipp: a megoldáshoz
-- rekurzió szükséges.
löb :: Functor f => f (f a -> a) -> f a
löb = undefined

-- bónusz bónusz 2:
newtype Fix f = Fix (f (Fix f))

fold :: Functor f => (f a -> a) -> Fix f -> a
fold = undefined
