{-# language StandaloneDeriving, UndecidableInstances #-}


-- köv feladat:
--   Functor instance, nem rekurzív adattípusra

------------------------------------------------------------

f :: Either (a -> b) (a -> c) -> a -> Either b c
f (Left g)  a = Left (g a)
f (Right g) a = Right (g a)



-- Osztályok, algebrai típusok
--------------------------------------------------------------------------------

data Color = Red | Green | Blue
data List a = Nil | Cons a (List a)
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

-- (standard, eleve importált)
-- class Eq a where
--   (==) :: a -> a -> Bool

-- instance Eq Bool where
--   (==) :: Bool -> Bool -> Bool
--   (==) True True = True
--   (==) False False = True
--   (==) _ _ = False

-- instance megszorítás lehet tetszőleges definíción:
--    f :: Eq a => a -> a -> a -> a
--    f x y z = if x == y then x else z

-- instance Eq a => Eq [a] where
--    (==) [] [] = True
--    (==) (x:xs) (y:ys) = x == y && xs == ys
--    (==) _ _ = False

-- írd meg a következő instance-okat!

instance Eq Color where
  (==) :: Color -> Color -> Bool
  (==) Red Red = True
  (==) Blue Blue = True
  (==) Green Green = True
  (==) _ _ = False

instance Ord Color where
  (<=) :: Color -> Color -> Bool
  (<=) Red _ = True
  (<=) Green Green = True
  (<=) Green Blue = True
  (<=) Blue Blue = True
  (<=) _ _ = False

-- class Show a where
--   show :: a -> String

-- (ghci-beli kinyomtatáshoz Show instance kell)

instance Show Color where
  show :: Color -> String
  show Red   = "Red"
  show Green = "Green"
  show Blue  = "Blue"

--  data List a = Nil | Cons a (List a)

instance Eq a => Eq (List a) where
  (==) :: List a -> List a -> Bool
  (==) Nil         Nil         = True
  (==) (Cons x xs) (Cons y ys) = x == y && xs == ys
  (==) _           _           = False

-- Eq, Ord, Show

-- data Tree a = Leaf a | Node (Tree a) (Tree a)
--   deriving (Eq, Show, Ord)

instance Show a => Show (List a) where
  show :: List a -> String
  show = undefined

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) = undefined

-- instance Show a => Show (Tree a) where
--   show :: Tree a -> String
--   show = undefined


-- Functor
--------------------------------------------------------------------------------

-- lista map:
-- map :: (a -> b) -> [a] -> [b]
-- map f []     = []
-- map f (a:as) = f a : map f as

-- treeMap :: (a -> b) -> Tree a -> Tree b

-- maybeMap :: (a -> b) -> Maybe a -> Maybe b

-- feature: paraméteres típusokra is lehet bármi
-- generikus/polimorf
id' :: f a -> f a
id' x = x

-- -- Functor osztály metódusa a "map" túlterhelése
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b


-- írd meg a következő instance-okat!

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap f Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

-- data Tree a = Leaf a | Node (Tree a) (Tree a)
instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a)   = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)
  -- bejárjuk a fát, minden levélre egy függvényt alkalmazunk

data Twice a = Twice a a deriving (Eq, Show)

instance Functor Twice where
  fmap :: (a -> b) -> Twice a -> Twice b
  fmap = undefined

data Triple a = Triple a a a deriving (Eq, Show)

instance Functor Triple where
  fmap :: (a -> b) -> Triple a -> Triple b
  fmap = undefined

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair c) where
  fmap :: (a -> b) -> Pair c a -> Pair c b
  fmap = undefined

data Foo1 a = Foo1 Int a a a deriving Show

instance Functor Foo1 where
  fmap = undefined

data Foo2 a = Foo2 Bool a Bool deriving Show

instance Functor Foo2 where
  fmap = undefined

data RoseTree a = RTNode a [RoseTree a] deriving Show

leaf :: RoseTree Int
leaf = RTNode 100 []  -- ha lista üres ~ levél

-- (RoseTree : választható elágazású fa)

instance Functor RoseTree where
  fmap :: (a -> b) -> RoseTree a -> RoseTree b
  fmap f (RTNode a ts) = RTNode (f a) (fmap (fmap f) ts)
    -- ts  :: [RoseTree a]
    -- cél :: [RoseTree b]
    -- map :: (a -> b) -> [a] -> [b]
    --     (\t -> fmap f t) :: RoseTree a -> RoseTree b
    -- map (\tg -> fmap f t) ts :: [RoseTree b]
    -- map (fmap f) ts          :: [RoseTree b]

    -- egymásba típusok esetén:
    --   rekurzív függvények ugyanazt a beágyazást követik

newtype Id a = Id a deriving Show

instance Functor Id where
  fmap = undefined

newtype Const a b = Const a deriving Show

instance Functor (Const a) where
  fmap = undefined

newtype Fun a b = Fun (a -> b)

instance Functor (Fun a) where
  fmap = undefined


-- Bónusz feladatok
--------------------------------------------------------------------------------

-- Add meg a következő definíciókat típushelyesen,
-- végtelen loop és kivételek nélkül!

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = undefined

apply :: Functor f => f (a -> b) -> a -> f b
apply = undefined

first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first = undefined

second :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
second = undefined

data Sum f g a = Inl (f a) | Inr (g a) deriving Show

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap = undefined

data Product f g a = Product (f a) (g a) deriving Show

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap = undefined

newtype Compose f g a = Compose (f (g a)) deriving Show

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap = undefined

newtype Exp x f a = Exp (x -> f a)

instance Functor f => Functor (Exp x f) where
  fmap = undefined

-- Mire használható ez a függvény? Tipp: a megoldáshoz rekurzió szükséges.
löb :: Functor f => f (f a -> a) -> f a
löb = undefined

newtype Fix f = Fix (f (Fix f))

-- A "StandaloneDeriving" opciót használjuk itt, mert
-- a standard "deriving Show" már nem működik.
deriving instance Show (f (Fix f)) => Show (Fix f)

fold :: Functor f => (f a -> a) -> Fix f -> a
fold = undefined

data Free f a = Pure a | Free (f (Free f a))

deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)

instance Functor f => Functor (Free f) where
  fmap = undefined

newtype Cont r a = Cont ((a -> r) -> r)

instance Functor (Cont r) where
  fmap = undefined
