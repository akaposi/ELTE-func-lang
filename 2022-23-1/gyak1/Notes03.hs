{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

------------------------------------------------------------
-- Következő kisfeladat: Functor instance
--   valamilyen nem rekurzív adattípusra

------------------------------------------------------------

-- Kisfeladat megoldás

-- max :: Int -> Int -> Int
depth :: Tree a -> Int
depth (Leaf _)   = 0
depth (Node l r) = max (depth l) (depth r) + 1

-- ghci-ben:

-- > :i <név>         név lehet: definíció, típus, osztály, (operátor)
-- pl
-- > :i (+)           megkapom: típus, melyik osztály metódusa,
--                    precendencia, "fixitás"

-- > :i Num
-- > :i Int
-- > :i not

--------------------------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)
  -- deriving (Eq, Show, Ord)
data Color = Red | Green | Blue
  -- deriving (Eq, Show, Ord)

instance Eq Color where
  (==) Red Red = True
  (==) Green Green = True
  (==) Blue Blue = True
  (==) _ _ = False

instance Ord Color where
  (<=) Red   _     = True
  (<=) Green Green = True
  (<=) Green Blue  = True
  (<=) Blue  Blue  = True
  (<=) _     _     = False

instance Show Color where
  show Red   = "Red"
  show Green = "Green"
  show Blue  = "Blue"

instance Eq a => Eq (Tree a) where
  (==) (Leaf a)   (Leaf a')    = a == a'
  (==) (Node l r) (Node l' r') = l == l' && r == r'
  (==) _          _            = False

instance Ord a => Ord (Tree a) where  -- lexikografikus
  (<=) (Leaf a)   (Leaf a')    = a <= a'
  (<=) (Leaf _)   (Node _ _)   = True
  (<=) (Node l r) (Node l' r') = l <= l' && r <= r'
  (<=) (Node _ _) (Leaf _)     = False

instance Show a => Show (Tree a) where
  -- bónusz: adj meg hatékony definíciót!

  -- naiv definíció (kvadratikus)
  -- show :: Tree a -> String
  -- show (Leaf a)   = "(Leaf " ++ show a ++ ")"
  -- show (Node l r) = "(Node " ++ show l ++ show r ++ ")"

  -- hatékony
  show :: Tree a -> String
  show t = showSTree t ""

-- standard típus: type ShowS = String -> String
showSTree :: Show a => Tree a -> ShowS
showSTree (Leaf a)   = ("(Leaf "++) . (show a++) . (")"++)
showSTree (Node l r) = ("(Node "++) . showSTree l . showSTree r . (")"++)

-- -- ugyanez a definíció kibontva:
-- showSTree :: Show a => Tree a -> String -> String
-- showSTree (Leaf a)   acc = "(Leaf " ++ show a ++ ")"++ acc
-- showSTree (Node l r) acc = "(Node "++ showSTree l (showSTree r (")"++acc))

-- Functor
--------------------------------------------------------------------------------

data    Foo1 a      = Foo1 Int a a a
data    Foo2 a      = Foo2 Bool a Bool
data    Foo3 a      = Foo3 a a a a a
data    Tree1 a     = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show
data    Tree2 a     = Node2 a [Tree2 a] deriving Show
data    Pair a b    = Pair a b
data    Either' a b = Left' a | Right' b
data    Tree3 i a   = Leaf3 a | Node3 (i -> Tree3 i a)
newtype Id a        = Id a
newtype Const a b   = Const a
newtype Fun a b     = Fun (a -> b)

{-
class Functor f where              -- "f" az egy 1-paraméteres típus
  fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing  = Nothing
  fmap f (Just a) = Just (f a)

instance Functor [] where
  fmap :: (a -> b) -> [] a -> [] b
  fmap = map                         -- standard map függvény
-}

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a)   = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

------------------------------------------------------------

-- data Foo1 a = Foo1 Int a a a

instance Functor Foo1 where
  fmap :: (a -> b) -> Foo1 a -> Foo1 b
     -- típust ghci-ben ki lehet nyomtatni
     -- ha _-t adunk meg
  fmap f (Foo1 n a1 a2 a3) = Foo1 n (f a1) (f a2) (f a3)
     -- helyes megoldásra igaz:
     --   fmap id x = x

instance Functor Tree1 where
  fmap = undefined

-- data Pair a b = Pair a b

-- ha több típusparaméter van, akkor az *utolsó*
-- paraméter fölött tudunk fmap-elni
instance Functor (Pair c) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap :: (a -> b) -> Pair c a -> Pair c b
  fmap f (Pair c a) = Pair c (f a)

instance Functor Foo2 where
  fmap = undefined

instance Functor Foo3 where
  fmap = undefined

instance Functor Tree2 where
  fmap = undefined

instance Functor (Tree3 i) where
  fmap = undefined

instance Functor (Either' a) where
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
newtype Exp x f a = Exp (x -> f a)

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap = undefined

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap = undefined

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap = undefined

instance Functor f => Functor (Exp x f) where
  fmap = undefined

-- bónusz bónusz: mire használható ez a függvény? Tipp: a megoldáshoz
-- rekurzió szükséges.
löb :: Functor f => f (f a -> a) -> f a
löb = undefined

-- bónusz bónusz 2:
newtype Fix f = Fix (f (Fix f))

fold :: Functor f => (f a -> a) -> Fix f -> a
fold = undefined
