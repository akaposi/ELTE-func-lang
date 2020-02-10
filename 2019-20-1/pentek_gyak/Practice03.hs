{-# LANGUAGE InstanceSigs #-}
module Practice03 where

{-
data Coord = C 
  { x :: Int
  , y :: Int 
  } deriving (Eq, Ord, Show)

x :: Coord -> Int 
x (C c1 c2) = c1
-}

newtype Fun a b = Fun { appF :: a -> b }

-- class C1 a => C2 a where
-- instance C2 a -- C1 a must be defined

-- instance C1 a => C1 (T a) where
-- when i use C1 (T a), (C1 a) must be defined

instance Semigroup b => Semigroup (Fun a b) where 
  (<>) :: Fun a b -> Fun a b -> Fun a b
  (<>) (Fun f) (Fun g) = Fun $ \x -> f x <> g x

instance Monoid b => Monoid (Fun a b) where 
  mempty :: Fun a b 
  mempty = Fun $ const mempty

{-
data T a = TODO
  deriving (Eq, Ord, Show)

lift :: a -> T a

instance Semigroup a => Semigroup (T a) where
  -- TODO: (<>) :: T a -> T a -> T a
  -- lift law: lift (x <> y) == lift x <> lift y

instance Semigroup a => Monoid (T a) where
  -- TODO: mempty :: T a
  -- lift unitR: lift x <> mempty == lift x
  -- lift unitL: mempty <> lift y == lift y
-}

data InfiniTree k v
  = Nil 
  | Branch v (k -> InfiniTree k v)

data Void

data Nat = Zero | Suc Nat

type Maybe'    a = InfiniTree Void a
type List'     a = InfiniTree () a
type BinTree'  a = InfiniTree Bool a
type RoseTree' a = InfiniTree Nat a

binTree :: BinTree' Int 
binTree = Branch 1 (\b -> if b then Nil else (Branch 2 $ \b2 -> if b2 then Nil else Nil))
