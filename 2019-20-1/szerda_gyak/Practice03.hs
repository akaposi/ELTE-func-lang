module Practice03 where

import Prelude hiding (Maybe(..))

newtype Fun a b = Fun (a -> b)

instance Semigroup b => Semigroup (Fun a b) where 
  (<>) (Fun f) (Fun g) = Fun $ \x -> f x <> g x

instance Monoid b => Monoid (Fun a b) where 
  mempty = Fun $ \x -> mempty

data Maybe a = Nothing | Just a
  deriving (Eq, Ord, Show)

instance Functor Maybe where 
  fmap _ Nothing = Nothing 
  fmap f (Just x) = Just (f x)

instance Functor (Fun a) where 
  fmap f (Fun fun) = Fun $ \x -> f (fun x)

data InfiniTree k v
  = Nil
  | Branch v (k -> InfiniTree k v)

-- type MaybeTree a = InfiniTree _ _

-- type ListTree a = InfiniTree _ _ 

type BinTree a = InfiniTree Bool a

binTree :: BinTree Int 
binTree = Branch 1 (\x -> if x then Nil else Branch 2 (const Nil))

-- type RoseTree a = InfiniTree _ _
