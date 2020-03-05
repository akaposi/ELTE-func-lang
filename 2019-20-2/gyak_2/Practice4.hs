{-# LANGUAGE InstanceSigs, KindSignatures #-}
module Practice4 where

data Sum a b = L a | R b
  deriving (Eq, Show, Ord)

data Prod a b = P a b 
  deriving (Eq, Ord, Show)

data Id a = Id a 
  deriving (Eq, Ord, Show)

data Const a b = Const a 
  deriving (Eq, Ord, Show)

instance Functor (Sum a) where 
  fmap :: (b -> c) -> Sum a b -> Sum a c  
  fmap f (L x) = undefined 
  fmap f (R x) = undefined

instance Functor (Prod a) where 
  fmap :: (b -> c) -> Prod a b -> Prod a c  
  fmap f (P x y) = undefined 

instance Functor Id where 
  fmap :: (a -> b) -> Id a -> Id b  
  fmap f (Id x) = undefined 

instance Functor (Const a) where 
  fmap :: (b -> c) -> Const a b -> Const a c 
  fmap f (Const x) = undefined

data List a = Nil             -- []
            | Cons a (List a) -- (:)
  deriving (Eq, Ord, Show)

instance Functor List where 
  fmap :: (a -> b) -> List a -> List b 
  fmap = undefined

data BinTree l n = Leaf l | Node n (BinTree l n) (BinTree l n)
  deriving (Eq, Show, Ord)

instance Functor (BinTree l) where 
  fmap :. (n -> n') -> BinTree l n -> BinTree l n' 
  fmap = undefined

newtype BinTreeFlipped n l = Flip { unflip :: BinTree l n } 
  deriving (Eq, Ord, Show)

instance Functor (BinTreeFlipped n) where 
  fmap :. (l -> l') -> BinTreeFlipped n l' -> BinTreeFlipped n l' 
  fmap = undefined

class Bifunctor (f :: * -> * -> *) where 
  bimap :: (a -> b) -> (c -> d) -> f a c -> f b d 

instance Bifunctor BinTree where 
  bimap :: (l -> l') -> (n -> n') -> BinTree l n -> BinTree l' n'
  bimap = undefined

newtype Fun a b = Fun {getFun :: a -> b}

instance Functor (Fun x) where 
  fmap :: (r -> r') -> (Fun x r) -> (Fun x r')
  fmap = undefined


-- record syntax
data T a = T 
  { getX :: Int
  , getY :: Int
  , getA :: (a, a)
  } deriving (Eq, Ord, Show)

-- data T a = T Int Int (a, a)

-- getX :: T a -> Int 
-- getX (T x _ _) = x

-- getY :: T a -> Int 
-- getY (T _ y _) = y

-- getA :: T a -> (a, a)
-- getA (T _ _ aa) = aa