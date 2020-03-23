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
  fmap f (L x) = L x
  fmap f (R x) = R $ f x

instance Functor (Prod a) where 
  fmap :: (b -> c) -> Prod a b -> Prod a c  
  fmap f (P x y) = P x (f y) 

instance Functor Id where 
  fmap :: (a -> b) -> Id a -> Id b  
  fmap f (Id x) = Id $ f x 

instance Functor (Const a) where 
  fmap :: (b -> c) -> Const a b -> Const a c 
  fmap f (Const x) = Const x

data List a = Nil             -- []
            | Cons a (List a) -- (:)
  deriving (Eq, Ord, Show)

instance Functor List where 
  fmap :: (a -> b) -> List a -> List b 
  fmap f Nil = Nil 
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

data BinTree l n = Leaf l | Node n (BinTree l n) (BinTree l n)
  deriving (Eq, Show, Ord)

instance Functor (BinTree l) where 
  fmap :: (n -> n') -> BinTree l n -> BinTree l n' 
  fmap _ (Leaf x) = Leaf x 
  fmap f (Node x lhs rhs) = Node (f x) (fmap f lhs) (fmap f rhs)


newtype BinTreeFlipped n l = Flip { unflip :: BinTree l n } 
  deriving (Eq, Ord, Show)

instance Functor (BinTreeFlipped n) where 
  fmap :: (l -> l') -> BinTreeFlipped n l -> BinTreeFlipped n l' 
  fmap f (Flip (Leaf x)) = Flip $ Leaf (f x)
  fmap f (Flip (Node x lhs rhs)) = Flip $ Node x (unflip $ fmap f $ Flip lhs) 
                                                 (unflip $ fmap f $ Flip rhs)

class Bifunctor (f :: * -> * -> *) where 
  bimap :: (a -> b) -> (c -> d) -> f a c -> f b d 

instance Bifunctor BinTree where 
  bimap :: (l -> l') -> (n -> n') -> BinTree l n -> BinTree l' n'
  bimap f g btree = unflip $ fmap f $ Flip $ fmap g btree

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