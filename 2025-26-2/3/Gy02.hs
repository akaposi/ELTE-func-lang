{-# LANGUAGE InstanceSigs, QuantifiedConstraints, StandaloneDeriving, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{- HLINT ignore "Use newtype instead of data" -}

module Gy02 where

import Prelude hiding (Either (..), Maybe (..))
import Data.Kind (Type)

-- Let's take the following data types
-- type Single :: Type -> Type 
data Single a = Single a deriving (Eq, Show)
data Tuple a = Tuple a a deriving (Eq, Show)
data Quintuple a = Quintuple a a a a a deriving (Eq, Show)
data List a = Nil | Cons a (List a) deriving (Eq, Show)
data Maybe a = Just a | Nothing deriving (Eq, Show)

-- Let's try to create functions that change the type parameter of the above types
-- E.g.: Single a -> Single b or List a -> List b
-- Since the above types all contain elements of type 'a' in some way, we will need a function of type a -> b.

mapSingle :: (a -> b) -> Single a -> Single b
mapSingle g (Single x) = Single (g x)

mapTuple :: (a -> b) -> Tuple a -> Tuple b
mapTuple g (Tuple x x') = Tuple (g x) (g x')

mapQuintuple :: (a -> b) -> Quintuple a -> Quintuple b
mapQuintuple g (Quintuple x y z u v) = Quintuple (g x) (g y) (g z) (g u) (g v)

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe g (Just x) = Just (g x)

mapList :: (a -> b) -> List a -> List b
mapList _ Nil = Nil
mapList g (Cons x xs) = Cons (g x) (mapList g xs)

-- Let's lift Single, Tuple, etc. from the type (this is called higher-kinded polymorphism because we apply polymorphism to type-level functions):
{-

        mapSingle    :: (a -> b) -> Single    a -> Single    b
        mapTuple     :: (a -> b) -> Tuple     a -> Tuple     b
        mapQuintuple :: (a -> b) -> Quintuple a -> Quintuple b

        map          :: (a -> b) ->     f     a ->     f     b
-}

-- A type supporting a mapping method like this is known as a Functor
{-
:i Functor
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
        -- Defined in ‘GHC.Base’
-}

-- The rule for Functor informally: it preserves structure
-- So it doesn't change the order, location, or number of constructors.
-- fmap (\x -> x) = \x -> x
-- fmap (g . f) = (fmap g) . (fmap f)

instance Functor Single where
  fmap :: (a -> b) -> Single a -> Single b
  fmap = mapSingle

instance Functor Tuple where
  fmap :: (a -> b) -> Tuple a -> Tuple b
  fmap = mapTuple

instance Functor Quintuple where
  fmap :: (a -> b) -> Quintuple a -> Quintuple b
  fmap = mapQuintuple

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap = mapMaybe

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap = mapList

data NonEmpty a = Last a | NECons a (NonEmpty a) deriving (Eq, Show)
data NonEmpty2 a = NECons2 a (List a) deriving (Eq, Show)
data Either e a = Left e | Right a deriving (Eq, Show)
data BiTuple e a = BiTuple e a deriving (Eq, Show)
data TriEither e1 e2 a = LeftT e1 | MiddleT e2 | RightT a deriving (Eq, Show)

-- Let's write the Functor instances for the other types too!

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap g (Last x) = Last (g x)
  fmap g (NECons x ne) = NECons (g x) (fmap g ne)

instance Functor NonEmpty2 where
  fmap :: (a -> b) -> NonEmpty2 a -> NonEmpty2 b
  fmap g (NECons2 x l) = NECons2 (g x) (fmap g l)

-- Functor expects a Type -> Type kind type, but Either is a Type -> Type -> Type kind type, so the first parameter must be fixed.

instance Functor (Either fixed) where
  fmap :: (a -> b) -> Either fixed a -> Either fixed b
  fmap _ (Left e) = Left e
  fmap g (Right x) = Right (g x)

instance Functor (BiTuple fixed) where
  fmap :: (a -> b) -> BiTuple fixed a -> BiTuple fixed b
  fmap g (BiTuple e x) = BiTuple e (g x)

instance Functor (TriEither fixed1 fixed2) where
  fmap :: (a -> b) -> TriEither fixed1 fixed2 a -> TriEither fixed1 fixed2 b
  fmap g (LeftT e) = LeftT e
  fmap g (MiddleT e) = MiddleT e
  fmap g (RightT x) = RightT (g x)

data BiList a b = ACons a (BiList a b) | BCons b (BiList a b) | ABNill deriving (Eq, Show)

instance Functor (BiList fixed) where
  fmap :: (a -> b) -> BiList fixed a -> BiList fixed b
  fmap g ABNill = ABNill
  fmap g (ACons e bl) = ACons e (fmap g bl)
  fmap g (BCons a bl) = BCons (g a) (fmap g bl)

-- Special Kind annotation to specify the type of a 
type    Lift :: (Type -> Type) -> Type -> Type
newtype Lift f a = Lift (f a) deriving (Eq, Show)

-- data BiTuple e a = BiTuple e a  deriving (Eq, Show)
-- data Lift    f a = Lift   (f a) deriving (Eq, Show)
-- There's a difference

-- Examples:
listOfInts :: Lift List Int
listOfInts = Lift (Cons 1 (Cons 2 Nil))

maybeABool :: Lift Maybe Bool
maybeABool = Lift (Just False)

-- We need to fix the first parameter so that we can write Functor instance for it
-- However, we need a Functor constraint on the fixed type so that we can replace a in it
instance (Functor f) => Functor (Lift f) where
  fmap :: Functor f => (a -> b) -> Lift f a -> Lift f b
  fmap g (Lift fa) = Lift (fmap g fa)

-- f is a Functor
-- g : a -> b
-- fa : Functor f => f a

-- Similar types
data Sum f g a = SumLeft (f a) | SumRight (g a) deriving (Eq, Show)
data Product f g a = Product (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Sum f g a -> Sum f g b
  fmap = undefined

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Product f g a -> Product f g b
  fmap = undefined

-- Hard

data Compose f g a = Compose (f (g a)) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Compose f g a -> Compose f g b
  fmap h (Compose fga) = Compose (fmap (fmap h) fga)

-- Functor f means we have fmap1 : (a -> b) -> f a -> f b
-- Functor g means we have fmap2 : (a -> b) -> g a -> g b  
-- Have fga : f (g a), h : a -> b
-- Need ? : f (g b), ? = fmap1 (fmap2 h) fga
-- fmap2 h : g a -> g b
-- fmap1 (fmap2 h) : f (g a) -> f (g b)

-- Are functions Functors?
data Fun a b = Fun (a -> b)

instance Functor (Fun q) where
  -- fmap :: (a -> b) -> (q -> a) -> (q -> b)
  fmap :: (a -> b) -> Fun q a -> Fun q b
  fmap g (Fun q2a) = Fun (g . q2a) --Fun (\q -> g (q2a q))

-- Other fun stuff:
data UselessF f a = Mk1 (f Int) a
--                       ^ f is not in a position where it needs to be mapped, so the Functor f constraint is unnecessary.

-- Practice:

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show)
data RoseTree a = RoseLeaf a | RoseNode [RoseTree a] deriving (Eq, Show)
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a) deriving (Eq, Show)
data SkipList a = Skip (SkipList a) | SCons a (SkipList a) | SNill deriving (Eq, Show)
data CrazyType a = C1 a a | C2 a Int | C3 (CrazyType a) deriving (Eq, Show)
data Either3 a b c = Left3 a | Middle3 b | Right3 c deriving (Eq, Show)
data Triplet a b c = Triplet a b c deriving (Eq, Show)
data SplitTree a b = SplitTree (Tree a) a b (Tree b) deriving (Eq, Show)
data TriCompose f g h a = TriCompose (f (g (h a))) deriving (Eq, Show)
data Free f a = Pure a | Free (f (Free f a))
type Fix :: (Type -> Type) -> Type -> Type
data Fix f a = Fix (f (Fix f a))
data Join a b = Join (a -> a -> b)
data CrazyType2 a b = SingleA a | SingleB b | Translate (a -> b)

-- Dont mind this

deriving instance (Eq a, forall q. (Eq q) => Eq (f q)) => Eq (Free f a)

deriving instance (Show a, forall q. (Show q) => Show (f q)) => Show (Free f a)

deriving instance (Eq a, forall q. (Eq q) => Eq (f q)) => Eq (Fix f a)

deriving instance (Show a, forall q. (Show q) => Show (f q)) => Show (Fix f a)

deriving instance (Show a, Show (f Int)) => Show (UselessF f a)

deriving instance (Eq a, Eq (f Int)) => Eq (UselessF f a)
