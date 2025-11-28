{-# LANGUAGE TypeFamilies #-}

module Ea10 where
{-
import Prelude hiding (id, (.), fst, snd)
import Data.Kind

{-
Objects
Morphisms
-}

type Category :: forall ob. (ob -> ob -> Type) -> Constraint
class Category mor where
  id :: mor a a
  (.) :: mor b c -> mor a b -> mor a c
  -- (f . g) . h = f . (g . h)
  -- id . f = f
  -- f . id = f

-- Hask category
instance Category (->) where
  id = \x -> x
  f . g = \x -> f (g x)

newtype Const2 m a b = Const2 m

instance Monoid m => Category (Const2 m) where
  id = Const2 mempty
  Const2 x . Const2 y = Const2 (x <> y)

newtype Kleisli m a b = Kleisli (a -> m b)

instance Monad m => Category (Kleisli m) where
  id = Kleisli $ \x -> pure x
  Kleisli f . Kleisli g = Kleisli $ \x -> do
    y <- g x
    f y

-- cartesian category = has products ~ (,)

class Category mor => Cartesian mor where
  type Product (mor :: ob -> ob -> Type) :: ob -> ob -> ob

  fst :: mor (Product mor a b) a -- (a, b) -> a
  snd :: mor (Product mor a b) b -- (a, b) -> b
  pair :: mor a b -> mor a c -> mor a (Product mor b c)
  -- (a -> b) -> (a -> c) -> a -> (b, c)

  -- fst . pair f g = f
  -- snd . pair f g = g
  -- pair (fst . f) (snd . f) = f

instance Cartesian (->) where
  type Product (->) = (,)

  fst (a, b) = a
  snd (a, b) = b
  pair f g a = (f a, g a)

-- cartesian closed category
class Cartesian mor => CCC mor where
  type Exp (mor :: ob -> ob -> Type) :: ob -> ob -> ob

  app :: mor (Product mor (Exp mor b a) a) b
  -- (a -> b, a) -> b
  lam :: mor (Product mor a b) c -> mor a (Exp mor c b)
  -- ((a, b) -> c) -> a -> (b -> c)

-- |a -> b| = |b|^|a|

newtype Op b a = Op (a -> b)

instance CCC (->) where
  type Exp (->) = Op

  app (Op f, a) = f a
  lam f a = Op $ \b -> f (a, b)
-}

-- deriving (Eq, Ord, Functor)

import GHC.Generics
import Data.Bits

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Generic)

-- Tree a ~ Either a (Tree, Tree)

class GEq a where
  geq :: a -> a -> Bool

instance (GEq (f p), GEq (g p)) => GEq ((f :*: g) p) where
  geq (a :*: b) (c :*: d) = geq a c && geq b d

instance (GEq (f p), GEq (g p)) => GEq ((f :+: g) p) where
  geq (L1 a) (L1 b) = geq a b
  geq (R1 a) (R1 b) = geq a b
  geq _ _ = False

instance Eq c => GEq (K1 i c p) where
  geq (K1 a) (K1 b) = a == b

instance GEq (f p) => GEq (M1 i c f p) where
  geq (M1 a) (M1 b) = geq a b

instance GEq Int where
  geq a b = a == b

eqDefault :: forall a. (Generic a, GEq (Rep a ())) => a -> a -> Bool
eqDefault x y = geq (from x :: Rep a ()) (from y :: Rep a ())

instance Eq a => Eq (Tree a) where
  (==) = eqDefault

class Encode a where
  encode :: a -> [Bool]

instance Encode Int where
  encode n = [n `testBit` b | b <- [0 .. finiteBitSize n - 1]]

instance Encode () where
  encode _ = []

class GEncode a where
  gencode :: a -> [Bool]

instance (GEncode (f p), GEncode (g p)) => GEncode ((f :*: g) p) where
  gencode (x :*: y) = gencode x ++ gencode y

instance (GEncode (f p), GEncode (g p)) => GEncode ((f :+: g) p) where
  gencode (L1 x) = False : gencode x
  gencode (R1 y) = True : gencode y

instance GEncode (U1 p) where
  gencode _ = []

instance Encode c => GEncode (K1 i c p) where
  gencode (K1 a) = encode a

instance GEncode (f p) => GEncode (M1 i c f p) where
  gencode (M1 a) = gencode a

encodeDefault :: forall a. (Generic a, GEncode (Rep a ())) => a -> [Bool]
encodeDefault x = gencode (from x :: Rep a ())

instance Encode a => Encode (Tree a) where
  encode = encodeDefault

data Four = One | Two | Three | Four
  deriving (Generic)
-- Either (Either () ()) (Either () ())

instance Encode Four where
  encode = encodeDefault
