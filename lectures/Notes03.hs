
--------------------------------------------------------------------------------

import Prelude hiding (Either(..), Functor(..), Semigroup(..), Monoid(..))

class Functor f where
  fmap :: (a -> b) -> f a -> f b

infixr 6 <>
class Semigroup a where
  (<>) :: a -> a -> a

class Semigroup a => Monoid a where
  mempty :: a

--------------------------------------------------------------------------------

-- 1. feladat. Írj Functor instance-t az összes alábbi típushoz:

data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a)
data Tree2 a = Node2 a [Tree2 a]
data Tree3 i a = Leaf3 a | Node3 (i -> Tree3 i a)
data Pair a b = Pair a b
data Either a b = Left a | Right b
newtype Id a = Id a
newtype Const a b = Const a
newtype Fun a b = Fun (a -> b)

instance Functor Tree1 where
  fmap = undefined

instance Functor Tree2 where
  fmap = undefined

instance Functor (Tree3 i) where
  fmap = undefined

instance Functor (Pair a) where
  fmap = undefined

instance Functor (Either a) where
  fmap = undefined

instance Functor Id where
  fmap = undefined

instance Functor (Const a) where
  fmap = undefined

instance Functor (Fun a) where
  fmap = undefined


-- 2. feladat (bónusz). Írj Functor instance-t az alábbi típusokhoz

data Prod f g a = Prod (f a) (g a)
data Sum f g a = Inl (f a) | Inr (g a)
data Compose f g a = Compose (f (g a))
data List f a = Empty | Cons (f a) (List f a)
newtype Cont r a = Cont ((a -> r) -> r)

instance (Functor f, Functor g) => Functor (Prod f g) where
  fmap = undefined

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap = undefined

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap = undefined

instance Functor f => Functor (List f) where
  fmap = undefined

instance Functor (Cont r) where
  fmap = undefined


-- 3. feladat. Írd meg a következő instance-okat.

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (<>) = undefined

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = undefined

instance Semigroup b => Semigroup (a -> b) where
  (<>) = undefined

instance Monoid b => Monoid (a -> b) where
  mempty = undefined


-- 4. feladat. Vegyük a következő típust:

data Expr = Literal Int | Add Expr Expr | Mul Expr Expr | Var String
  deriving Show

-- ez a típus egyszerű kifejezésfák típusa, ahol vannak számliterálok, szorzás,
-- összeadás és változónevek. Pl:

e1 = Add (Literal 10) (Add (Literal 20) (Var "x"))
-- ez megfelel annak, hogy "10 + (20 + x)"

e2 = Add (Mul (Var "y") (Var "x")) (Literal 100)
-- "y * x + 100"

-- Írj kiértékelő függvényt. Paraméterként megkapunk egy '[(String, Int)]' listát,
-- ez minden változónévhez megad egy értéket.

-- példa:
-- eval [("x", 10)] e1 == 40
-- eval [("x", 2), ("y", 2)] e2 == 400

eval :: [(String, Int)] -> Expr -> Int
eval = undefined
