
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
  fmap f (Leaf1 a) = Leaf1 (f a)
  fmap f (Node1 l r) = Node1 (fmap f l) (fmap f r)

instance Functor Tree2 where
  fmap f (Node2 a ts) = Node2 (f a) (map (fmap f) ts)

instance Functor (Tree3 i) where
  fmap f (Leaf3 a) = Leaf3 (f a)
  fmap f (Node3 g) = Node3 (\i -> fmap f (g i))

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Functor (Either a) where
  fmap f (Left a) = Left a
  fmap f (Right b) = Right (f b)

instance Functor Id where
  fmap f (Id a) = Id (f a)

instance Functor (Const a) where
  fmap f (Const a) = Const a

instance Functor (Fun a) where
  fmap f (Fun g) = Fun (\a -> f (g a))


-- 2. feladat (bónusz). Írj Functor instance-t az alábbi típusokhoz

data Prod f g a = Prod (f a) (g a)
data Sum f g a = Inl (f a) | Inr (g a)
data Compose f g a = Compose (f (g a))
data List f a = Empty | Cons (f a) (List f a)

-- utána lehet nézni: continuation monad
newtype Cont r a = Cont ((a -> r) -> r)

instance (Functor f, Functor g) => Functor (Prod f g) where
  fmap f (Prod fa ga) = Prod (fmap f fa) (fmap f ga)

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap f (Inl fa) = Inl (fmap f fa)
  fmap f (Inr ga) = Inr (fmap f ga)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose (fmap (fmap f) fga)

instance Functor f => Functor (List f) where
  fmap f Empty        = Empty
  fmap f (Cons fa fs) = Cons (fmap f fa) (fmap f fs)

instance Functor (Cont r) where
  fmap f (Cont g) = Cont (\k -> g (k . f))


-- 3. feladat. Írd meg a következő instance-okat.

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (a, b) <> (c, d) = (a <> c, b <> d)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)

instance Semigroup b => Semigroup (a -> b) where
  f <> g = \a -> f a <> g a

instance Monoid b => Monoid (a -> b) where
  mempty = \a -> mempty


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
eval env (Literal n) = n
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Var x)     = case lookup x env of
  Just n  -> n
  Nothing -> error "variable not in environment"
