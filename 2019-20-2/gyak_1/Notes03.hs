

-- Semigroup, Monoid, Functor
------------------------------------------------------------

-- bevezetés, egyszerű: Semigroup, Monoid
-- tárgy anyagának jelentős része:
--   3 class, összesen 4 daraba metódus
--     Functor, Applicative, Monad


import Prelude hiding (Functor(..), Semigroup(..), Monoid(..))

-- általánosan igaz: osztályok
--    algebrából jönnek (elnevezés + definíció)
--    kategóriaelméletből (szintén)

infixr 6 <>
class Semigroup a where
  (<>) :: a -> a -> a
  -- konvenció: (<>) asszociatív
  -- (a <> b) <> c = a <> (b <> c)

class Semigroup a => Monoid a where
  mempty :: a
  -- konvenció: mempty egységeleme a (<>) műveletnek
  -- tehát: mempty <> a = a
  --        a <> mempty = a


-- Feladat: írd meg a következő instance-okat!

instance Semigroup [a] where
  (<>) = (++)   -- összefűzés
                -- OK, mivel (++) asszociatív
                -- (xs ++ ys) ++ zs = xs ++ (ys ++ zs)

instance Monoid [a] where
  mempty = []   -- OK, mivel ([] ++ xs) = xs
                --           (xs ++ []) = xs

-- párokat <>-vel tudunk kombinálni, ha az elemeket tudjuk
-- kombinálni
instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  -- (<>) :: (a, b) -> (a, b) -> (a, b)
  (a, b) <> (a', b') = (a <> a', b <> b')

-- emlékeztető:
-- instance (Eq a, Eq b) => Eq (a, b)

-- ("foo", "bar") <> ("a", "b") == ("fooa", "barb")
-- ("foo", ("bar", "a")) <> ("a", ("b", "c")) == ("fooa",("barb","ac"))

-- (<>) művelet egységeleme párokra
instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)

  -- mempty <> (a, b) = (a, b)
  -- (mempty, mempty) <> (a, b) = (mempty <> a, mempty <> b)
  -- = (a, b) OK
  -- példa: ("foo", "bar") <> mempty == ("foo", "bar")

-- HÁZI feladat:
instance Semigroup b => Semigroup (a -> b) where
  (<>) = undefined

instance Monoid b => Monoid (a -> b) where
  mempty = undefined


-- Functor
------------------------------------------------------------

-- "map-elhető" típusok osztálya
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- listákra: map :: (a -> b) -> [a] -> [b]
--  pl: fmap (+1) [0..10] == [1..11]

instance Functor Maybe where
  -- fmap definíciója, feltéve f = Maybe
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f (Just a) = Just (f a)
  fmap f Nothing  = Nothing

-- NEM OK: csak 1 paraméteres típusokra írhatunk
-- Functor instance-t
--   instance Functor Int

instance Functor (Either c) where
  -- mi fmap, ha f = Either c
  -- fmap :: (a -> b) -> Either c a -> Either c b
  fmap f (Left c)  = Left c
  fmap f (Right a) = Right (f a)

-- röviden: Functor-al utolsó típusparaméter fölött tudunk
-- map-elni.

-- példa:

data Three a = Three a a a
data Foo1 a  = Foo1 Int a a a
data Foo2 a  = Foo2 Bool a Bool
data Foo3 a  = Foo3 a a a a a

instance Functor Three where
  -- fmap :: (a -> b) -> Three a -> Three b
  fmap f (Three x y z) = Three (f x) (f y) (f z)

-- Feladat: írj Functor instance-t az összes alábbi típushoz!
data    Tree1 a     = Leaf1 a | Node1 (Tree1 a) (Tree1 a)
data    Tree2 a     = Node2 a [Tree2 a]
data    Pair a b    = Pair a b
data    Either' a b = Left' a | Right' b
data    Tree3 i a   = Leaf3 a | Node3 (i -> Tree3 i a)
newtype Id a        = Id a
newtype Const a b   = Const a
newtype Fun a b     = Fun (a -> b)

instance Functor Tree1 where
  fmap = undefined

instance Functor Tree2 where
  fmap = undefined

instance Functor (Tree3 i) where
  fmap = undefined

instance Functor (Pair a) where
  fmap = undefined

instance Functor (Either' a) where
  fmap = undefined

instance Functor Id where
  fmap = undefined

instance Functor (Const a) where
  fmap = undefined

instance Functor (Fun a) where
  fmap = undefined
