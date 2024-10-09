class Contra f where
  contramap :: (a -> b) -> f b -> f a

data Fun f g a = Fun { unFun :: f a -> g a }

-- f x = tipus, g y = tipus, x ↦ (f x -> g x)
{-
fmap :: Functor f => (a -> b) -> f a -> f b

(h : a -> b)
fmap{f}       h : f a -> f b
contramap{g}  h : g b -> g a

fmap{Fun g f} h : (g a -> f a) -> (g b -> f b)

       contramap{g} h
    g a <----------- g b
     |                |
input|                ?
     v                v
    f a -----------> f b
       fmap{f} h
-}

instance (Functor f, Contra g) => Functor (Fun g f) where
  fmap :: (a -> b) -> Fun g f a -> Fun g f b
  fmap h (Fun input) = Fun (fmap h . input . contramap h)
--                         \____________________________/
--                              : g b -> f b
-- 
-- h                              :: a -> b
-- contramap h                    :: g b -> g a
-- input                          :: g a -> f a
-- fmap h                         :: f a -> f b
-- (fmap h . input . contramap h) :: g b -> f b
-- Fun (^^)                       :: Fun g f b

instance (Functor f, Contra g) => Contra (Fun f g) where
  contramap :: (a -> b) -> Fun f g b -> Fun f g a
  contramap h (Fun input) = Fun (contramap h . input . fmap h)

id' :: forall a . a -> a
id' x = x

type H f a b = f a b

-- egyE :: ((->) Int) Bool
egyE :: forall a . a (->) Bool Int
-- a :: (*->*->*)->*->*->*
-- a = Id, akkor Bool -> Int
-- a = Flip, akkor Int -> Bool
-- a f b c = f b b, akkor Int -> Int
egyE = undefined -- i /= 0

data ArrBool a = ArrBool (a -> Bool)

instance Contra ArrBool where
  contramap h (ArrBool i) = ArrBool (i . h)
  -- h :: a -> b
  -- i :: b -> Bool
  -- ? :: a -> Bool

--     (a -> b) -> ((a->Bool)->(Int,a))    -> ((b->Bool)->(Int,b))
fgv :: (a -> b) -> Fun ArrBool ((,) Int) a -> Fun ArrBool ((,) Int) b
fgv = fmap

data Weird a = Weird ((a->Bool)->(Int,a))
  deriving (Functor)

-- Haskell tipusok kategoriat alkotnak (Hask), es Functor f = egy kategoriaelmeleti funktor Hask-bol Hask-ba
-- (.), id, (.) asszociativ, id . f = f, f . id = f
-- id . f = \x->id(f x) = \x->f x = f
-- HF: f . (g . h) = (f . g) . h

-- Hajtogatás, Foldable, Semigroup, Monoid

-- class Foldable t where
--   foldr   :: (a -> b -> b) -> b -> t a -> b
-- iteList :: (a -> b -> b) -> b -> [a] -> b

data Maybe' a = Nothing' | Just' a

instance Foldable Maybe' where
  foldr :: (a -> b -> b) -> b -> Maybe' a -> b
  foldr f b Nothing'  = b
  foldr f b (Just' a) = f a b

data Tree a = Leaf a | Node (Tree a) (Tree a)

-- Semigroup
{-
f :: a -> b
g :: a -> b

(f <> g) a = f a <> g a
-- ? :: b

f :: Int -> String
g :: Int -> String
(f <> g) i = f i ++ g i

-- ((show :: Int -> String) <> show) 3

-}

instance Semigroup Int where
  i <> j = i + j
  -- (i<>j)<>k = i<>(j<>k)
instance Monoid Int where
  mempty = 0
  -- mempty <> a = a
  -- 0 + a = a
{-
mempty <> f =(def.)
\a -> mempty <> f a =(b-re igaz a monoid torveny)
\a -> f a  =(η)
f
-}
 

-- Monoid

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf a) = f a
  foldMap f (Node t1 t2) = foldMap f t1 <> foldMap f t2
   -- foldMap f t1 :: m
   -- foldMap f t2 :: m

ex = foldMap show (Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 2) (Leaf 3)))
{-
   /\
 /\  /\
1 4 2  3
-}

{-
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f b (Leaf a) = f a b
  -- foldr f b (Node t1 t2) = foldr f (foldr f b t1) t2
  foldr f b (Node t1 t2) = foldr f (foldr f b t2) t1
  -- foldr f b t1 :: b
  -- foldr f (foldr f b t1) t2 :: b
-}

-- foldmap :: Monoid m => (a -> m) -> [a]    -> m
-- foldMap :: (String -> String) -> [String] -> String

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing f = Nothing
bindMaybe (Just a) f = f a
