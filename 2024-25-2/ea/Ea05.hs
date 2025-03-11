{-
Monad, IO                                                                               |
State, Except                                                                           |
-}

-- instance Functor ((->) c) where
--   fmap :: (a -> b) -> ((->) c a) -> ((->) c b)
--   fmap :: (a -> b) -> (c -> a) -> (c -> b)
--   fmap = (.)

-- instance Functor ((,) c)
--   fmap :: (a -> b) -> ((,) c a) -> ((,) c b)
--   fmap :: (a -> b) -> (c,a) -> (c,b)

-- instance Functor (c,)
--   fmap :: (a -> b) -> (a,c) -> (b,c)

-- Either, (,) mindket parametereben kovarians

data c :→: a = Fun (c -> a)

instance Functor ((:→:) c) where
  fmap :: (a -> b) -> c :→: a -> c :→: b
  fmap f (Fun g) = Fun (f . g)

data c :←: a = Nuf (a -> c)

-- Int -> Real
-- c -> Int
---------------
-- c -> Real

-- (->) az elso parametereben kontravarians, masodik kovarians

{-
instance Functor ((:←:) c) where
  fmap :: (a -> b) -> c :←: a -> c :←: b
  fmap f (Nuf g) = Nuf undefined
-}

-- f :: a -> b
-- g :: a -> c
-- ? :: b -> c
 
class Contra f where
  contramap :: (a -> b) -> f b -> f a

instance Contra ((:←:) c) where
  contramap :: (a -> b) -> c :←: b -> c :←: a
  contramap f (Nuf g) = Nuf (g . f)

-- f :: a -> b
-- g :: b -> c
-- ? :: a -> c

-- Int ----> Real
-- ----------------------------
-- (a -> Int) ----> (a -> Real)

-- Int ----> Real
-- ----------------------------
-- (Real -> a) ----> (Int -> a)

-- Semigroup

{-
instance Semigroup a => Semigroup (a,a) where

instance Semigroup a => Semigroup (c -> a) where
  (<>) :: (c -> a) -> (c -> a) -> (c -> a)
  f <> g = \c -> f c <> g c
-}

{-
(Nothing <> Just a) <> Just a' = Nothing <> Just a' = Nothing = Nothing <> Just (a <> a')= Nothing <> (Just a <> Just a')
(Just a <> Just a') <> Just a'' = Just ((a <> a') <> a'') =(Semigroup a) Just (a <> (a' <> a'')) = Just a <> (Just a' <> Just a'')
-}

-- Semigroup Ordering
{- 
LT <> _ = LT
GT <> _ = GT
EQ <> x = x

-- 

Ordering = Either () Bool, ahol EQ := Left (), ekkor a Semigroup (Either () Bool)-bol megkapjuk az Ordering Semigroup instance-at
-}

{-
-- Left = error
-- Right = vegeredmeny

instance Semigroup (Either a b) where
  Right a <> _ = Right a
  Left a  <> x = x

-- tipp: ez majd parsernel jo lesz
-}

-- Monoid, ++, []

-- induiktiv tipusokra van fold/catamorphism/eliminator/recursor/iterator
-- foldr :: c -> (a -> c -> c) -> [a] -> c
-- generikus programozas

{-
instance Foldable [] where
  foldMap :: Monoid m => (a -> m) -> [a] -> m
  foldMap f [] = mempty
  foldMap f (x:xs) = f x <> foldMap f xs

instance Foldable ((,) c) where
  foldMap :: Monoid m => (a -> m) -> (c,a) -> m
  foldMap f (c,a) = f a

instance Foldable (Either c) where
  foldMap :: Monoid m => (a -> m) -> Either c a -> m
  foldMap f (Left c) = mempty
  foldMap f (Right a) = f a
-}

data Tree a = Leaf a | Node (Tree a) a (Tree a)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr c n (Leaf a) = c a n
  foldr c n (Node t1 a t2) = foldr c (c a (foldr c n t1)) t2
  -- foldr c n t1 :: b
  -- foldr c n t2 :: b
  -- a :: a

{-
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf a) = f a
  foldMap f (Node t1 a t2) = foldMap f t1 <> f a <> foldMap f t2

  -- foldr from foldMap
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr c n t = foldrList c n (foldMap (:[]) t)
  
  -- foldMap from foldr
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f t = foldr (\a m -> f a <> m) mempty t
  -- f :: a -> m
  -- t :: t a
  -- foldr :: (a -> m -> m) -> m -> t a -> m

[a] = a feletti szabad monoid

-- szabad semigroup = Empty
-- szabad monoid = ()
-- szabad monoid, amiben benne van az a
-- (), a1, a2, a3, a1+a2, a1+a3, a1+a2+a3, a1+a1, ...
-}

-- functor <= applicative <= monad

-- kov. ora 15 perccel rovidebb
