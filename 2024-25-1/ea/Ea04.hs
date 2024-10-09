-- Hilbert-Brouwer controversy 20-30-as evek
-- 1962 Errett Bishop: Constructive Analysis

-- f injektiv klasszikus:  ¬(∃y,z.y≠z ∧ f y = f z)
-- f injektiv konstruktiv: ∀y,z. f y = f z → y = z

-- Eq, Show, Ord :: * -> Constraint

-- Bool, Int, Float, String :: *
-- [], Maybe, (,) Int, Either Bool, (->) Bool :: * -> *

-- t :: ? :: *

-- t :: Maybe   -- ERROR
-- t = undefined

-- Functor :: (* -> *) -> Constraint

-- tarolo

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just a) = Just (f a)

data Pair c a = Pair c a

instance Functor (Pair c) where
  fmap f (Pair c a) = Pair c (f a)

data HomPair a = HomPair a a

instance Functor HomPair where
  fmap f (HomPair a a') = HomPair (f a) (f a')

-- instance Functor (Either c) where
--   fmap f (Left c)  = Left c
--   fmap f (Right a) = Right (f a)

-- (Bool ->) :: * -> *

-- HomPair a ≅ Bool -> a
mapFun :: (a -> b) -> (Bool -> a) -> (Bool -> b)
-- mapFun f h = \x -> f (h x)
-- f :: a -> b
-- h :: Bool -> a
-- x :: Bool
-- h x :: a
-- f (h x) :: b
-- mapFun f h = f . h
mapFun = (.)

data Id a = Id a
instance Functor Id where
  fmap f (Id a) = Id (f a)

data Const c a = Const c
instance Functor (Const c) where
  fmap f (Const c) = Const c

-- (Bool,) :: * -> *  funktor
-- (,Bool) :: * -> *  funktor
-- Either Bool :: * -> *  funktor
-- Either _ Bool :: * -> *  funktor
-- (Bool->) :: * -> *  funktor
-- (->Bool) :: * -> * nem funktor

{-
    f                  f       
a -----> b          a ---> b  
 \     /            ^     ^
 h\   /?            h\   /?  
   v v                \ /    
  Bool               Bool    
-}

-- ((Bool->_)->Bool)->_ funktor

-- kovarians funktor:
-- fmap :: Functor f => (a -> b) -> f a -> f b

-- kontravarians funktor:
class Contra f where
  contramap :: (a -> b) -> f b -> f a

-- (->Bool) nem funktor, viszont kontravarians funktor

data Fun a = Fun (a -> Bool)

instance Contra Fun where
  contramap f (Fun h) = Fun (h . f)
-- f :: a -> b
-- h :: b -> Bool
-- ? :: a -> Bool
