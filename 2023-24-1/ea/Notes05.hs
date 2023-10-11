{-# LANGUAGE AllowAmbiguousTypes #-}

--------------------------------------------------
-- @a tipus-applikacio
--------------------------------------------------

class Finite (a :: *) where
  elemCount :: Int
  -- elems :: [a]

-- elemCount :: forall a . Finite a => a -> Int

instance Finite Bool where
  elemCount = 2

nnn :: Int
-- nnn = elemCount (undefined :: Bool)
-- nnn<Bool>
nnn = elemCount @Bool

--------------------------------------------------
-- functor instances
--------------------------------------------------

data Fun1 c b = Mk (c -> b) -- HF: instance Fun1 a

instance Functor (Fun1 c) where
  fmap :: (a -> b) -> Fun1 c a -> Fun1 c b
  fmap f (Mk g) = Mk (f . g)
{-
    f
 a ---> b
 ^      ^
 |g    /
 |   /?
 c /

-}

-- instance Functor ((->) c)
-- c -> a,   (Bool -> a) ≅ (a,a)

data Fun2 c a = Mk2 (a -> c) -- HF: nincs instance Fun2 a-ra

instance Functor (Fun2 c) where
  fmap :: (a -> b) -> Fun2 c a -> Fun2 c b
  fmap f (Mk2 g) = Mk2 undefined -- nem tudjuk megadni
{-
    f
 a ---> b
 |     /
 |g   / ?
 v   /
 c <

-}

class ContravariantFunctor (f :: * -> *) where
  contrafmap :: (a -> b) -> f b -> f a

instance ContravariantFunctor (Fun2 c) where
  contrafmap :: (a -> b) -> Fun2 c b -> Fun2 c a
  contrafmap f (Mk2 g) = Mk2 (g . f)
{-
    f
 a ---> b
 |     /
 |?   / g
 v   /
 c <

-}

-- fmap       :: (a -> b) -> (c -> a) -> (c -> b)
-- contrafmap :: (a -> b) -> (b -> c) -> (a -> c)

-- a   -> b    "=="  a ⊂ b
-- Nat -> Int        Nat ⊂ Int
-- (a,b) -> a        A × B ⊂ A
-- Line -> GeomObj   Line ⊂ GeomObj

-- a -> a'    b -> b'   a ⊂ a'       b ⊂ b' 
-- ------------------   -------------------
--  (a,b) -> (a',b')      (a,b) ⊂ (a',b')

-- a -> a'    b -> b'              a ⊂ a'       b ⊂ b' 
-- ------------------              -------------------------
-- Either a b -> Either a' b'      Either a b ⊂ Either a' b'

--   a' -> a    b -> b'  
-- ----------------------
-- (a -> b) -> (a' -> b')

--   Nat -> Int   Bool -> Maybe Bool
-- ------------------------------------
-- (Int -> Bool) -> (Nat -> Maybe Bool)
{-

   Int -----> Bool
    ^          |
    |          v
   Nat    Maybe Bool
-}

data UniPair a = Mk' (a,a)

instance Functor UniPair where
  fmap f (Mk' (x,y)) = Mk' (f x , f y)

data FunDelta a = Mk'' (a -> a)

-- nincs fmap:
-- fmap :: (a -> b) -> FunDelta a -> FunDelta b
-- fmap :: (a -> b) -> (a -> a) -> (b -> b)
{-
    a ----> a
    |
    v  ?
    b ----> b
-}

data Id a = MkId a

instance Functor Id where
  fmap :: (a -> b) -> Id a -> Id b
  fmap f (MkId x) = MkId (f x)

data Comp f g a = MkComp (f (g a))
  deriving Show

instance (Functor f, Functor g) => Functor (Comp f g) where
  fmap :: (a -> b) -> Comp f g a -> Comp f g b
  fmap h (MkComp x) = MkComp (fmap (fmap h) x)
  -- x :: f (g a)
  -- g-re az fmap h :: g a -> g b
  -- f-re az fmap (fmap h) :: f (g a) -> f (g b)
  -- ? :: f (g b)

-- [] (((->) Bool) a) = [Bool -> a]

xss :: Comp [] [] Int
xss = MkComp [[1,2],[3,4],[5,6]]

-- fmap (>2) xss

yss :: Comp [] ((->) Bool) Int
yss = MkComp [(\x->if x then 3 else 4), const 2]

instance Show a => Show (Bool -> a) where
  show f = "(True↦" ++ show (f True) ++ ",False↦" ++ show (f False) ++ ")"

-- fmap (>3) yss =
-- MkComp [(\x->if x then False else True), const False]

-- HF: instance (Finite a, Show b) => Show (a -> b)

data Const c a = MkConst c

instance Functor (Const c) where
  fmap f (MkConst c) = MkConst c

-- fmap id x = x
-- fmap (h . h') x = fmap h (fmap h' x)

--------------------------------------------------
-- Foldable
--------------------------------------------------

foldList :: (a -> c -> c) -> c -> [a] -> c
foldList cons nil [] = nil
foldList cons nil (a : as) = cons a (foldList cons nil as)

length', length'' :: [a] -> Int
length' []       = 0
length' (a : as) = 1 + length' as

length'' = foldList (const (1 +)) 0

-- pointfree, combinator style programming

class Monoid' a where
  mempty :: a
  (<>)   :: a -> a -> a
  -- (x <> y) <> z = x <> (y <> z)
  -- mempty <> x = x
  -- x <> mempty = x

instance Monoid' Int where
  mempty = 0
  (<>) = (+)

instance Monoid' [a] where
  mempty = []
  (<>) = (++)

{-
foldMap :: Monoid m => (a -> m) -> t a -> m
foldmap f (Node (Node (Leaf x0) (Leaf x1)) (Leaf x2))

     /\
    /  \
   /\   x2   |--> (f x0 <> f x1) <> f x2
  /  \
 x0  x1

  |
  x0
  |    x0:x1:x2:[]   |--> f x0 <> f x1 <> f x2 <> mempty
  x1
  |
  x2
  |
-}

-- Monad: interpreter for lang with div, in Maybe monad

data Exp = Const Int | Add Exp Exp | Mul Exp Exp

-- 3 + ((1 + 1) * 2)
{-
   +
  / \
 3   *
     /\
    +  2
   / \
   1  1
-}
e1 :: Exp
e1 = Add (Const 3) (Mul (Add (Const 1) (Const 1)) (Const 2))

-- evaluator, kiertekelo
eval :: Exp -> Int
eval (Const i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

data Exp' = Const' Int | Add' Exp' Exp' | Mul' Exp' Exp' | Div' Exp' Exp'

e2 :: Exp'
e2 = Const' 3 `Mul'` (Const' 4 `Div'` Const' 0)

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y = case y of
  0 -> Nothing
  _ -> Just (x `div` y)

eval' :: Exp' -> Maybe Int
eval' (Const' i) = Just i
eval' (Add' e1 e2) = case (eval' e1) of
  Nothing -> Nothing
  Just n1 -> case (eval' e2) of
    Nothing -> Nothing
    Just n2 -> Just (n1 + n2)
eval' (Mul' e1 e2) = case (eval' e1) of
  Nothing -> Nothing
  Just n1 -> case (eval' e2) of
    Nothing -> Nothing
    Just n2 -> Just (n1 * n2)
eval' (Div' e1 e2) = case (eval' e1) of
  Nothing -> Nothing
  Just n1 -> case (eval' e2) of
    Nothing -> Nothing
    Just n2 -> safeDiv n1 n2

-- Monad

eval'' :: Exp' -> Maybe Int
eval'' (Const' i) = Just i
eval'' (Add' e1 e2) = do
  n1 <- eval'' e1
  n2 <- eval'' e2
  return (n1 + n2)
eval'' (Mul' e1 e2) = do
  n1 <- eval'' e1
  n2 <- eval'' e2
  return (n1 * n2)
eval'' (Div' e1 e2) = do
  n1 <- eval'' e1
  n2 <- eval'' e2
  safeDiv n1 n2

-- kategoriaelmeletbol, monad = monoid az endofunktorok kategoriajaban
class Monad' (m :: * -> *) where
  return' :: a -> m a
  (>>==)   :: m a -> (a -> m b) -> m b

instance Monad' Maybe where
  return' = Just
  Nothing >>== f = Nothing
  Just a  >>== f = f a

eval''' :: Exp' -> Maybe Int
eval''' (Const' i) = Just i
eval''' (Add' e1 e2) = 
  eval''' e1 >>== \n1 -> 
  eval''' e2 >>== \n2 ->
  return' (n1 + n2)
eval''' (Mul' e1 e2) =
  eval''' e1 >>== \n1 -> 
  eval''' e2 >>== \n2 ->
  return' (n1 * n2)
eval''' (Div' e1 e2) =
  eval''' e1 >>== \n1 -> 
  eval''' e2 >>== \n2 ->
  safeDiv n1 n2

e3 :: Exp'
e3 = Const' 3 `Mul'` (Const' 4 `Div'` Const' 0)
