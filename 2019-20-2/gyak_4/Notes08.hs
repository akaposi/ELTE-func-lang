module Notes08 where
import Prelude hiding (mapM, sequence)

class Functor m => Monad' m where
  return' :: a -> m a
  bind'   :: m a -> (a -> m b) -> m b

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe f x' = case x' of
  Just a -> f a
  _      -> Nothing

instance Monad' Maybe where
  return' = Just
  bind'   = flip bindMaybe

instance Monad' [] where
  return' x  = [x]
  bind' xs f = concat (map f xs)

newtype Identity a = Identity a
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)
instance Monad' Identity where
  return' = Identity
  bind' (Identity x) f = f x

-- class Functor m => Monad m where
--   return :: a -> m a
--   (>>=)  :: m a -> (a -> m b) -> m b

apM :: Monad m => m (a -> b) -> m a -> m b
apM f x = do
  f' <- f
  x' <- x
  return (f' x')

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f [] _ = return []
zipWithM f _ [] = return []
zipWithM f (x:xs) (y:ys) = do
  z  <- f x y
  zs <- zipWithM f xs ys
  return (z : zs)


-- join for [] : concat
join :: Monad m => m (m a) -> m a
-- join x = (x >>= id)

-- join x = do
--   y <- x -- y :: m a
--   z <- y -- z :: a
--   return z

join x = do
  y <- x -- y :: m a
  y

-- join3 for []  :  (concat . concat)
join3 :: Monad m => m (m (m a)) -> m a
-- join3 x = join (join x)
-- join3 x = join (fmap join x)
join3 x = do
  y <- x
  z <- y
  z

e0 :: [String]
e0 = join3 [[["a","b"],["c"],["d"],[]]]

-- When m is the Identity monad
--   ap3 is just function application
ap3 :: Monad m => m (a -> b -> c) -> m a -> m b -> m c
ap3 f x y = do
  f' <- f
  x' <- x
  y' <- y
  return (f' x' y')

e1 :: [Int]
e1 = ap3 [(+), (*)] [1, 2] [3, 4]

-- When m is the Identity monad
--   mapM is map
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f []     = return []
mapM f (x:xs) = do
  y <- f x
  ys <- mapM f xs
  return (y : ys)

forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM = flip mapM

e2 :: IO ()
e2 = do
  forM [0..10] $ \x -> do
    forM [0..10] $ \y -> do
      print (x+y)
  return ()

-- When m is the Identity monad
--   sequence is the identity (id :: [a] -> [a])
sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (x:xs) = do
  y <- x
  ys <- sequence xs
  return (y:ys)

-- When m is the Identity monad
--   kleisliComp is function composition
kleisliComp :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
kleisliComp f g a = do
  b <- f a
  g b

-- When m is the Identity monad
--   replicateM is replicate
replicateM :: Monad m => Int -> m a -> m [a]
replicateM n x | n <= 0 = return []
replicateM n x = do
  y <- x
  ys <- replicateM (n-1) x
  return (y:ys)

-- or: replicateM n x = sequence (replicate n x)
