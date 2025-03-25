import Control.Monad.Reader

-- (* -> *) ⊃ Functor ⊃ Applicative ⊃ Monad
data Maybe' a = Nothing' | Just' a Int

-- ellenpelda nem-funktorra (olyan fmap definicio, ami nem tartja meg
-- a funktor torvenyeket: megszamoljuk, hogy hanyszor alkalmaztuk az
-- fmap-et)

instance Functor Maybe' where
  fmap f Nothing' = Nothing'
  fmap f (Just' a i) = Just' (f a) (1+i)

-- fmap id (Just' a i) = Just' a (1+i) ≠ Just' a i

-- fmap id = id
-- fmap (f.g) = fmap f . fmap g

-- applikativ: f :: * -> *, Functor f
functorProd :: Functor f => f (a,b) -> (f a,f b)        -- f(a,b) =? (f a,f b),    f() =? ()
functorProd w = (fmap fst w,fmap snd w)
-- minden functor op-lax szorzatmegorzo

-- applikativ: (f a,f b) -> f(a,b) = lax szorzatmegorzo funktor

-- fmap0 :: a -> f a
-- fmap1 :: (a -> b) -> f a -> f b
-- fmap2 :: (a -> b -> c) -> f a -> f b -> f c
-- fmap2 :: ((a,b) -> c) -> (f a,f b) -> f c
-- fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- ...

class Functor f => Applicative' f where
  pure' :: a -> f a
  app'  :: f (a -> b) -> f a -> f b -- (<*>)

fmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2 g x y = pure g <*> x <*> y
-- g :: a -> b -> c
-- pure g :: f (a -> b -> c)
-- (pure g <*>) :: f a -> f (b -> c)
-- pure g <*> x :: f (b -> c)
-- (pure g <*> x <*>) :: f b -> f c
-- pure g <*> x <*> y :: f c

fmap3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 g x y z = pure g <*> x <*> y <*> z

applicProd :: Applicative f => (f a,f b) -> f(a,b)
applicProd (x,y) = fmap2 (,) x y

-- Monad: Philip Wadler vezette be a Haskellbe 90-es evek elejen
-- Applicative: Conor McBride 2000-es evekben

-- Monad vs. applikativ

{-
instance Functor ((,) c) where
  fmap :: (a -> b) -> (c,a) -> (c,b)
  fmap f (c,a) = (c,f a)

instance Applicative ((,) c) where
  pure :: a -> (c,a)
  pure a = (?,a)
  (<*>) :: (c,a -> b) -> (c,a) -> (c,b)
  (c,f) <*> (c',a) = (c vagy c'???,f a)

instance Monoid c => Applicative ((,) c) where
  pure a = (mempty,a)
  (c,f) <*> (c',a) = (c `mappend` c',f a)
-}

-- mely beagyazas 
data Exp = Lit Int | Exp :+: Exp
-- (1 + (2 + 3)) ≠ (1 + 2) + 3
e1 :: Exp
e1 = Lit 1 :+: (Lit 2 :+: Lit 3)

e1' :: Int
e1' = 1 + (2 + 3) -- sekely beagyazas
e1'' = (1 + 2) + 3 -- sekely beagyazas

-- a kifejezesnyelvem standard (metacirkularis) szemantikaja
eval :: Exp -> Int
eval (Lit n) = n
eval (e1 :+: e2) = eval e1 + eval e2

-- eval . mely beagyazas = sekely beagyazas

data Tm = Const Int | Tm :*: Tm | Tm :/: Tm

safeDiv :: Int -> Int -> Maybe Int
safeDiv i j | j == 0 = Nothing
            | otherwise = Just $ i `div` j

sem :: Tm -> Maybe Int
sem (Const i) = Just i
sem (e1 :*: e2) = case (sem e1,sem e2) of
  (Just i1,Just i2) -> Just ((*) i1 i2)
  _                 -> Nothing
sem (e1 :/: e2) = case (sem e1,sem e2) of
  (Just i1,Just i2) -> safeDiv i1 i2
  _                 -> Nothing

sem1 :: Tm -> Maybe Int
sem1 (Const i) = return i
sem1 (e1 :*: e2) = do
  i1 <- sem1 e1
  i2 <- sem1 e2
  return $ i1 * i2
sem1 (e1 :/: e2) = do
  i1 <- sem1 e1
  i2 <- sem1 e2
  safeDiv i1 i2

join :: Monad m => m (m a) -> m a
join w = w >>= id
-- w :: m (m a)
-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>=) :: m (m a) -> (m a -> m a) -> m a

sem2 :: Tm -> Maybe Int
sem2 (Const i) = pure i
sem2 (e1 :*: e2) = (*) <$> sem2 e1 <*> sem2 e2
sem2 (e1 :/: e2) = join $ safeDiv <$> sem2 e1 <*> sem2 e2
---- Maybe (Int -> Int -> Maybe Int)
-- Maybe (Maybe Int)

-- unsafe, de olvashato valtozat
sem3 :: Tm -> Int
sem3 (Const i) = i
sem3 (e1 :*: e2) = (*) (sem3 e1) (sem3 e2)
sem3 (e1 :/: e2) = div (sem3 e1) (sem3 e2)

-- 5.05

guard :: Bool -> [] ()
guard True = [()]
guard False = []

prods :: [Int] -> [Int] -> [Int]
-- prods xs ys = [ x*y | x <- xs, y <- ys, x + y < 4]
prods xs ys = do
  x <- xs
  y <- ys
  guard (x + y < 4)
  return (x * y)

guardMaybe :: Bool -> Maybe ()
guardMaybe True = Just ()
guardMaybe False = Nothing

prods' :: Maybe Int -> Maybe Int -> Maybe Int
{-
prods' mx my = do
  x <- mx
  y <- my
  guardMaybe (x + y < 4)
  return (x * y)
-}
prods' mx my =
  mx >>= \x ->
  my >>= \y -> 
  guardMaybe (x + y < 4) >>= \_ ->
  Just (x * y)

-- Nothing >>= _ = Nothing

-- >>= :: Maybe Int -> (Int -> Maybe Int) -> Maybe Int

data Prog = Var String | Cnst Int | Add Prog Prog

run :: Prog -> (String -> Int) -> Int
run (Var x)  env = env x
run (Cnst i) env = i
run (Add p1 p2) env = run p1 env + run p2 env

run1 :: Prog -> Reader (String -> Int) Int
-- run1 (Var x) = ask <*> pure x
-- run1 (Var x) = do
--   vars <- ask
--   return $ vars x
run1 (Var x) = ask >>= (return . ($ x))
run1 (Cnst i) = return i
run1 (Add p1 p2) = (+) <$> run1 p1 <*> run1 p2

-- Reader a b = (a -> b)
-- ask :: Reader a a

-- jovo oran:

-- gcd debug outputtal, writer, tell
-- relabel, state, get, put, debug outputtal

-- 3 + 2 rovidebb
