import Control.Monad

data Exp = Lit Int | Plus Exp Exp | Mul Exp Exp | Div Exp Exp
  deriving (Show)

safeDiv :: Int -> Int -> Maybe Int
safeDiv a b | b == 0 = Nothing
            | otherwise = Just (a `div` b)

-- eval :: Exp -> Int

eval :: Exp -> Maybe Int
eval (Lit n) = pure n
eval (Plus e1 e2) = pure (+) <*> eval e1 <*> eval e2
eval (Mul e1 e2) = pure (*) <*> eval e1 <*> eval e2
eval (Div e1 e2) = join (pure safeDiv <*> eval e1 <*> eval e2)
{-
eval (Div  e1 e2) = do
  n1 <- eval e1
  n2 <- eval e2
  n1 `safeDiv` n2
-}
-- eval (Div e1 e2) = safeDiv <*> eval e1 <*> eval e2
{-
f x y = (f x) y
f <*> x <*> y = (f <*> x) <*> y

pure (+) :: Maybe (Int -> (Int -> Int))
eval e1  :: Maybe Int
(pure (+) <*> eval e1) :: Maybe (Int -> Int)
eval e2  :: Maybe Int
(pure (+) <*> eval e1) <*> eval e2 :: Maybe Int
(pure safeDiv <*> eval e1 <*> eval e2) :: Maybe (Maybe Int)

Int -> Int -> Maybe Int
-}
-- eval (Plus e1 e2) = (+) (eval e1) (eval e2)

{-
eval (Lit n) = return n
eval (Plus e1 e2) = do
  n1 <- eval e1
  n2 <- eval e2
  return (n1 + n2)
eval (Mul  e1 e2) = do
  n1 <- eval e1
  n2 <- eval e2
  return (n1 * n2)
eval (Div  e1 e2) = do
  n1 <- eval e1
  n2 <- eval e2
  n1 `safeDiv` n2
-}

-- monad typeclass, join

-- Applicative:
-- pure  :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b

-- Monad:
-- return :: a -> f a         := pure
-- (>>=)  :: f a -> (a -> f b) -> f b
-- join   :: f (f a) -> f a

join'' :: (Monad f) => f (f a) -> f a
join'' x = x >>= id
-- x :: f (f a)
-- (>>=)  :: f (f a) -> (f a -> f a) -> f a

bind :: (Monad f) => f a -> (a -> f b) -> f b
bind x h = join (fmap h x)
-- pure h <*> x :: f (f b)
-- fmap h :: f a -> f (f b)
-- pure h . <*> = fmap h

-- Functor f, fmap (f . g) = fmap f . fmap g
--            fmap id = id

data CMaybe a = CNothing | CJust Int a
  deriving (Show)

instance Functor CMaybe where
  fmap :: (a -> b) -> CMaybe a -> CMaybe b
  fmap f CNothing = CNothing
  fmap f (CJust i x) = CJust (i+1) (f x)

-- erre nem mukodnek a functor torvenyek, ilyet TILOS!!!!
{-
ghci> fmap (+3) (fmap (+1) (CJust 0 3))
CJust 2 7
ghci> fmap ((+3).(+1)) (CJust 0 3)
CJust 1 7
ghci> fmap id (CJust 0 3)
CJust 1 3
-}

-- Monoid: (a `mappend` b) `mappend` c = a `mappend` (b `mappend` c)
--         mempty `mappend` a = a, a `mappend` mempty = a

-- Monad torvenyek:
-- x >>= return = x                a `mappend` mempty = a
--   x      :: m a
--   return :: a -> m a
-- return a >>= k = k a
--   x :: a
--   return x :: m a
--   k :: a -> m b
--   return x >>= k :: m b
--   k x :: m b
-- (\ a -> return a >>= k) = k     mempty `mappend` a = a
-- (a >>= f) >>= g = a >>= (\x -> f x >>= g)
--   a :: m a
--   f :: a -> m b
--   a >>= f :: m b
--   g :: b -> m c
--   (\x -> f x) :: a -> m b
--   (\x -> f x >>= g) :: a -> m c
--   ? :: a -> m c

-- fail: Maybe
-- [] : nemdeterminisztikus
-- IO : barmi,   Int -> IO String

-- Functor => Applicative => Monad

-- ellenpelda: Functor, de nem Applicative
data Tag t a = MkTag a t

instance Functor (Tag t) where
  fmap f (MkTag x s) = MkTag (f x) s

instance Applicative (Tag t) where
  pure x = MkTag x undefined  -- ide nem tudok mit irni
  MkTag f t <*> MkTag x t' = MkTag (f x) t -- ad hoc, miert nem t'?

dupl :: Int -> [Int]
dupl x = [x+1,x-1]

szamit :: [Int]
szamit = do
  a <- return 2
  b <- dupl a         -- [3,1]
  c <- dupl (b+1)     -- [4,2], [5,3,3,1]
  return c

data Reader c a = MkReader { runReader :: c -> a }
  deriving Functor

instance Applicative (Reader c) where
  pure x = MkReader (\_->x)
  -- x :: a,  ? :: c -> a
  (<*>) :: Reader c (a -> b) -> Reader c a -> Reader c b
  MkReader f <*> MkReader x = MkReader (\c -> f c (x c))
  -- f :: c -> (a -> b)
  -- x :: c -> a
  -- ? :: c -> b
  -- (<*>) :: (c -> (a -> b)) -> (c -> a) -> (c -> b)

instance Monad (Reader c) where
  MkReader f >>= g = MkReader (\c -> runReader (g (f c)) c)
  -- MkReader f :: Read c a
  -- g          :: a -> Reader c b
  -- MkReader ? :: Reader c b
  -- f          :: c -> a
  -- c          :: c
  -- ?          :: b
  -- f c        :: a
  -- g (f c)    :: Reader c b
  -- runReader (g (f c)) :: c -> b

ask :: Reader c c
ask = MkReader id

addStuff :: Reader Int Int
addStuff = do
  a <- MkReader (*2)
  b <- MkReader (+10)
  c <- ask
  return (a+b + c)

data Expr = Liter Int | Var String | Add Expr Expr

{-
-- imperativ nyelv: van egy rho globalis valtozom
evaluate :: Expr -> Int
evaluate (Liter i) = i
evaluate (Add e1 e2) = evaluate e1 + evaluate e2
evaluate (Var s) = rho s

evaluate :: Expr -> (String -> Int) -> Int
evaluate (Liter i) rho = i
evaluate (Add e1 e2) rho = evaluate e1 rho + evaluate e2 rho
evaluate (Var s) rho = rho s
-}

evaluate :: Expr -> Reader (String -> Int) Int
evaluate (Liter i) = return i
-- evaluate (Add e1 e2) = pure (+) <*> evaluate e1 <*> evaluate e2
evaluate (Add e1 e2) = do
  i <- evaluate e1
  j <- evaluate e2
  return (i + j)
-- evaluate (Var s) = fmap ($ s) ask
evaluate (Var s) = do
  rho <- ask
  return (rho s)

-- type Reader c a = c -> a

-- join :: m (m a) -> m a
-- (*) :: Int -> Int -> Int
-- (*) :: Reader Int (Reader Int Int)
-- join (*)

-- jovo ora 11 perccel rovidebb (kumulalt)

-- - Writer: gcd with debugging, pure, Identity, IO, Writer, tell
-- - State: tree labelling, pure relabel, get, put, relabel with applicative (Notes07)

-- monad trafo

-- induktiv tipusok polinomokkal
-- parametricitas, type () = forall a . a -> a
