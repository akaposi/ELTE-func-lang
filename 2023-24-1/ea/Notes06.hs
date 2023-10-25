-- ez az ora 10 perccel rovidebb

-- mult ora: Functor, Monoid, Foldable, Monad(Maybe, do notation)

-- IO functor
main :: IO ()
main = do
  x <- getLine
  putStrLn "hello"

-- fmap :: (a -> b) -> IO a -> IO b
-- fmap :: (String -> String) -> IO String -> IO String

getLineBang :: IO String
getLineBang = fmap (++"!") getLine
getLineBang' = do
  s <- getLine
  return (s ++ "!")

-- Functor meg: Maybe, [], IO, (->) a, Either a, mind * -> *

-- nem functor: f a = a -> b, de Contravariant
--              (a -> b) -> f b -> f a
-- meg csak nem is kontravarians, hanem semmilyen functor:
--              f a = a -> a
--              g (a,b) = b -> a
--              f a = g (a,a)   difunctor

-- free theorem:   h :: forall a . a -> a
-- h @Int :: Int -> Int
-- h @String :: String -> String
-- ...
-- h @() :: () -> ()
-- h @() :: Empty -> Empty

-- h :: forall a . f a -> g a
-- TODO: jovo oran bebizonyitjuk, hogy h csak az identitas lehet
-- egysegesen mukodik, ugyanugy, uniform, parametrikusan polimorf
-- (+) :: forall a . Num a => a -> a -> a   --- ad hoc polimorfizmus

-- functor torvenyek: fmap id = id, fmap (f . g) = fmap f . fmap g

data CMaybe a = CNothing | CJust Int a

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust i x) = CJust (i+1) (f x)

-- fmap id (CJust i x) = CJust (i+1) x ≠ CJust i x = id (CJust i x)

-- (*3)      :: Int -> Int
-- fmap (*3) :: f Int -> f Int
-- (+100)    :: f Int
-- (+100)    :: Int -> Int
-- f a = Int -> a              ReaderInt a
-- fmap (*3) (+100) 1
-- fmap :: (a -> b) -> (Int -> a) -> (Int -> b)
-- fmap = (.)

--------------------------------------------------
-- monad
--------------------------------------------------

-- return :: Monad m => a -> m a
-- (>>=)  :: Monad m => m a -> (a -> m b) -> m b
-- bind, kotes
-- fmap'  ::            f a    (a -> b)   -> f b

fmap' :: Monad f => f a -> (a -> b) -> f b
fmap' x g = x >>= (return . g)
-- x :: f a
-- g :: a -> b
-- return . g :: a -> f b
{-
instance Monad Maybe where
  return = Just
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= _ = Nothing
  Just a  >>= f = f a
-}

-- monad instances:
-- List

{-
instance Monad [] where
  return :: a -> [a]
  return x = [x]
  (>>=) :: [a] -> (a -> [b]) -> [b]
  xs >>= f = concat (map f xs)
-}
dupl :: Int -> [Int]
dupl i = [i+1,i-1]

is :: [Int]
is = do
  i <- return 1  -- [1]
  j <- dupl i    -- [0,2]
  k <- dupl (j + i)    -- [-1,1,1,3]
  return k
is' :: [Int]
is' = return 1 >>= \i -> 
      dupl i >>= \j ->
      dupl (j + i) >>= \k ->
      return k
-- (\x -> \x -> x) = (\a -> \b -> b)

-- Reader, ask

type Reader a b = a -> b

addStuff :: Reader Int Int
-- addStuff :: Int -> Int
addStuff = do   
    a <- (*2)    -- a := 3*2
    b <- (+10)   -- b := 3+10
    return (a+b) -- a+b = 6+13 = 19
-- 22
-- 19
addStuff' = do   
    a <- (*2)
    b <- return (a+10)
    return (a+b)

join :: Monad m => m (m a) -> m a
join mma = mma >>= id
-- mma :: m (m a)
-- return :: a -> m a
-- bind   :: m (m a) -> (m a -> m a) -> m a

-- HF: gondolkozni: join (*) -- Reader

--      Reader r a = r -> a
newtype WriterString a = MkWriter (String,a)
  deriving (Functor,Applicative,Show)

instance Monad WriterString where
--  return :: a -> WriterString a
--  return x = MkWriter ("",x)
  MkWriter (log,a) >>= f = case f a of
    MkWriter (newlog,b) -> MkWriter (log ++ newlog,b)

tell :: String -> WriterString ()
tell s = MkWriter (s,())

{-
type Id a = a
  deriving (Monad)

gcd'' :: Int -> Int -> Id Int
gcd'' a b = case compare a b of
  LT -> do
    gcd'' (b - a) a
  GT ->  do
    gcd'' (a - b) b
  EQ -> do
    return a
-}
gcd' :: Int -> Int -> WriterString Int
gcd' a b = case compare a b of
  LT -> do
    tell $ "a: " ++ show a ++ ", b: " ++ show b ++ "\n"
    gcd' (b - a) a
  GT ->  do
    tell $ "a: " ++ show a ++ ", b: " ++ show b ++ "\n"
    gcd' (a - b) b
  EQ -> do
    tell "keszen vagyok!\n"
    return a

test :: IO ()
test = let (MkWriter (log,i)) = gcd' 12 3 in
  putStrLn log


-- a kovetkezo ora 18 perccel rovidebb
