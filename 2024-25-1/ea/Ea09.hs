import Prelude hiding (gcd)

data Writer c a = Writer { deb :: c , dat :: a }

instance Functor (Writer c) where
  fmap :: (a -> b) -> Writer c a -> Writer c b
  fmap f (Writer c a) = Writer c (f a)

-- fmap id (Writer c a) = Writer c (id a) = Writer c a

instance Monoid c => Applicative (Writer c) where
  pure :: a -> Writer c a
  pure a = Writer mempty a
  (<*>) :: Writer c (a -> b) -> Writer c a -> Writer c b
  Writer c f <*> Writer c' a = Writer (c `mappend` c') (f a)

-- pure id <*> Writer c a =
-- Writer mempty id <*> Writer c a =
-- Writer (mempty `mappend` c) (id a) =(monoid law)
-- Writer c a

instance Monoid c => Monad (Writer c) where
  (>>=) :: Writer c a -> (a -> Writer c b) -> Writer c b
  -- Writer c a >>= f = let Writer c' b = f a in Writer (c `mappend` c') b
  Writer c a >>= f = Writer (c `mappend` deb (f a)) (dat (f a))


-- minden w,k,h-ra: w >>= (\x -> k x >>= h) = (w >>= k) >>= h:
---------------------------------------------------------------------
-- Writer c a >>= (\x -> k x >>= h)  =
-- Writer (c `mappend` deb (k a >>= h)) (dat (k a >>= h)) =
-- Writer (c `mappend` deb (Writer (deb (k a)) (dat (k a)) >>= h)) (dat (Writer (deb (k a)) (dat (k a)) >>= h)) =
-- Writer (c `mappend` deb (Writer (deb (k a) `mappend` deb (h (dat (k a)))) (dat (h (dat (k a)))))) (dat (Writer (deb (k a) `mappend` deb (h (dat (k a)))) (dat (h (dat (k a)))))) =
-- Writer (c `mappend` (deb (k a) `mappend` deb (h (dat (k a))))) (dat (h (dat (k a)))) =(assoc)
-- Writer ((c `mappend` deb (f a)) `mappend` deb (h (dat (k a)))) (dat (h (dat (k a)))) =
-- Writer (c `mappend` deb (f a)) (dat (k a)) >>= h =
-- (Writer c a >>= k) >>= h

{-
(++) associativ

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

minden xs,ys,zs-re  (xs++ys)++zs = xs++(ys++zs)

(1) xs = []
    ([]++ys)++zs =
    ys++zs =
    []++(ys++zs)

(2) xs = x:xs
    IH = (xs++ys)++zs = xs++(ys++zs)
    
    ((x:xs)++ys)++zs =
    (x : (xs++ys))++zs =
    x : ((xs++ys)+zs) =(IH)
    x : (xs ++ (ys++zs)) =
    (x:xs)++(ys++zs) =
-}

-- debug nelkul
gcd :: Int -> Int -> Int
gcd x y = case compare x y of
  LT -> gcd (y-x) x
  EQ -> x
  GT -> gcd (x-y) y

{- Java
gcd :: Int -> Int -> Int
gcd x y = case compare x y of
  LT -> putStrLn (show x ++ "<" ++ show y) ; return (gcd (y-x) x)
  EQ -> putStrLn (show x ++ "=" ++ show y) ; return x
  GT -> putStrLn (show x ++ ">" ++ show y) ; return (gcd (x-y) y)
-}

-- debuggal, Monad nelkul
gcd1 :: Int -> Int -> (String,Int)
gcd1 x y = case compare x y of
  LT -> let (d,n) = gcd1 (y-x) x in (show x ++ "<" ++ show y ++ "\n"++d,n)
  EQ -> (show x ++ "=" ++ show y ++ "\n",x)
  GT -> let (d,n) = gcd1 (x-y) y in (show x ++ ">" ++ show y ++ "\n"++d,n)

-- debuggal, brutal valtozat
-- baj1: IO az tul sok mindent tud
-- baj2: debug output az elveszik
gcd2 :: Int -> Int -> IO Int
gcd2 x y = case compare x y of
  LT -> do
    putStrLn $ show x ++ "<" ++ show y
    gcd2 (y-x) x
  EQ -> do
    putStrLn $ show x ++ "=" ++ show y
    return x
  GT -> do
    putStrLn $ show x ++ ">" ++ show y
    gcd2 (x-y) y

-- ask :: Reader c c := id
tell :: c -> Writer c ()
tell c = Writer c ()

gcd3 :: Int -> Int -> Writer String Int
gcd3 x y = case compare x y of
{-
  LT ->
    (tell $ show x ++ "<" ++ show y ++ "\n") >>=
      (\k -> gcd3 (y-x) x)
-}
  LT -> do
    k <- tell $ show x ++ "<" ++ show y ++ "\n"
    gcd3 (y-x) x
  EQ -> do
    tell $ show x ++ "=" ++ show y ++ "\n"
    return x
  GT -> do
    tell $ show x ++ ">" ++ show y ++ "\n"
    gcd3 (x-y) y

-- State

data State c a = State { runState :: c -> (c,a) } deriving (Functor)

instance Applicative (State c) where
  pure :: a -> State c a
  pure a = State (\c -> (c,a))
  (<*>) :: State c (a -> b) -> State c a -> State c b
  State cf <*> State ca = State
    (\c -> let (c' ,f) = cf c  in
           let (c'',a) = ca c' in
           (c'',f a))
  -- (c',a) = ca c :: (c,a)
  -- (c'',f) = cf c' :: (c,a->b)

instance Monad (State c) where
  State ca >>= f = State (\c ->
    let (c',a) = ca c in runState (f a) c')
  -- ca :: c -> (c,a)
  -- (c',a) = ca c :: (c,a)
  -- f :: a -> State c b

data Tree = Leaf Int | Node Tree Tree deriving (Show)

-- relabel binary tree, start with:
relabel :: Tree -> Int -> (Tree,Int)
relabel (Leaf _) i = (Leaf i, i+1)
relabel (Node t1 t2) i =
  let (t1',i' ) = relabel t1 i in
  let (t2',i'') = relabel t2 i' in
     (Node t1' t2',i'')

get :: State c c
get = State (\c -> (c,c))

put :: c -> State c ()
put c = State (\_ -> (c,()))

relabel1 :: Tree -> State Int Tree
relabel1 (Leaf _) = do
  i <- get
  put (i+1)
  return (Leaf i)
relabel1 (Node t1 t2) = pure Node <*> relabel1 t1 <*> relabel1 t2
{-
relabel1 (Node t1 t2) = do
  t1' <- relabel1 t1
  t2' <- relabel1 t2
  return (Node t1' t2')
-}
