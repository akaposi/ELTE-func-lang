import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State

-- ez az ora 18 perccel rovidebb

-- debugging gcd with IO

-- type vs. newtype vs. data

type Szam = Int

f :: Szam -> Szam
f x = x+1

newtype Szam1 = Szam1 { runSzam1 :: Int }

f1 :: Szam1 -> Szam1
f1 x = Szam1 (runSzam1 x + 1)
{-
gcd' :: Int -> Int -> Identity Int
gcd' x y = case compare x y of
    LT -> do
      gcd' (y - x) x
    GT -> do
      gcd' (x - y) y
    EQ -> return x

gcd' :: Int -> Int -> IO Int
gcd' x y = do
  putStrLn $ show x ++ ", " ++ show y
  case compare x y of
    LT -> do
      gcd' (y - x) x
    GT -> do
      gcd' (x - y) y
    EQ -> return x
-}

-- newtype Identity a = Identity a
-- newtype Writer w a = Writer { runWriter :: (a,w) }

-- tell   :: Monoid w => w -> Writer w ()
-- tell   :: String -> Writer String ()
-- putStr :: String -> IO ()

gcd' :: Int -> Int -> Writer String Int
gcd' x y = do
  tell $ show x ++ ", " ++ show y ++ "\n"
  case compare x y of
    LT -> do
      gcd' (y - x) x
    GT -> do
      gcd' (x - y) y
    EQ -> return x

-- newtype State s a = State (s -> (a,s))

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

t1 :: Tree Bool
t1 = Node (Leaf True) (Node (Leaf False) (Leaf False))
{-
  /\
 T /\
  F  F
-}
t2 :: Tree Bool
t2 = Node (Node (Leaf False) (Leaf False)) (Leaf True)
{-
  /\
 /\
-}

relabel :: Tree a -> Int -> (Tree Int, Int)
relabel (Leaf _)   i = (Leaf i, i+1)
relabel (Node l r) i = case relabel l i of
  (l', i') -> case relabel r i' of
    (r',i'') -> (Node l' r', i'')

-- get :: State s s
--        s -> (s,s)
-- put :: s -> State s ()

relabel1 :: Tree a -> State Int (Tree Int)
relabel1 (Leaf _)   = do
  i <- get
  put (i+1)
  return (Leaf i)
relabel1 (Node l r) = do
  l' <- relabel1 l
  r' <- relabel1 r
  return $ Node l' r'

relabel2 :: Tree a ->
  StateT Int (Writer String) (Tree Int)
relabel2 (Leaf _)   = do
  i <- get
  put (i+1)
  tell $ "cimkeztunk egy levelet " ++ show i ++ "-vel\n"
  return (Leaf i)
relabel2 (Node l r) = do
  l' <- relabel2 l
  r' <- relabel2 r
  tell $ "kesz a node\n"
  return $ Node l' r'

fresh :: State Int Int
fresh = do
  i <- get
  put (i+1)
  return i

relabel3 :: Tree a -> State Int (Tree Int)
relabel3 (Leaf _)   = Leaf <$> fresh
relabel3 (Node l r) =
  Node <$> relabel3 l <*> relabel3 r
{- naiv implementacio pl. Java/ML/OCaml-ban:
relabel3 :: Tree a -> Tree Int
relabel3 (Leaf _)   = Leaf fresh
relabel3 (Node l r) =
  Node (relabel3 l) (relabel3 r)
-}

-- Applicative

-- Graham Hutton: Programming in Haskell
-- fmap0 :: a -> f a
-- fmap1 :: (a -> b) -> f a -> f b
-- fmap2 :: (a -> b -> c) -> f a -> f b -> f c
-- fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- ... 
{-
class Applicative (f :: * -> *) where
  pure :: a -> f a
  <*>  :: f (a -> b) -> f a -> f b
-}
fmap0 :: Applicative f => a -> f a
fmap0 = pure
fmap1 :: Applicative f => (a -> b) -> f a -> f b
fmap1 g x = pure g <*> x
-- g :: a -> b
-- pure g :: f (a -> b)
fmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2 g x y = pure g <*> x <*> y
-- g :: a -> b -> c
-- pure g       :: f (a -> (b -> c))
-- x            :: f a
-- pure g <*> x :: f (b -> c)
-- y            :: f b
-- (pure g <*> x) <*> y :: f c
-- HF: fmap3, fmap4

-- pure (+) <*> Just 1 <*> Just 2
-- pure (+) <*> Just 1 <*> Nothing

-- (++) <$> Just "johntra" <*> Just "volta"  
-- (++) "johntra" "volta"

-- pure (+) <*> [1,2] <*> [3,4]
-- pure (+) <*> [1,2] <*> []

-- prods xs ys = [ x*y | x <- xs, y <- ys ]
-- prods xs ys = pure (*) <*> xs <*> ys

-- IO
-- (++) <$> getLine <*> getLine
-- OCaml: (++) getLine getLine

-- kovetkezo ora 15 perccel rovidebb
