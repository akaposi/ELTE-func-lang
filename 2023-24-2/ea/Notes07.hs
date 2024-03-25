import Prelude hiding (gcd)
import Control.Monad.Writer hiding (guard)
import Control.Monad.State hiding (guard)

-- pure  :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b

-- applikativ a funktor altalanositasa:
fmap0 :: Applicative f => a                         -> f a
fmap1 :: Applicative f => (a -> b)           -> f a -> f b
fmap2 :: Applicative f => (a -> b -> c)      -> f a -> f b -> f c
fmap3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- ...

fmap0 = pure
fmap1 f a = pure f <*> a
fmap2 f a b = pure f <*> a <*> b
fmap3 f a b c = pure f <*> a <*> b <*> c

-- [ x*y | x <- xs, y <- ys ] =
prod :: [Int]->[Int]->[Int]
prod xs ys = pure (*) <*> xs <*> ys

-- ez olyan "extra" fuggveny, amit a [] monad tud (ask a Reader-nek)
guard :: Bool->[()]
guard True  = [()]
guard False = []

-- [ x*y | x <- xs, y <- ys, x + y < 3]
prod' :: [Int]->[Int]->[Int]
prod' xs ys = do
  x <- xs
  y <- ys
  guard (x + y < 8)
  return (x * y)
  
-- ugy tunik, hogy ezt nem tudjuk rekonstrualni applikativval

-- (OCa)ML: s = (++) getLine getLine

-- Haskell:

sss :: IO String
sss = (++) <$> getLine <*> getLine

-- Brouwer <-> Hilbert

-- f : A -> B injektiv: ∀x,y:A. x≠y → f(x)≠f(y)
--                      ∀x,y:A, f(x)=f(y) → x = y

-- Int helyett (Int -> a) -> a, continuation passing style
-- Cont b = forall a . (b -> a) -> a
-- b      helyett (b -> ⊥) -> ⊥
-- konstr         klasszikus

-- WriterString a = (a,String)
-- class Monad WriterString where
--   return a = (a,x"")
--   (a,s) >>= g = let (b,s') = g a in (b, s++s')
-- (a,s) :: (a,String)
-- g  :: a -> (b,String)
-- ?  :: (b,String)

{-
gcd :: Int -> Int -> Int
gcd x y = case compare x y of
  LT -> gcd (y-x) x
  EQ -> x
  GT -> gcd (x-y) y


gcd :: Int -> Int -> (Int,String)
gcd x y = case compare x y of
  LT -> let (z,s) = gcd (y-x) x in (z,s ++ show y ++ ">" ++ show x ++ "\n")
  EQ -> (x,"vege!\n")
  GT -> let (z,s) = gcd (x-y) y in (z,s ++ show x ++ ">" ++ show y ++ "\n")

gcd :: Int -> Int -> IO Int
gcd x y = case compare x y of
  LT -> do
    putStrLn $ show y ++ ">" ++ show x
    gcd (y-x) x
  EQ -> do
    putStrLn $ "vege!"
    return x
  GT -> do
    putStrLn $ show x ++ ">" ++ show y
    gcd (x-y) y
-}

-- - Writer: gcd with debugging, pure, Identity, IO, Writer, tell

gcd :: Int -> Int -> Writer String Int
gcd x y = case compare x y of
  LT -> do
    tell $ show y ++ ">" ++ show x ++ "\n"
    gcd (y-x) x
  EQ -> do
    tell "vege!"
    return x
  GT -> do
    tell $ show x ++ ">" ++ show y ++ "\n"
    gcd (x-y) y

-- debug: putStrLn $ snd $ runWriter $ gcd 20 130

data Tree = Leaf | Node Tree Int Tree
  deriving (Show)
{-
relabel :: Tree -> Int -> (Tree,Int)
relabel Leaf         n = (Leaf,n)
relabel (Node l _ r) n =
  let (l',n') = relabel l n in
  let (r',n'') = relabel r (n'+1) in
  (Node l' n' r',n'')

-- - State: tree labelling, pure relabel, get, put, relabel and debug with trafo, relabel with applicative

relabel :: Tree -> State Int Tree
relabel Leaf         = return Leaf
relabel (Node l _ r) = do
  l' <- relabel l
  r' <- relabel r
  i <- get
  put (i+1)
  return (Node l' i r')
-}

relabel :: Tree -> StateT Int (Writer String) Tree
relabel Leaf         = return Leaf
relabel (Node l _ r) = do
  l' <- relabel l
  r' <- relabel r
  i <- get
  put (i+1)
  tell (show i ++ "-t megnoveltem\n")
  return (Node l' i r')

-- runWriter (runStateT (relabel (Node (Node Leaf 0 Leaf) 0 Leaf)) 0)
-- putStrLn $ snd $ runWriter (runStateT (relabel (Node (Node Leaf 0 Leaf) 0 Leaf)) 0)

-- ez az ora 17 perccel rovidebb (kumulalt)

-- induktiv tipusok polinomokkal
-- parametricitas, type () = forall a . a -> a
