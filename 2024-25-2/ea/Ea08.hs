import Prelude hiding (gcd)
import Control.Monad.Writer
import Control.Monad.State

{-
szabad: aprilis 8, majus 6, majus 13

- staged programming, two-level type theory for generating Haskell
  - Kovács András ICFP cikk
- Embedded languages as Free Monads
- Free Applicative Functors
  - cikk
- Hughes: Why (lazy) functional programming matters?    Akos (majus 6)
  - cikk
- Simon Marlow book on parallelism and concurrency
  - konyv
- Datatypes a la carte
  - cikk
- Haskell hatekonysag
  - Graham Hutton cikkek
- Typing Haskell in Haskell
  - SPJ
- ST monad, encapsulating stateful computations
- lenses (van laarhoven vs profunctor lenses)
- websites in Haskell (Misi aprilis 8)
- FRP
- seemingly impossible functional programs, Eq ((Integer -> Bool) -> Bool)  ???? jovo heten
  - Martin Escardo
- parametricity
  - John Reynolds fabula
  - t :: forall a. a -> a,       unit tipus Church kodolasa pont ez
         forall a. a -> a -> a   ennek csak ket eleme van
    f :: forall a. [a] -> [a]    minden g :: a -> b-re map g . f = f . map g
  - Philip Wadler: Free theorems

  relacio-megorzes: minden definialhato fuggveny megorzi a relaciokat, 
  a :: *, P(x), x :: a predikatum, u :: a, P(u), akkor P(t u)

  a :: *, u :: a,  P(x):=(u=x),    P(u)=(u=u), P(t u)=(t u = u)

talan kesobb:
- logic in Haskell
- Church encodings of coinductive types
- (co)inductive types as (greatest)/least fixpoints of polynomial functors
- derivatives of type operators
- parametricity and free theorems
- laziness, dynamic programming, breadth-first labelling
- lazy IO, streaming (explicit, safe version of lazy IO)
- (co)recursion schemes
- debugging Haskell
- Haskell in industry
- template Haskell
- generics, deriving Eq, Functor, Hashable, serialisation, user interface
- continuation monad, classical logic in Haskell
- concurrency, STM
- parallelism
- algebraic effects, free monads
- type inference algorithms
~ cartesian closed category
-}

gcd :: Int -> Int -> Int
gcd x y | x == y = x
        | x >= y = gcd y (x-y)
        | x <= y = gcd x (y-x)

gcd' :: Int -> Int -> IO Int
gcd' x y | x == y = return x
         | x >= y = do
           putStr $ show x ++ ", " ++ show y ++ "\n"
           gcd' y (x-y)
         | x <= y = do
           putStr $ show x ++ ", " ++ show y ++ "\n"
           gcd' x (y-x)

gcd1 :: Int -> Int -> Writer String Int
gcd1 x y | x == y = return x
         | x >= y = do
           tell $ show x ++ ", " ++ show y ++ "\n"
           gcd1 y (x-y)
         | x <= y = do
           tell $ show x ++ ", " ++ show y ++ "\n"
           gcd1 x (y-x)

data Tree = Leaf Int | Node Tree Tree
  deriving (Show)

relabel :: Int -> Tree -> (Tree,Int)
relabel i (Leaf _) = (Leaf i,i+1)
relabel i (Node t1 t2) =
  let (t1',i1) = relabel i  t1 in
  let (t2',i2) = relabel i1 t2 in
  (Node t1' t2',i2)
{-
  Node
    (relabel (i+1) t1)
    (relabel (i+?) t2)
-}  

t1 = Node (Node (Leaf 0) (Leaf 0)) (Node (Leaf 0) (Leaf 0))

-- [], Maybe  guard
-- Reader     ask
-- Writer     tell
-- State      get, put

relabel' :: Tree -> WriterT String (State Int) Tree
relabel' (Leaf _) = do
  i <- get
  put (i+1)
  tell $ show i ++ "-et megnovolem 1-el\n"
  return (Leaf i)
relabel' (Node t1 t2) = do
  t1' <- relabel' t1
  t2' <- relabel' t2
  return (Node t1' t2')

-- get :: State s s
-- put :: s -> State s ()

-- runState (runWriterT $ relabel' t1) 0

-- 3 + 2 + 6 rovidebb
