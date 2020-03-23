module Notes04 where
import Prelude hiding (Semigroup(..), Monoid(..))

infixr 6 <>
class Semigroup a where
  (<>) :: a -> a -> a
  -- Laws:
  --   (x <> y) <> z == x <> (y <> z)
 
class Semigroup a => Monoid a where
  mempty :: a
  -- Laws:
  --   mempty <> x == x
  --   x <> mempty == x

instance Semigroup [a] where
  (<>)  = (++)
 
instance Monoid [a] where
  mempty = []

data BinTree a = Leaf a
               | Node (BinTree a) (BinTree a)
               deriving (Eq, Show, Ord)

mconcat :: Monoid a => [a] -> a
mconcat [] = mempty
mconcat (x:xs) = x <> mconcat xs
-- ex. mconcat ["abc", "def", "ghi"] = "abcdefghi"

concatTree :: Semigroup a => BinTree a -> a
concatTree (Leaf x) = x
concatTree (Node l r) = concatTree l <> concatTree r
-- ex. concatTree (Node (Leaf "abc") (Leaf "def")) == "abcdef"

instance (Semigroup a, Semigroup b) =>
         Semigroup (a,b) where
  (x1,y1) <> (x2,y2) = (x1 <> x2, y1 <> y2)

instance (Monoid a, Monoid b) =>
         Monoid (a,b) where
  mempty = (mempty, mempty)

instance (Semigroup b) => Semigroup (a -> b) where
  (f <> g) x = f x <> g x

instance (Monoid b) => Monoid (a -> b) where
  mempty x = mempty
