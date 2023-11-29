data Tree = Leaf | Node Tree Int Tree
  deriving (Show)

label :: Int -> Tree -> (Tree,Int)
label i Leaf = (Leaf,i)
label i (Node t1 _ t2) =
  let (t1',j) = label i t1 in
  let (t2',k) = label j t2 in
  (Node t1' k t2',k+1)

{-
 0--0
 |  |
 |  +--0
 |
 +--0

   2         3   
  / \       / \  
  0  3      1  2 
   \         \   
    1         0
-}
n = Node
l = Leaf
t :: Tree
t = n (n l 0 (n l 0 l)) 0 (n l 0 l)

-- fmap : (a -> b) -> t a -> t b
--


data Tr a = Nd a [Tr a] deriving Show

tr :: Tr Int
tr = Nd 0 [Nd 0 [Nd 0 []], Nd 0 []]
{-    0
     / \
    0   0
     \
      0
-}

levels :: Tr a -> [[a]]
-- levels (Nd a bs) = a : foldr (zipWith' (++)) [] (map levels bs)
levels (Nd a bs) = [a] : foldr (zipWith' (++)) [] (map levels bs)

zipWith' f xs [] = xs
zipWith' f [] xs = xs
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

levLabel :: Int -> Tr Int -> Tr Int
levLabel i (Nd _ ts) =
  Nd i (map (levLabel (i+1)) ts)



-- sorfolytonos binaris fa cimkezes

-- breadth-first labelling of a binary tree with one traversal

data F x = L' | N' Int x x
           deriving Show
data T   = L  | N  Int T T
           deriving Show

-- minden f :: * -> * elkodol egy adattipust
data Fix f = Mk (f (Fix f))
-- (F (Fix F)) = Either () (Int,Fix F,Fix F)
-- T ≅ Fix F

-- F T -> T   ≅   T
-- F egy funktor, es T az inicialis F-algebra = T = (μ F)
-- F x = Either () (Int,x,x)      μ F = binaris fa, node-oknal Int      data T = Mk1 | Mk2 Int T T
-- F x = Either () (Int,x)        μ F = Int-ek listaja                  data T = Mk1 | Mk2 Int T
-- F x = Either () x              μ F = Peano termeszetes szamok        data T = Mk1 | Mk2 T
-- F x = x                        μ F = ures                            data T = Mk T
-- F x = Either x x               μ F = ures                            data T = Mk1 T | Mk2 T
-- F x = Either Int (Either x x)  μ F = egy Int es egy binaris szam     data T = Mk1 Int | Mk2 T | Mk3 T
-- F x = Either () ()             μ F = Bool                            data T = Mk1 | Mk2

-- F x = Either () (a,x)
-- foldr :: (F x -> x) -> [a] -> x
-- foldr :: (Either () (a,x) -> x) -> [a] -> x         -- Either a b -> c  ≅  (a->c , b->c)
-- foldr :: (() -> x , (a,x) -> x) -> [a] -> x         -- curry
-- foldr :: (() -> x) -> ((a,x) -> x) -> [a] -> x      -- curry
-- foldr :: (() -> x) -> (a -> x -> x) -> [a] -> x     -- x^1 = 1   ()->x ≅ x
-- foldr :: x -> (a -> x -> x) -> [a] -> x

fold :: (F x -> x) -> T -> x
fold f L         = f L'
fold f (N i l r) = f (N' i (fold f l) (fold f r))

-- fold' :: x -> (Int -> x -> x -> x) -> T -> x

-- types for inherited and synthesized attributes
type I = [[Int]]
type S = (T, [[Int]]) -- the labelled tree and the remaining labels

f :: F (I->S) -> (I -> S)
f L'             xss          = (L,       xss)
f (N' _ isl isr) ((x:xs):xss) = (N x l r, xs:zss)
    where
      (l, yss) = isl xss
      (r, zss) = isr yss

labelT :: T -> T
labelT L         = L
labelT (N _ l r) = N 1 l' r'
    where
      (l', xss) = fold f l ([2..]:yss)
      (r', yss) = fold f r xss

ex :: T
ex = n0
    where
      n0 = N 0 n1 n2
      n1 = N 1 L n5
      n2 = N 2 n3 n4
      n3 = N 3 L L
      n4 = N 4 L n8
      n5 = N 5 L n6
      n6 = N 6 L n7
      n7 = N 7 L L
      n8 = N 8 L L
{-
      0
   /     \
  1       2
   \     / \
    5   3   4
     \       \
      6       8
       \
        7

N 1
  (N 2 L
    (N 4 L
      (N 7 L
        (N 9 L L))))
  (N 3
    (N 5 L L)
    (N 6 L
      (N 8 L L)))

-}








