import Prelude hiding (sum)


-- foldr :: (a -> b -> b) -> b -> List a -> b
-- listara vonatkozo iterator
-- data List a = Nil | Cons a (List a)
-- lista-algebra = (b :: *), (n :: b), (c :: a -> b -> b)
-- List a is egy lista-algebra, b:=List a, n:=Nil, c:=Cons
-- foldr = ite(List a) :: lista-algebra -> List a -> b

data Tree a = Node (Tree a) (Tree a) | Leaf a
  deriving (Show, Functor)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr c n (Node t1 t2) = foldr c (foldr c n t1) t2
  -- foldr c n (Node t1 t2) = foldr c (foldr c n t2) t1
  --  foldr c n t1 :: b
  --  foldr c n t2 :: b
  foldr c n (Leaf a) = c a n

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Node t1 t2) = foldMap f t1 `mappend` foldMap f t2
  foldMap f (Leaf a) = f a

t1 :: Tree Int
t1 = Node (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) (Leaf 4)

t1' :: [Int]
t1' = foldr (:) [] t1

t1'' :: [Int]
t1'' = foldMap (:[]) t1

-- generic programming: average, and, or, all,  with Foldable

sum :: (Num a, Foldable t) => t a -> a
-- sum [] = 0
-- sum (x:xs) = x + sum xs
sum = foldr (+) 0

len :: (Foldable t, Num b) => t a -> b
len = foldr (\_ n -> 1 + n) 0

average :: (Fractional a, Foldable t) => t a -> a
average xs = sum xs / len xs

instance Semigroup Bool where
  (<>) = (&&)
instance Monoid Bool where
  mempty  = True

-- True `mappend` True `mappend` True = False

and' :: Foldable t => t Bool -> Bool
and' = foldMap id

-- Kerdes: hogy kell Int-rol Double-re konvertalni Haskellben?
-- Valasz: fromIntegral

