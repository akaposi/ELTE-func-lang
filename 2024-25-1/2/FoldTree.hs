module FoldTree where

data FoldTree f a = Leaf a | Node a (f (FoldTree f a))

instance Foldable f => Foldable (FoldTree f) where
  foldMap :: Monoid m => (a -> m) -> FoldTree f a -> m
  foldMap f (Leaf a)    = f a
  foldMap f (Node a fa) = f a <> foldMap (foldMap f) fa
    

-- >>> foldl (+) 0 (Leaf 10) == 10
-- >>> foldr (+) 0 (Node 10 [(Leaf 0), (Leaf 7), (Leaf 2)]) == 19
-- >>> foldr (*) 6.0 (Node 0.5 Nothing) == 3.0
-- >>> foldr (:) "" (Node 'a' [(Leaf 'l'), (Node 'm' [(Leaf 'a'), (Leaf 'f'), (Leaf 'a')])]) == "almafa"
-- True
-- True
-- True
-- True
