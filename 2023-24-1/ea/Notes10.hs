-- ez az ora 26 perccel rovidebb

-- Traversable
-- map
-- traverse :: (a -> Maybe b) -> [a] -> Maybe [b]   -- Maybe is Applicative
-- pred :: Int -> Maybe Int
-- sequenceA

{-
label :: (Traversable t) => t a -> t (a, Int)
label t = evalState (traverse f t) 0 where
  f a = do
    n <- get
    put (n + 1)
    pure (a, n)
-}

-- Alternative
-- Maybe
-- List

-- Parser

-- ST monad

-- sorfolytonos tree labelling
