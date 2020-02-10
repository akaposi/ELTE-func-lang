







-- {-# LANGUAGE DeriveTraversable #-}
-- {-# LANGUAGE DeriveFoldable #-}
-- {-# LANGUAGE DeriveFunctor #-}

-- import Data.Foldable
-- import Data.Traversable
-- import Data.Functor (void)
-- import Control.Applicative
-- import Control.Monad.Trans
-- import Control.Monad.Trans.State
-- import Data.Char

-- data Expr = Literal Int | Add Expr Expr | Mul Expr Expr | Var String
--   deriving Show

-- -- ez a típus egyszerű kifejezésfák típusa, ahol vannak számliterálok, szorzás,
-- -- összeadás és változónevek. Pl:

-- e1 = Add (Literal 10) (Add (Literal 20) (Var "x"))
-- -- ez megfelel annak, hogy "10 + (20 + x)"

-- e2 = Add (Mul (Var "y") (Var "x")) (Literal 100)
-- -- "y * x + 100"
-- -- "let z = 10 in x * z"

-- -- Írj kiértékelő függvényt. Paraméterként megkapunk egy '[(String, Int)]' listát,
-- -- ez minden változónévhez megad egy értéket.

-- -- példa:
-- -- eval [("x", 10)] e1 == 40
-- -- eval [("x", 2), ("y", 2)] e2 == 400
-- -- miért nem kell felbontani a listát? miért csak az expressiont kell felbontani?

-- eval :: [(String, Int)] -> Expr -> Int
-- eval env = go where
--   go (Add e1 e2) = go e1 + go e2
--   go (Mul e1 e2) = go e1 * go e2
--   go (Var x)     = case lookup x env of
--     Just n -> n
--     _      -> error "var not in scope"

-- -- eval :: [(String, Int)] -> Expr -> Int
-- -- eval env (Literal n) = n
-- -- eval env (Add e1 e2) = eval env e1 + eval env e2
-- -- eval env (Mul e1 e2) = eval env e1 * eval env e2
-- -- eval env (Var x)     = case lookup x env of
-- --   Just n  -> n
-- --   Nothing -> error "variable not in environment"

-- -- lookup' :: Eq a => a -> [(a, b)] -> Maybe b
-- -- lookup' a [] = Nothing
-- -- lookup' a ((a', b):abs)
-- --   | a == a'   = Just b
-- --   | otherwise = lookup' a abs

-- type Graph = [(Int, [Int])]

-- dfs :: Graph -> Int -> [Int]
-- dfs g = reverse . go []
--   where
--    -- két minta, az elsőn guard (feltétel)
--    -- ha a guard hamis, akkor a második sor értékelődik
--   go visited i | elem i visited = visited
--   go visited i = case lookup i g of
--     Just is -> foldl' go (i:visited) is
--     Nothing -> error "impossible"

--   -- go visited i =
--   --   if elem i visited
--   --     then visited
--   --     else case lookup i g of
--   --            Just is -> foldl' go (i:visited) is
--   --            Nothing -> error "impossible"


-- data Tree a = Leaf a | Node (Tree a) (Tree a)
--   deriving (Show, Functor, Foldable)

-- mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
-- mapMaybeTree f (Leaf a) = Leaf <$> f a
-- mapMaybeTree f (Node l r) = case mapMaybeTree f l of
--   Nothing -> Nothing
--   Just l  -> case mapMaybeTree f r of
--     Nothing -> Nothing
--     Just r  -> Just (Node l r)
--   -- mapMaybeTree f (Node l r) = Node <$> mapMaybeTree f l <*> mapMaybeTree f r

-- -- mapTree f (Leaf a) = Leaf (f a)
-- -- mapTree f (Node l r) = case mapTree f l of
-- --   l' -> case mapTree f r of
-- --     r' -> Node l' r'

-- -- mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

-- instance Traversable Tree where
--   traverse f (Leaf a) = Leaf <$> f a
--   traverse f (Node l r) = Node <$> traverse f l <*> traverse f r
--     -- jobbról balra:  (\r' l' -> Node l' r') <$> traverse f r <*> traverse f l

--  -- (<*>)
--  -- mf <*> ma = do {f <- mf; a <- ma; pure (f a)}


-- data Either' a b = Left' a | Right' b | Both a b
--   deriving Show

-- instance Functor (Either' a) where
--   -- fmap f (Left' a)  = Left' a
--   -- fmap f (Right' b) = Right' (f b)
--   -- fmap f (Both a b) = Both a (f b)
--   fmap = fmapDefault

-- instance Foldable (Either' a) where
--   -- foldMap f (Left' a)  = mempty
--   -- foldMap f (Right' b) = f b
--   -- foldMap f (Both a b) = f b
--   foldMap = foldMapDefault

-- instance Traversable (Either' a) where
--   traverse f (Left' a) = pure (Left' a)
--   traverse f (Right' b) = Right' <$> f b
--   traverse f (Both a b) = Both a <$> f b

-- newtype Id a = Id {runId :: a} deriving (Functor)

-- instance Applicative Id where
--   pure = Id
--   Id f <*> Id a = Id (f a)


-- -- Tipp: gyors Functor, Foldable instance írása: default függvényekkel
-- fmapDefault' :: Traversable t => (a -> b) -> t a -> t b
-- fmapDefault' f ta = runId (traverse (\a -> Id (f a)) ta)

-- foldMapDefault' :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
-- foldMapDefault' f ta =
--   execState
--     (traverse (\a -> modify (\m -> m <> f a)) ta)
--     mempty


-- partition :: [Either' a b] -> ([a], [b], [(a, b)])
-- partition [] = ([], [], [])
-- partition (ab : abs) =
--   let (as, bs, abs') = partition abs
--   in case ab of
--        Left' a  -> (a:as, bs, abs')
--        Right' b -> (as, b:bs, abs')
--        Both a b -> (as, bs, (a, b):abs')


-- --------------------------------------------------------------------------------

-- type Parser a = StateT String Maybe a

-- runParser :: Parser a -> String -> Maybe (a, String)
-- runParser = runStateT

-- evalParser :: Parser a -> String -> Maybe a
-- evalParser p s = fmap fst $ runParser p s

-- eof :: Parser ()
-- eof = do
--   str <- get
--   case str of
--     "" -> pure ()
--     _  -> empty

-- satisfy :: (Char -> Bool) -> Parser Char
-- satisfy f = do
--   str <- get
--   case str of
--     c:cs | f c -> c <$ put cs
--     _          -> empty

-- char :: Char -> Parser Char
-- char c = satisfy (== c)

-- lowerAlpha :: Parser Char
-- lowerAlpha = satisfy isLower

-- natural :: Parser Int
-- natural = read <$> some (satisfy isDigit)

-- token :: Parser a -> Parser a
-- token pa = pa <* ws

-- string :: String -> Parser ()
-- string s = () <$ traverse char s

-- string' :: String -> Parser ()
-- string' s = token (string s)

-- ws :: Parser ()
-- ws = () <$ many (satisfy isSpace)
