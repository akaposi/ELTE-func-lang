{-# language DeriveFunctor #-}

-- import Prelude hiding (Foldable(..))

-- Foldable, lista + fa feladatok
--------------------------------------------------------------------------------

-- -- osztály fold-olható típusokra
-- -- emlékezzünk: lista foldr :: (a -> b -> b) -> b -> [a] -> b
-- class Foldable f where
--   foldr :: (a -> b -> b) -> b -> f a -> b


-- 1. Írd meg a következő instance-okat.
data Two a     = Two a a deriving (Show)
data Three a   = Three a a a deriving (Show, Functor)
data Id a      = Id a deriving (Show)
data Const a b = Const a deriving (Show)  -- konstans funktor

instance Foldable Two where
  foldr f z (Two x y) = f x (f y z)
  -- foldr f z [x, y] = f x (f y z)
  -- cél: f-el jobbra zárójelezve az összes
  --  belső "a" típusú értéket kombináljuk

sum :: Foldable t => t Int -> Int
sum = foldr (+) 0
-- sum (Tree 10 20) == 30

product :: Foldable t => t Int -> Int
product = foldr (*) 1

instance Foldable Three where
  foldr f z (Three a b c) = f a (f b (f c z))
     -- mindig: z legjobb oldalt a függvényalkalmazásban

instance Foldable (Const a) where
  foldr f z (Const a) = z
    -- b-értékek *üres* listája
    -- tehát úgy foldr-ezzük, mint az üres listát

instance Foldable Id where
  foldr f z (Id a) = f a z

-- instance Foldable [] where
--   foldr f z []     = z
--   foldr f z (a:as) = f a (foldr f z as)

-- instance Foldable Maybe where
--   foldr f z Nothing  = z
--   foldr f z (Just a) = f a z
  -- Maybe: legfeljebb 1 elemű lista


-- 2. Írd meg a következő függvényeket, amelyek tetszőleges Foldable
--    típuson működnek! Teszteld a megoldásokat a korábban megadott
--    instance-okkal!

-- (írjuk meg az alábbi függvényeket a foldr felhasználásával)

-- üres-e?
isEmpty :: Foldable f => f a -> Bool
isEmpty fa = foldr (\_ _ -> False) True fa
   -- foldr f z <üres>    = z       -- ha z = True, akkor az eredmény True
   -- foldr f z <nemüres> = f _ _   -- ha f _ _ = False, akkor az eredmény False

-- a-típusú elemek száma (f a)-ban.
size :: Foldable f => f a -> Int
size fa = foldr (\_ acc -> 1 + acc) 0 fa

toList :: Foldable f => f a -> [a]
toList fa = foldr (\a as -> a : as) [] fa
-- toList = foldr (:) []

elem :: (Foldable f, Eq a) => a -> f a -> Bool
elem a fa = foldr (\a' acc -> (a == a') || acc) False fa

-- (megjegyzés: *minden* lehetséges véges listán működő függvényt
--              foldr-el lehet definiálni)
-- (minden fv, ami véges listán működik, az tetszőleges Foldable-n működik)

-- Kombináljuk (<>) segítségével az összes a-t (f a)-ban.
-- Példa: mconcat' ["foo", "bar"] = "foobar"
mconcat' :: (Foldable f, Monoid a) => f a -> a
mconcat' fa = foldr (\a as -> a <> as) mempty fa
-- mconcat' = foldr (<>) mempty


-- 3. Írd meg a következő instance-okat! (nehéz)
------------------------------------------------------------

data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a) deriving (Show)

instance Foldable Tree2 where
  foldr f z (Leaf2 a)   = f a z
  foldr f z (Node2 l r) = foldr f (foldr f z r) l   -- OK

     -- emlékeztető: foldr f z -nél z mindig a legjobb oldali érték
     -- ha az eredmény   f a (f b (f c (f d ...... (f .. z)))))


data Tree1 a = Node1 a [Tree1 a] deriving (Show)

instance Functor Tree1 where
  fmap f (Node1 a ts) = Node1 (f a) (map (fmap f) ts)

instance Foldable Tree1 where
  foldr f z (Node1 a ts) = f a (foldr (\t b -> foldr f b t) z ts)

-- sum $ Node1 10 [Node1 20 [], Node1 30 []] == 60


-- Traversable
--------------------------------------------------------------------------------

-- Foldable:    foldr overload egy osztállyal
-- Traversable: mapM overload egy osztállyal

-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]

-- class (Functor t, Foldable t) => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

--   általánosítás: - [] helyett "t" típuskonstruktor
--                  - Monad constraint helyett Applicative

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- (Előadást meg lehet nézni (jegyzet))


-- példa:


-- IO monádban
-- traverse print [0..10]

instance Traversable Three  where
  traverse f (Three a b c) = Three <$> f a <*> f b <*> f c
  -- lásd előadás: Applicative-ból: N-paraméteres fmap jön
  -- 3-paraméteres fmap-et

-- Köv gyakorlat: Parser monád
