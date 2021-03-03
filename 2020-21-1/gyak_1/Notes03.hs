
-- Functor instance-ok folyt.
data    Pair a b    = Pair a b

instance Functor (Pair a) where  -- utolsó paraméter fölött fmap
  fmap f (Pair a b) = Pair a (f b)

data Either' a b = Left' a | Right' b

instance Functor (Either' a) where
  fmap f (Left' a)  = Left' a
  fmap f (Right' b) = Right' (f b)

data D a b c     = D1 a b | D2 c a c

instance Functor (D a b) where
  fmap f (D1 a b)    = D1 a b
  fmap f (D2 c a c') = D2 (f c) a (f c')

-- (konstans funktor)
newtype Const a b = Const a

instance Functor (Const a) where
  fmap f (Const a) = Const a

newtype Fun a b = Fun (a -> b)   -- instance Functor ((->) a)

instance Functor (Fun a) where
  fmap f (Fun g) = Fun (f . g)


-- mai feladat:
data D' a = D1' a (Maybe a) | D2' [a] deriving (Eq, Show)

instance Functor D' where
  fmap f (D1' a ma) = D1' (f a) (fmap f ma)  -- Functor Maybe
  fmap f (D2' as)   = D2' (fmap f as)        -- Functor []  (fmap = map)

  -- fmap :: (a -> b) -> D' a -> D' b
  -- fmap f (D1' a Nothing)   = D1' (f a) Nothing
  -- fmap f (D1' a (Just a')) = D1' (f a) (Just (f a'))
  -- fmap f (D2' [])     = D2' []
  -- fmap f (D2' (a:as)) = D2' (f a : fmap f as)


-- Monad
--------------------------------------------------------------------------------

-- instance Eq a => Eq [a]

-- constraint
eqList :: (Eq a) => [a] -> [a] -> Bool
eqList []     []     = True
eqList (x:xs) (y:ys) = (x == y) && eqList xs ys
eqList _      _      = False

-- tipp: ha definíciónak nem tudjuk a típusát, akkor lehetséges megoldás, hogy nem írunk típust,
-- ghci-ben: :t definíció
--            megmondja a kikövetkeztetett típust

{-
class Semigroup a => Monoid a where
  mempty :: a

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
   (x, y) <> (x', y') = (x <> x', y <> y')

instance (Monoid a, Monoid b) => Monoid (a, b) where
  -- mempty :: (a, b)
  mempty = (mempty, mempty)
  -- (mempty, mempty) <> (x, y) = (mempty <> x, mempty <> y) = (x, y)   OK (bal identitás)
  --                    -- || --                                        OK (jobb identitás)

-}


-- class Functor m => Monad m where ... (2 metódus)
-- Feladat: 2 függvényt írni (motivációs)


-- 1. Írd meg a következő függvényt. A függvény úgy működik,
--    mint a lista "filter", viszont ha a kapott (a -> Maybe Bool)
--    függvény valamely elemre Nothing-ot ad, akkor Nothing legyen
--    a végeredmény, egyébként Just <szűrt lista>
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f []     = Just []
filterMaybe f (a:as) = case f a of        -- köztes eredményre illeszteni: case kifejezéssel
  Nothing -> Nothing
  Just b  -> case filterMaybe f as of
    Nothing  -> Nothing
    Just as' -> if b then Just (a:as') else Just as'

-- hasonlóképpen listára:
-- case xs of
--   []    -> _
--   x:xs' -> _

-- 2. Alkalmazz egy (a -> Maybe b) függvény egy Tree minden
-- levelére, ha bármelyik alkalmazás Nothing-ot ad,
-- legyen az eredmény Nothing!
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree f (Leaf a)   =
  case f a of
    Nothing -> Nothing
    Just b  -> Just (Leaf b)
  -- Maybe fmap-el szebben
  -- fmap Leaf (f a)
mapMaybeTree f (Node l r) = case mapMaybeTree f l of
  Nothing -> Nothing
  Just l' -> case mapMaybeTree f r of
    Nothing -> Nothing
    Just r' -> Just (Node l' r')

-- Maybe típusnál: ne kelljen kézzel Nothing-ot propagálni
--                 legyen inkább "exception" a Nothing, és ne hibakód

-- C#, (Erik Meijer (LINQ)), Javascript async kód

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing  _ = Nothing
bind (Just a) f = f a


mapMaybeTree' :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree' f (Leaf a) =
  bind (f a) $ \b ->
  Just (Leaf b)
  -- fmap Leaf (f a)

mapMaybeTree' f (Node l r) =
  -- egymásba ágyazott lambdák
  -- bind (mapMaybeTree' f l) (\l' -> bind (mapMaybeTree' f r) (\r' -> Just (Node l' r')))

  bind (mapMaybeTree' f l) $ \l' ->       -- bind ~ imperatív értékadás
  bind (mapMaybeTree' f r) $ \r' ->
  Just (Node l' r')

  -- imperatív pszeudokód (Nothing kivétel dobás)
  -- var l' = mapMaybeTree' f l;
  -- var r' = mapMaybeTree' f l;
  -- return (Node l' r');

-- instance Monad Maybe      -- hatás: Nothing mint kivétel
-- instance Monad (State s)  -- hatás: írható (mutábilis) referencia s típussal


-- Functor bónusz feladatok
--------------------------------------------------------------------------------

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = undefined

apply :: Functor f => f (a -> b) -> a -> f b
apply = undefined

first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first = undefined

second :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
second = undefined

-- bónusz bónusz: mire használható ez a függvény? Tipp: a megoldáshoz
-- rekurzió szükséges.
löb :: Functor f => f (f a -> a) -> f a
löb = undefined
