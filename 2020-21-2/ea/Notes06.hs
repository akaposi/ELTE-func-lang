
-- Applicative példák, Applicative ((,) a) (annotáció)
-- Foldable, Traversable, (State/Maybe traverse példák)
--   mi nem Traversable?
-- Traversable --> Functor + Foldable
-- Lista, Alternative, guard, lista kifejezések (comprehension)
--   sublists, MonadComp

--------------------------------------------------------------------------------

{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, InstanceSigs #-}

import Data.Traversable
import Control.Applicative
import Control.Monad
import Data.Foldable  -- csomó generikus Foldable függvény

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))
  State f >>= g = State (\s -> case f s of (a, s') -> runState (g a) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma


-- Applicative folytatás
--------------------------------------------------------------------------------

-- Functor => Applicative =>   Monad
--  map       N-áris map      bind-olás

-- Applicative: mellékhatásos program, viszont nem interaktív

-- IO monád:      lehetséges: addig olvas be egy sort, amíg az nem üres, utána visszatér
-- IO applikatív: nem lehetséges
--                lehetséges: statikusan ismert mennyiségű IO művelet elvégzése

--    előny: statikusan elemezhető programok
--           (kétirányú parser/printer)
--           (statikusan elemezhető lekérdezés-nyelv (SQL résznyelve))
--               (library: Haxl, Facebook spamszűrő logikája ebben van írva)
--           (kódgenerálás: buffer-be írunk, hány byte-ot ír egy művelet buffer-be?)


-- N-áris map függvény:
-- monádikus map listákra: elég ha Applicative megszorítás
mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f []     = pure []
mapA f (a:as) = (:) <$> f a <*> mapA f as
   -- f a       :: f b
   -- mapA f as :: f [b]
   -- (:)       :: b -> [b] -> [b]

mapA' :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA' f []     = pure []
mapA' f (a:as) = liftA2 (:) (f a) (mapA' f as)
   -- f a       :: f b
   -- mapA f as :: f [b]
   -- (:)       :: b -> [b] -> [b]

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 g fa fb fc fd = g <$> fa <*> fb <*> fc <*> fd

-- általánosan:   f <$> arg_1 <*> ... <*> arg_n
--   liftM -- legacy függvény (olyan időből, amikor még nem volt Applicative a Monad superclass-ja)
--   ugyanaz, mint return = pure

-- filterA :: Monad f => (a -> f Bool) -> [a] -> f [a]
-- filterA g []     = pure []
-- filterA g (a:as) = do
--   b <- g a
--   if b then do
--     as <- filterA g as
--     pure (a : as)
--   else
--     filterA g as

filterA :: Applicative f => (a -> f Bool) -> [a] -> f [a]
filterA g []     = pure []
filterA g (a:as) =
  (\b as -> if b then a:as else as) <$> g a <*> filterA g as

-- Applicative példa (nem Monad)
--  (annotáció Applicative)

-- standard instance:
-- instance Monoid ann => Applicative ((,) ann) where
--   -- pure :: a -> (ann, a)
--   pure a = (mempty, a)         -- zérus annotáció
--   (ann, f) <*> (ann', a) = (ann <> ann', f a)

type Ann a = (String, a)

add :: Num a => Ann (a -> a -> a)
add = ("add", (+))

mul :: Num a => Ann (a -> a -> a)
mul = ("mul", (*))

ann :: Show a => a -> Ann a
ann a = (" " ++ show a ++ " ", a)

-- Int annotáció: hány byte-ot ír egy művelet egy bufferbe

-- (f <*> arg1 <*> arg2)  (run függvény: elvégzi az írást)
-- fst (f <*> arg1 <*> arg2) :: Int  (hány byte-ot ír a művelet)

-- Foldable, Traversable    (struktúrűk bejárását absztrahálják)
--------------------------------------------------------------------------------

-- foldr :: (a -> b -> b) -> b -> [a] -> b

-- class Foldable t where
--   foldr :: (a -> b -> b) -> b -> t a -> b

-- fold-olható típusok
--   (ekvivalens azzal: olyan típusok, amiket listává lehet alakítani)
--    toList :: t a -> [a]

toList :: Foldable t => t a -> [a]
toList ta = foldr (:) [] ta

foldr' :: (t a -> [a]) -> (a -> b -> b) -> b -> t a -> b
foldr' toList f b ta = foldr f b (toList ta)

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Foldable, Show, Traversable)

t1 :: Tree Int
t1 = Node (Node (Leaf 10) (Leaf 20)) (Leaf 30)

-- jó feladat:
--   definiáljuk a következő függvényeket: minimum, maximum, length, null, sum, product
--     csak foldr felhasználásával
--   definiáljuk a foldl függvényt csak foldr felhasználásával (trükkös feladat)


-- Traversable
--------------------------------------------------------------------------------

-- mellékhatásos map-elés

-- class (Functor t, Foldable t) => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- mapA     :: Applicative f => (a -> f b) -> [a] -> f [b]
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- pl: traverse (const getLine) t1   -- bejárja a fát IO Applicative-ban

--    bizonyos típus lehet egyszerre Traversable struktúra, és Applicative is
--      pl: Maybe

-- maga a Maybe bejárható
traverseMaybe :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
traverseMaybe g Nothing  = pure Nothing
traverseMaybe g (Just a) = Just <$> g a

-- "Maybe-ben" bejárható egy másik struktúra
traverseListInMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
traverseListInMaybe = traverse

-- címkézzük a fa leveleit balról jobbra bejárási sorrendben
label :: Tree a -> Tree (a, Int)
label ta = evalState (traverse go ta) 0 where
  go a = do
    n <- get
    put (n + 1)
    pure (a, n)

-- identitás funktor (semmi mellékhatás)
newtype Id a = Id {unId :: a} deriving Functor

instance Applicative Id where
  pure = Id
  Id f <*> Id a = Id (f a)

-- definiáljuk fmap-et traverse felhasználásával
fmap' :: Traversable t => (a -> b) -> t a -> t b
fmap' f ta = unId (traverse (\a -> Id (f a)) ta)

-- lens library-k : hasonló trükköt használunk ahhoz, általánosított map és traverse függvényeket
-- definiáljunk (keresés: Haskell lens)
--    lásd: jQuery


-- Lista monád
--------------------------------------------------------------------------------

-- lista monád a Maybe monád instance általánosítása

-- return :: a -> Maybe a
-- return a = Just a

-- instance Monad [] where
--   return :: a -> [a]
--   return a = [a]

--   (>>=) :: [a] -> (a -> [b]) -> [b]
--   (>>=) []       f = []
--   (>>=) [a]      f = f a
--   (>>=) [a1, a2] f = f a1 ++ f a2
--   (>>=) [a1, a2, a3] f = f a1 ++ f a2 ++ f a3

-- (>>=) ~ concatMap

-- concatMap :: (a -> [b]) -> [a]        -> [b]
-- (>>=)     :: [a]        -> (a -> [b]) -> [b]
-- (>>=) = flip concatMap

-- mi a mellékhatás?
--    --> "nem-determinizmus"
--    több lehetséges értekre lefut a progamunk

l1 :: [(Int, Int)]
l1 = do
  x <- [0..10]   -- nem-determinisztikus értékadás
  y <- [0..x]
  pure (x, y)

-- lista kifejezés: szintaktikus cukor lista monád kifejezésekre
l1' :: [(Int, Int)]
l1' = [(x, y) | x <- [0..10], y <- [0..x]]

l1'' :: [(Int, Int)]
l1'' =
  concatMap (\x -> concatMap (\y -> [(x, y)]) [0..x]) [0..10]

l1''' :: [(Int, Int)]
l1''' =
  flip concatMap [0..10] $ \x ->
  flip concatMap [0..x]  $ \y ->
  [(x, y)]

-- concatMap (\x -> [x]) [0..10]   -- identitás függvény
-- do {x <- ma; return x} ~ ma     --

map' :: (a -> b) -> [a] -> [b]
map' f as = [f a | a <- as]

map'' f as = do
  a <- as
  pure (f a)

map''' f as =
  concatMap (\a -> [f a]) as

-- szűrőfeltételek listakifejezében?
l2 :: [(Int, Int)]
l2 = [(x, y) | x <- [0..10], even x, y <- [0..x], even (x + y)]

-- standard : guard   (Control.Monad-ból)

guard' :: Bool -> [()]
guard' True  = [()]    -- "továbengedi" a concatMap-et
guard' False = []      -- "elvágja" a concatMap-et


l2' :: [(Int, Int)]
l2' = do
  x <- [0..10]
  guard' (even x)
  y <- [0..x]
  guard' (even (x + y))
  pure (x, y)

l2'' :: [(Int, Int)]
l2'' = do
  x <- [0..10]
  if even x then pure ()
            else []
  y <- [0..x]
  if even (x + y) then pure ()
                  else []
  pure (x, y)

--
