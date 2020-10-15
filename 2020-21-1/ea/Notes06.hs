
{-# language MonadComprehensions #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- Lista monád, list comprehension, sublists, guard (Alternative)
-- két class: Foldable, Traversable
-- Parser monád
-- Nagyobb példa: parser + interpreter minimális nyelvre ("while" nyelv, int, bool + while ciklus, if-then-else, változó értékadás)

--------------------------------------------------------------------------------

{-# language DeriveFunctor #-}

import Data.Traversable
import Control.Applicative
import Control.Monad

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


-- lista monád
--------------------------------------------------------------------------------

-- instance Monad [] where
--   return a = [a]
--   as >>= f = concatMap f as
--   -- (>>=) = flip concatMap

-- (<*>) :: [a -> b] -> [a] -> [b]

-- példa:

list1 :: [(Int, Int)]
list1 = do
  x <- [0..10]      -- legyen x bármilyen szám a listából (nem-determinisztikus értékadás)
  y <- [0..x]       -- legyen y bármilyen a listából
  pure (x, y)

-- BSc stílusban
list1' :: [(Int, Int)]
list1' = [(x, y) | x <- [0..10], y <- [0..x]]

-- concatMap-el:
list1'' :: [(Int, Int)]
list1'' = concatMap (\x -> concatMap (\y -> [(x, y)]) [0..x]) [0..10]

-- lista kifejezés: szintaktikus cukorka lista monád instance-ra!
-- lista kifejezésben: szűrőfeltétel, (+lokális let-definíció)

list2 :: [(Int, Int)]
list2 = [(x, y) | x <- [0..10], let foo = x * x, y <- [0..x], even y]

list2' :: [(Int, Int)]
list2' = do
  x <- [0..10]
  let foo = x * x
  y <- [0..x]
  if even y then pure ()        -- (pure () == [()]) és (concatMap f [a] == f a) minden f,a-ra
            else []             -- bármelyik sorban ha [] szerepel, akkor a végeredmény is [] (concatMap f [] == [])
  pure (x, y)

-- emlékezzünk: pure (): nincs mellékhatása
-- (pure () >> ma) == ma
-- Lista monádban: egy elemű listával concatMap: végeredmény hossza 1-el szorzódik ("mellékhatás" nélküli lista)


filter' :: (a -> Bool) -> [a] -> [a]
filter' f as = [a | a <- as, f a]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f as = do
  a <- as
  if f a then pure a else []
  -- filter'' f as = concatMap (\a -> if f a then [a] else []) as

list3 :: [Int]
list3 = (+) <$> [0..10] <*> [0..10]     -- minden lehetséges kombinációra alkalmazza a (+)-t

   -- do x <- [0..10]
   --    y <- [0..10]
   --    pure (x + y)

-- példa generikus Monad függvényekre

-- mapM :: (a -> m b) -> [a] -> m [b]
-- mapM :: (a -> [b]) -> [a] -> [[b]]

list4 :: [[Int]]
list4 = mapM (\x -> pure x) [0, 1, 2, 3]   -- (házi feladat: kibontani a mapM definíciót, + lista monád definíciót)

list5 :: [[Int]]
list5 = mapM (\x -> [x, x*(-1)]) [0, 1, 2, 3]  -- [x, x*(-1)] összes lehetséges kombinációja végeredmény

-- klasszikus példa: összes lehetséges részlista megadása
-- (generikus Monad függvényel)
sublists :: [a] -> [[a]]   -- összes lehetséges mód, hogy 0 vagy több elemet kiszűrjek/elhagyjak a listából
sublists as = filterM (\a -> [True, False]) as

  -- szűrőfeltétel legyen igaz és hamis is (mindkét lehetőséget járjuk be)
  -- filterM :: (a -> m Bool) -> [a] -> m [a]
  -- (házi feladat: kibontani a filterM/lista monád definíciót, hogy látszódjon a működés)

-- Alternative
--------------------------------------------------------------------------------

-- Definiálva van a Control.Applicative-ban
-- class Applicative f => Alternative f where
--   empty :: f a
--   (<|>) :: f a -> f a -> f a

-- Monoid, viszont nem konkrét típusra, hanem (f :: * -> *) konstruktorokra
-- Alternative f : tetszőleges "a" típusra "f a" Monoid.

-- instance Monoid (f a) where
-- Alternative f => ...       (minden "a"-ra Monoid (f a))
-- Monoid (f a)  => ...       (konkrét "a"-ra Monoid (f a))

-- instance Alternative [] where
--   empty = []
--   (<|>) = (++)

-- érdekesebb példa:

-- instance Alternative Maybe where
--    empty = Nothing
--    Just a <|> _ = Just a         -- balról az első Just-ot visszaadja (ha iterált (<|>) kifejezés van)
--    _      <|> m = m


-- Általánosítja a list szűrőfeltételt:

-- guard :: Alternative f => Bool -> f ()
-- guard b = if b then pure () else empty

-- guard listára megfelel a korábban látott szűrésnek (do blokkban)

list2'' :: [(Int, Int)]
list2'' = do
  x <- [0..10]
  let foo = x * x
  y <- [0..x]
  guard (even y)    -- konkrét fordítása a szűrő kifejezésnek
  pure (x, y)

-- miért guard-ra fordul a lista kifejezés, ha csak a lista guard-ot szeretnénk?
-- válasz: nem feltétlenül csak listára van lista kifejezés, hanem "monád kifejezés"
-- {-# language MonadComprehensions #-}

io1 :: IO String
io1 = [xs ++ ys | xs <- getLine, ys <- getLine]
  -- do {xs <- getLine; ys <- getLine; pure (xs ++ ys)}


-- Foldable
--------------------------------------------------------------------------------

-- class Foldable t where
--   foldr :: (a -> b -> b) -> b -> t a -> b

-- Minden olyan "t"-re adható instance, ami foldr-ezhető
-- ekvivalens metódus: toList :: t a -> [a]

-- (standard: Data.Foldable)
toList :: Foldable t => t a -> [a]
toList ta = foldr (:) [] ta

foldr' :: (t a -> [a]) -> (a -> b -> b) -> b -> t a -> b
foldr' toList f b ta = foldr f b (toList ta)

-- hatékonyabb, ha köztes lista nélkül fold-olunk

data T a = T a a a

instance Foldable T where
  foldr f b (T x y z) = f x (f y (f z b))

-- generikus Foldable függvények: length, sum, product, elem, find

-- (feladat: írjunk foldr-t fákra)
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Functor)

instance Foldable Tree where    -- nem könnyű
  foldr = undefined


-- Traversable
--------------------------------------------------------------------------------

-- class (Functor t, Foldable t) => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- először néztük mapM :: ...
--                mapA ::                 Applicative f  => (a -> f b) -> [a] -> f [b]
--            traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

-- mapM túlterhelése, nem csak listán működik

instance Traversable Tree where
  -- hajtsuk végre a műveletet minden "a"-n a Leaf-ekben, adjuk vissza
  -- az eredmény értékek fáját
  traverse f (Leaf a)   = Leaf <$> f a
  traverse f (Node l r) = Node <$> traverse f l <*> traverse f r

-- IO példa: traverse (\_ -> getLine) (Node (Leaf 0) (Leaf 10))
-- State példa:

-- korábbi label függvény definíciója
-- label :: Tree a -> Tree (a, Int)
-- label t = evalState (traverse go t) 0 where
--   go :: a -> State Int (a, Int)
--   go a = do
--     n <- get
--     put (n + 1)
--     pure (a, n)

-- Korábbi osztályok használata definiált típusokon:
--   Functor: fmap
--   Foldable: sum, length, foldr, foldl, ....
--   Traversable: traverse (map-elés + mellékhatás)

-- Fenti három: derive-olható!
-- {-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

data Tree2 a = Leaf2 a | Node2A (Tree2 a) (Tree2 a) | Node2B (Tree2 a) (Tree2 a) (Tree2 a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- általánosított label: működik Tree, Tree2, lista, stb...
label :: Traversable t => t a -> t (a, Int)
label t = evalState (traverse go t) 0 where
  go :: a -> State Int (a, Int)
  go a = do
    n <- get
    put (n + 1)
    pure (a, n)

-- mikor *nem* Traversable/Foldable?  (viszont Functor)
-- alap példa: függvény Functor, de nem Traversable/Foldable

-- (Foldable/Traversable: probléma, hogy minden lehetséges String input-ra meg kéne hívnom a függvényt)
newtype StringFun a = StringFun (String -> a) deriving (Functor)

-- Első házi: jövő héttől
--  beadási határidő: szorgalmi időszak vége (mindhárom feladatra)
