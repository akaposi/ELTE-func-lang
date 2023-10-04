{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}
module Gy04 where

data SparseList a = Nil | Skip (SparseList a) | Cons a (SparseList a)
  deriving (Show)

exampleList :: SparseList Char
exampleList = Cons 's' (Cons 'a' (Skip (Cons 'j' (Skip (Cons 't' Nil)))))

exampleList' :: SparseList Int
exampleList' = Cons 1 (Skip (Cons 4 Nil))

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

exampleTree :: Tree String
exampleTree = Node
                (Node (Leaf "the") (Leaf "cake"))
                (Node (Leaf "is") (Node (Leaf "a") (Leaf "lie")))

{-
             ∙
            / \
           /   \
          /     \
         ∙       \
        / \       \
       /   \       \
   "the"   "cake"   ∙
                   / \
                  /   \
               "is"    ∙
                      / \
                     /   \
                   "a"   "lie"
-}

exampleTree' :: Tree Int
exampleTree' = Node (Node (Leaf 2) (Leaf 3)) (Leaf 5)

{-
        ∙
       / \
      ∙   5
     / \
    2   3
-}


-- Foldable
--------------------------------------------------------------------------------

-- class Foldable f where
--   foldr :: (a -> b -> b) -> b -> f a -> b
--   foldl
--   foldMap
--   ...

instance Foldable SparseList where
  foldr :: (a -> b -> b) -> b -> SparseList a -> b
  foldr f z sl = undefined

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z t = undefined


-- Maybe monád motiváló feladatok
--------------------------------------------------------------------------------

-- Írd meg a következő függvényt. A függvény úgy működik,
-- mint a lista "filter", viszont ha a kapott (a -> Maybe Bool)
-- függvény valamely elemre Nothing-ot ad, akkor Nothing legyen
-- a végeredmény, egyébként Just <szűrt lista>
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f as = undefined


-- Alkalmazz egy (a -> Maybe b) függvény egy Tree minden
-- levelére, ha bármelyik alkalmazás Nothing-ot ad,
-- legyen az eredmény Nothing!
mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree f t = undefined


-- Alkalmazzuk páronként a kapott (a -> b -> Maybe c) függvényt a bemenő listák
-- elemeire! Ha bármelyik függvényalkalmazás Nothing, akkor a kimenet legyen
-- Nothing, egyébként Just <lista zippelés eredménye>.
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe f as bs = undefined


-- Definiáld újra az előbbi három feladatot Maybe monád instance használatával!

filterMaybe' :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe' f as = undefined

mapMaybeTree' :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree' f t = undefined

zipWithMaybe' :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe' f as bs = undefined


-- IO monád
--------------------------------------------------------------------------------

-- getLine  :: IO String             -- beolvas
-- print    :: Show a => a -> IO ()  -- kinyomtat értéket
-- putStrLn :: String -> IO ()       -- String-et nyomtat ki

-- (>>=)  :: IO a -> (a -> IO b) -> IO b
-- return :: a -> IO a
-- fmap   :: (a -> b) -> IO a -> IO b


-- Írj egy függvényt, ami beolvas egy sort, majd visszaadja a sorban található
-- 'a' betűk számát!
io1 :: IO ()
io1 = undefined


-- Írj egy függvényt, ami beolvas egy sort, majd a sort kinyomtatja annyiszor,
-- ahány karakter van a sorban!
io2 :: IO ()
io2 = undefined


-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert. Ha a sorban 'x' van, akkor a program nyomtassa ki az
-- összes eddig beolvasott sort és térjen vissza.
io3 :: IO ()
io3 = undefined


-- A következőt ismételd végtelenül: olvass be egy sort, majd nyomtasd ki a
-- sorban a kisbetűk számát.  A Ctrl-c-c -vel lehet megszakítani a futtatást
-- ghci-ben.
io4 :: IO ()
io4 = undefined


-- Bónusz funktor feladatok múlt hétről
--------------------------------------------------------------------------------

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = undefined

apply :: Functor f => f (a -> b) -> a -> f b
apply = undefined

first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first = undefined

second :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
second = undefined

data Sum f g a = Inl (f a) | Inr (g a) deriving Show
data Product f g a = Product (f a) (g a) deriving Show
newtype Compose f g a = Compose (f (g a)) deriving Show

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap = undefined

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap = undefined

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap = undefined


-- bónusz bónusz: mire használható ez a függvény? Tipp: a megoldáshoz
-- rekurzió szükséges.
löb :: Functor f => f (f a -> a) -> f a
löb = undefined

-- bónusz bónusz 2:
newtype Fix f = Fix (f (Fix f))

fold :: Functor f => (f a -> a) -> Fix f -> a
fold = undefined
