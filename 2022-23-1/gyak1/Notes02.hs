
-- ADT-k, osztály definíciók, feladatok
--------------------------------------------------------------------------------

-- köv +- feladat: konkrétan az itteni Tree típusra egy egyszerű rekurzív
--   függvény

--------------------------------------------------------------------------------

-- nyelvi feature-öket állítani
{-# language InstanceSigs #-}
   -- GHC language flag reference

-- gh/ghci-nek opciót átadni
{-# options_ghc -Wincomplete-patterns #-}
  -- warning, ha hiányos mintaillesztést csinálunk

-- alapból a Prelude van importálva, viszont
--  "import Prelude hiding"-al lehet elrejteni ezeket a definíciókat
--     hiding (Eq)      magát az "Eq" osztályt rejti el
--     hiding (Eq(..))  az "Eq" osztály + a metódusait rejti el
--  hasonló ADT-kre:
--     hiding (Bool)      típust rejti
--     hiding (Bool(..))  típust + konstruktorokat rejti
import Prelude hiding (Eq(..), Show(..), Ord(..), Semigroup(..), Monoid(..))

-- incomplete :: Bool -> Bool
-- incomplete True = True     -- warning! incomplete False exception-t dob

-- Definiáld a következő függvényeket!
------------------------------------------------------------

  -- típus , "a" típusparaméter
data Tree a
  = Leaf a                  -- "Leaf" konstruktor, 1 db "a" típusú mező
  | Node (Tree a) (Tree a)  -- "Node" konstr, 2 db (Tree a) típusú mező

-- 1. megakapjuk :
--   Leaf :: a -> Tree a
--   Node :: Tree a -> Tree a -> Tree a

t1 :: Tree Int
t1 = Leaf 100

t2 :: Tree Int
t2 = Node (Leaf 10) (Leaf 20)  -- bináris elágazás, két levél alatta

-- 2. megkapjuk: mintaillesztést konstruktorokra

-- Számold meg a leveleket.
numLeaves :: Tree a -> Int
numLeaves (Leaf _)   = 1     -- "default" minta
numLeaves (Node l r) = numLeaves l + numLeaves r  -- "left" és "right"

-- numLeaves t2 == 2

-- egymásba ágyazott mintaillesztés:
--  listák esetén:
--  f :: [a] -> [a]
--  f (x:y:z:xs) = ...

foo :: Tree a -> Bool
foo (Node (Leaf _) (Leaf _)) = True
foo _                        = False

-- Alkalmazz egy függvényt az összes tárolt értékre
-- Rekurzít típusra rekurzív függvény:
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a)   = Leaf (f a)
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

-- Összegezd a fában tárolt értékeket.
-- A "Num" osztály metódusa a "(+)" művelet.
sumTree :: Num a => Tree a -> a
sumTree (Leaf a)   = a
sumTree (Node l r) = sumTree l + sumTree r

-- Add vissza az elemek listáját.
-- Figyelem: a legegyszerűbb definíció nem hatékony (kvadratikus)!
-- Próbálj a fa méretében lineáris komplexitású definíciót adni.
treeToList :: Tree a -> [a]
treeToList (Leaf a)   = [a]
treeToList (Node l r) = treeToList l ++ treeToList r

   -- xs ++ ys : lemásolja az "xs"-t
   --      költség: xs hossza
   -- (((as ++ bs) ++ cs) ++ ds)  -- kvadratikus!

   -- hogyan csinálunk lineáris toList függvényt?

treeToList' :: Tree a -> [a]
treeToList' t = go t [] where

  -- privát segédfüggvényt
  go :: Tree a -> [a] -> [a]
  go (Leaf a) acc   = a:acc           -- lista elejét kiegészíteni O(1)
  go (Node l r) acc = go l (go r acc)

-- Definiálj egy "NonEmptyList a" típust ADT-ként, aminek az értékei nemüres
-- listák.
data NonEmptyList a
  = Single a                 -- pontosan 1 hosszú
  | Cons a (NonEmptyList a)  -- kiegészítünk egy listát egy új elemmel

-- standard lista típus:
-- data List a = Single a | Cons a (List a)

-- konvertálás listává
toList :: NonEmptyList a -> [a]
toList (Single a)  = [a]
toList (Cons a as) = a : toList as

-- Konvertálj listát nemüres listává! Ha a bemenet üres, legyen Nothing az
-- eredmény.
fromList :: [a] -> Maybe (NonEmptyList a)
fromList []     = Nothing
fromList (a:as) = case fromList as of
  Nothing -> Just (Single a)
  Just as -> Just (Cons a as)
  -- opcionális házi: oldjuk meg úgy, hogy
  --   csak egy üres/nemüres esetszétválasztás legyen

-- Osztályok
------------------------------------------------------------

data Color = Red | Green | Blue

-- Eq: osztály
-- "a": osztály paraméter
class Eq a where
  (==) :: a -> a -> Bool    -- (==) metódus, operátorként deklarálva
  infix 4 ==                -- nem asszociatív művelet

class Eq a => Ord a where   -- rendezés
  (<) :: a -> a -> Bool
  infix 4 <

class Show a where          -- értékek String-é alakítása
  show :: a -> String

-- írd meg a következő instance-okat
instance Eq Color where
  (==) :: Color -> Color -> Bool
  (==) Red Red = True
  (==) Blue Blue = True
  (==) Green Green = True
  (==) _ _ = False

instance Ord Color where
  (<) = undefined

instance Show Color where
  show = undefined

instance Eq a => Eq (Maybe a) where
  (==) = undefined

instance Ord a => Ord (Maybe a) where
  (<) = undefined

instance Show a => Show (Maybe a) where
  show = undefined

instance Eq a => Eq [a] where
  (==) = undefined

instance Ord a => Ord [a] where
  (<) = undefined

instance Show a => Show [a] where
  show = undefined

instance Eq a => Eq (Tree a) where
  (==) = undefined

instance Ord a => Ord (Tree a) where
  (<) = undefined

instance Show a => Show (Tree a) where
  -- bónusz: adj meg hatékony definíciót!
  show = undefined


-- Írd meg a következő instance-okat úgy, hogy megfeleljenek
-- az osztály törvényeknek!
------------------------------------------------------------

class Semigroup a where
  (<>) :: a -> a -> a   -- asszociatív művelet
  infixr 6 <>

class Semigroup a => Monoid a where
  mempty :: a  -- egységeleme <>-nek

instance Semigroup [a] where
  (<>) :: [a] -> [a] -> [a]
  (<>) = undefined

instance Monoid [a] where
  mempty :: [a]
  mempty = undefined

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (<>) :: (a, b) -> (a, b) -> (a, b)
  (<>) = undefined

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty :: (a, b)
  mempty = undefined

instance Semigroup b => Semigroup (a -> b) where
  (<>) :: (a -> b) -> (a -> b) -> (a -> b)
  (<>) = undefined

instance Monoid b => Monoid (a -> b) where
  mempty :: a -> b
  mempty = undefined
