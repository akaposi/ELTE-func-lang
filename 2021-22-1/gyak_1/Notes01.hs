

-- online résztvevők: megosztják a képernyőjüket
--   mikrofonból szóltok, akkor könnyebben tudok reagálni

--------------------------------------------------------------------------------

-- jövő héttől kezdve: canvas feladat óra elején + jelenléti ív
--   egy darab .hs fájlt feltölteni (1 feladat + tesztek)

-- jegyzeteket felteszem (EA + gyak) órák után github-ra
-- források:
--   - EA + gyak jegyzetek, régebbi szemeszterek:
--     https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2021-22-1
--
--   - régebbi interaktív jegyzet ("Kezdő Haskell") ismétlés:
--     http://lambda.inf.elte.hu/Index.xml

-- Ismétlés: BSc funck. prog tárgy, készség szintű ismeret

-- Következő óra eleji feladat: könnyű (egyszerű függvény definíció)

--------------------------------------------------------------------------------


{-# options_ghc -Wincomplete-patterns #-}

import Data.List (foldl')


-- Gyakorló feladatok (ismétlés, függvények, mintaillesztés, ADT-k, osztályok)
-- Lásd "Feladatok01.hs"
--------------------------------------------------------------------------------


-- Definiáld a "xor" műveletet Bool típuson. Használj mintaillesztést,
-- vagy Prelude-ből standard függvényt.
xor :: Bool -> Bool -> Bool
xor = undefined



-- függvények
--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen, de
-- típushelyesen és totális függvényként (azaz nem lehet végtelen loop
-- vagy kivétel dobás!).
f1 :: (a, (b, (c, d))) -> (b, c)
f1 (a, (b, (c, d))) = (b, c)                         -- (a, (b, (c, d))) -> (b, c)

     -- gyakran jó ötlet: érték váltázó neve lehet ugyanaz, mint a típusa

     -- undefined : bármilyen típusa lehet, rögtön egy kivételt dob

     -- type hole-ok használata (ghci-ben)
     --    :l <file>        -- file betöltése ghci-ben
     --    :r               -- újratöltés
     -- "type hole"


f2 :: (a -> b) -> a -> b        -- standard: ($) operátor
f2 f a = f a

    -- map (+10) (map (+20) [0..10])
    -- map (+10) $ map (+20) [0..10]

--      3 darab paraméter
f3 :: (b -> c) -> (a -> b) -> a -> c
f3 f g a = f (g a)                        -- kérdés: melyik standard függvény?

f3' :: (b -> c) -> (a -> b) -> a -> c
f3' = (.)                                 -- (.) :: (b -> c) -> (a -> b) -> a -> c
                                          -- első osztályú függvény, fv típusú
                                          -- értékre hivatkozok azonnal


-- f4 :: (a -> b -> c) -> (b -> a -> c)      -- curry-zés!
-- f4 = undefined

   -- több-paraméteres függvény = függvényt visszaadó függvények

-- f4 :: (a -> (b -> c)) -> (b -> (a -> c))      -- curry-zés!
-- f4 = undefined                                -- csak 1-paraméteres függvények vannak!

f4 :: (a -> b -> c) -> b -> a -> c
f4 f b a = f a b                       -- standard függvény: flip
                                       -- első két argumentumot felcseréli


-- párt kapó függvény -> két argumentumot kapó függvény
f5 :: ((a, b) -> c) -> a -> b -> c
f5 f a b = f (a, b)           -- standard függvény: curry

-- uncurry (curry inverze)
f5b :: (a -> b -> c) -> (a, b) -> c
f5b f (a, b) = f a b

-- lambda kifejezés!
f6 :: (a -> (b, c)) -> ((a -> b), (a -> c))   -- (függvények párja az eredmény)
f6 f = ((\a -> fst (f a)), (\a -> snd (f a)))
    -- pusztán a type hole-al lehet definíciókat írni mechanikusan
    -- _ :: (a -> b, a -> c)     -- látom, hogy pár kell
    -- (_, _)
    --     1. hole : _ :: a -> b
    --         ((\a -> _), _)
    --     2. hole : _ :: a -> c
   -- f6 f = ((fst . f), (snd . f))

-- Következő előadás:
f7 :: (a -> b, a -> c) -> (a -> (b, c))
f7 (f, g) a = (f a, g a)

-- standard típus Either

-- template <a, b>
-- data Either a b = Left a | Right b

e1 :: Either Bool Int
e1 = Left True

e2 :: Either Bool Int
e2 = Right 300

-- mintaillesztés (megcseréli a két konstruktort)
foo :: Either Int Bool -> Either Bool Int
foo (Left n)  = Right n
foo (Right b) = Left b

-- mintaillesztés: "case" kifejezéssel

foo' :: Either Int Bool -> Either Bool Int
foo' eib = case eib of
  Left n  -> Right n
  Right b -> Left b
  -- minták ugyannaban az oszlopban legyenek a bal oldalon

foo'' :: Either Int Bool -> Either Bool Int
foo'' eib = case eib of Left n -> Right n; Right b -> Left b


--     függvény ami a-t és b-is
--        fel tud dolgozni             a-t váró függvény és b-t váró függvény
f8 :: (Either a b -> c)      ->      ((a -> c), (b -> c))
f8 f = ((\a -> f (Left a)), (\b -> f (Right b)))
    -- ((f . Left), (f . Right))

-- exp :: Either a b
-- f :: Either a b -> c
--   f (Left ...)
---  f (Right ...)


-- f :: c -> Either a b
--   f c

-- standard függvény kövtkező típussal:
-- either :: (a -> c) -> (b -> c) ->  Either a b -> c
-- maybe  :: b        -> (a -> b) ->  Maybe a    -> b
--  (gyakori minta: esetszétválsztást függvényként kifaktoráljuk)

f9 :: (a -> c, b -> c)  ->  (Either a b -> c)
f9 (f, g) = \eab -> case eab of               -- mindegy, hogy paraméter van
   Left a -> f a                              -- vagy lambda
   Right b -> g b

-- Either és a pár "disztributivitása"
f10 :: Either (a, b) (a, c) -> (a, Either b c)
f10 (Left  (a, b)) = (a, Left b)
f10 (Right (a, c)) = (a, Right c)

f10' :: Either (a, b) (a, c) -> (a, Either b c)
f10' eabac = case eabac of
  Left (a, b)  -> (a, Left b)
  Right (a, c) -> (a, Right c)
  -- space-ek és tabok!
  --  szerkesztő: space-eket használjon, ne tabokat

f11 :: (a, Either b c) -> Either (a, b) (a, c)
f11 (a, Left  b) = Left (a, b)
f11 (a, Right c) = Right (a, c)

-- bónusz feladat (nehéz!)
f12 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f12 = undefined


-- listák
--------------------------------------------------------------------------------

-- Írj egy "applyMany :: [a -> b] -> a -> [b]" függvényt, ami egy
-- listában található minden függvényt alkalmaz egy
-- értékre. Pl. "applyMany [(+10), (*10)] 10 == [20, 100]".
-- applyMany :: [a -> b] -> a -> [b]
-- applyMany = undefined

applyMany :: [a -> b] -> a -> [b]
applyMany = undefined


-- Definiálj egy "NonEmptyList a" típust, akár ADT-ként, akár
-- típusszinonímaként, aminek az értékei nemüres listák.

--   - Írj egy "toList :: NonEmptyList a -> [a]" függvényt!

--   - Írj egy "fromList :: [a] -> Maybe (NonEmptyList a)" függvényt, ami
--     nemüres listát ad vissza egy standard listából, ha az input nem
--     üres.


-- Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
-- az összes bemenő függvény kompozíciója,
-- pl. "composeAll [f, g, h] x == f (g (h x))"
composeAll :: [a -> a] -> a -> a
composeAll = undefined


-- Definiáld a "merge :: Ord a => [a] -> [a] -> [a]" függvényt, ami két nemcsökkenő
-- rendezett listát összefésül úgy, hogy az eredmény is rendezett maradjon.
merge :: Ord a => [a] -> [a] -> [a]
merge = undefined


-- (bónusz) Definiáld a "mergeSort :: Ord a => [a] -> [a]" függvényt, ami a "merge"
-- iterált felhasználásával rendez egy listát.
mergeSort :: Ord a => [a] -> [a]
mergeSort = undefined


-- (bónusz) Definiáld a "sublists :: [a] -> [[a]]" függvényt, ami a bemenő lista
-- minden lehetséges részlistáját visszaadja. Pl. "sublists [1, 2] == [[],
-- [1], [2], [1, 2]]".  A részlisták sorrendje az eredményben tetszőleges, a
-- fontos, hogy az össze részlista szerepeljen.
-- Kapcsolódó fogalom: hatványhalmaz
sublists :: [a] -> [[a]]
sublists = undefined


-- osztályok
--------------------------------------------------------------------------------

class Eq' a where
  eq :: a -> a -> Bool

class Eq' a => Ord' a where
  lte :: a -> a -> Bool

class Show' a where
  show' :: a -> String

data Tree a = Leaf a | Node (Tree a) (Tree a)
data Color = Red | Green | Blue

-- írd meg a következő instance-okat
instance Eq' Color where
  eq = undefined

instance Ord' Color where
  lte = undefined

instance Show' Color where
  show' = undefined

instance Eq' a => Eq' (Maybe a) where
  eq = undefined

instance Ord' a => Ord' (Maybe a) where
  lte = undefined

instance Show' a => Show' (Maybe a) where
  show' = undefined

instance Eq' a => Eq' [a] where
  eq = undefined

instance Ord' a => Ord' [a] where
  lte = undefined

instance Show' a => Show' [a] where
  show' = undefined

instance Eq' a => Eq' (Tree a) where
  eq = undefined

instance Ord' a => Ord' (Tree a) where
  lte = undefined

instance Show' a => Show' (Tree a) where
  show' = undefined
