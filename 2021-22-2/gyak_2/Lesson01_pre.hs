{-
- Gyak felvételek: Teams->tárgy team->gyakX->files
- canvas EA oldal: tematika + összes info
 - követelmény:
   - gyak: minimum követelmény: 13 pont
     - órai eleji kisfeladat: 0-2 pont (10 perc)
     - nagy házi feladat: félév során 3 darab, 4-4 pontért
        - első hf félév közepén van kiírva
        - mindegyik határideje: vizsgaidőszak eleje
     - canvas-ra egy .hs fájlt kell feltölteni
   - vizsga:
     - jegy ebből származik
     - 2 órás, feladatmegoldás
   - jelenlét:
     - max 4 hiányzás
     - óra eleji feladat (+/- néven ismert dolog, üresen is küldjétek be a feladatot!) + órai aktivitás (képernyőmegosztás és órai feladatok végzése) számít jelenlétnek (távoktatásban!)
       - jelenléti oktatás: jelenlét számít jelenlétnek
- Távoktatás vs jelenléti oktatás?
  - min 3 hétig táv, utána valamikor váltunk
    (szerintem félév nagy részében jelenlét lesz)
- Önálló feladatmegoldás: képernyőt osszatok
- Ha valaki el van maradva: lambda.inf.elte.hu kezdő Haskell jegyzet
                            Learn you a Haskell! könyv, stb.
- Előzetes ismeretek:
  - BSc "funkcionális programozás" ismerete (készség szinten)
    (lambda.inf.elte.hu "Kezdő Haskell" jegyzet ajánlott)
Következő kisfeladat:
  - nagyon egyszerű: lényeg, hogy canvas-ba feladatot feltöltse mindenki
Feladatok, technika, ismétlés
  - ghci parancsok
    :l <fájl>       betöltés
    :r              újratöltés
    :bro            kilistázza a jelenlegi modult
    :bro <modul_név> kilistázza az adott modul függvényeit, adatait
    :t <kifejezés>  kifejezés típusát megadja
    :i <azonosító>  információt ír ki (operátorról is)
  - {-# options_ghc -Wincomplete-patterns #-}   ("options pragma", ez éppen a parciális mintaillesztésekre figyelmeztet)
  - case, let, where, mintaillesztés
  - hole-ok
-}


--------------------------------------------------------------------------------

{-# options_ghc -Wincomplete-patterns #-}

module Lesson01 where

-- Gyakorló feladatok (ismétlés, függvények, mintaillesztés, ADT-k, osztályok)
--------------------------------------------------------------------------------
-- Definiáld a "xor" műveletet Bool típuson. Használj mintaillesztést,
-- vagy Prelude-ből standard függvényt.
xor :: Bool -> Bool -> Bool
xor = undefined


-- -- "case" kifejezéssel
-- xor' :: Bool -> Bool -> Bool
-- xor' x y = case x of
--   True  -> _
--   False -> _

-- "let" / "where"

foo :: Int -> Int -> Int
foo x y = a + b + f y where
  a = 100 + x
  b = 200 + y

  f :: Int -> Int
  f 0 = 10
  f _ = 100

foo2 :: Int -> Int -> Int
foo2 x y =
  let a = 100 + x
      b = 200 + y

      f :: Int -> Int
      f 0 = 10
      f _ = 100

  in a + b + f y

  -- egysoros let példa:  "let x = 10 in x + 20"


-- függvények
--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen, de
-- típushelyesen és totális függvényként (azaz nem lehet végtelen rekurzió
-- vagy kivétel dobás!).

id' :: a -> a
id' = undefined

-- polimorf függvény: tetszőleges a, b, c, d típusra működik
--    a, b, c, d: "típusváltozó", kisbetűs nevek típusparaméterek (tetszőleges típusok)
f1 :: (a, (b, (c, d))) -> (b, c)
f1 = undefined

   -- érték :: típus   : gyakran érdemes értéket a típusáról elnevezni

   -- hole:
   --   _-t teszünk definícióba, ghci megírja, hogy milyen típusú kifejezést
   --       kéne a helyére írni + lokális scope-beli típusokat
   -- _             _ :: (b, c)
   -- (_, _)        _ :: b     _ :: c


f2 :: (a -> b) -> a -> b
f2 = undefined

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 = undefined

f4 :: (a -> b -> c) -> b -> a -> c
f4 = undefined

f5 :: ((a, b) -> c) -> (a -> (b -> c))
f5 = undefined

f6 :: (a -> b -> c) -> (a, b) -> c
f6 = undefined

f7 :: (a -> (b, c)) -> (a -> b, a -> c)
f7 = undefined

f8 :: (a -> b, a -> c) -> (a -> (b, c))
f8 = undefined

f9 :: (Either a b -> c) -> (a -> c, b -> c)
f9 = undefined

f10 :: (a -> c, b -> c) -> (Either a b -> c)
f10 = undefined

f11 :: Either (a, b) (a, c) -> (a, Either b c)
f11 = undefined

f12 :: (a, Either b c) -> Either (a, b) (a, c)
f12 = undefined

-- (nehezebb)
f13 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f13 = undefined


-- listák
--------------------------------------------------------------------------------

-- Írj egy "applyMany :: [a -> b] -> a -> [b]" függvényt, ami egy
-- listában található minden függvényt alkalmaz egy
-- értékre. Pl. "applyMany [(+10), (*10)] 10 == [20, 100]".
-- applyMany :: [a -> b] -> a -> [b]
-- applyMany = undefined

applyMany :: [a -> b] -> a -> [b]
applyMany = undefined


-- Definiálj egy "NonEmptyList a" típust ADT-ként,
-- aminek az értékei nemüres listák.

--   - Írj egy "toList :: NonEmptyList a -> [a]" függvényt!

--   - Írj egy "fromList :: [a] -> Maybe (NonEmptyList a)" függvényt, ami
--     nemüres listát ad vissza egy standard listából, ha az input nem
--     üres.


-- Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
-- az összes bemenő függvény kompozíciója,
-- pl. "composeAll [f, g, h] x == f (g (h x))"
composeAll :: [a -> a] -> a -> a
composeAll = undefined


-- (bónusz) Definiáld a "sublists :: [a] -> [[a]]" függvényt, ami a bemenő lista
-- minden lehetséges részlistáját visszaadja. Pl:
-- sublists [1, 2] == [[], [1], [2], [1, 2]]
-- sublists "abc" == ["","a","b","c","ab","ac","bc","abc"]
-- A részlisták sorrendje az eredményben tetszőleges, a
-- fontos, hogy az összes részlista szerepeljen.
-- Kapcsolódó fogalom: hatványhalmaz
-- Segítség: Nem kell túlbonyolítani. Azt kell leírni, hogy egy adott vagy benne van a részlistában vagy nincs benne.
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

data List a
data Color
data Tree a

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

instance Eq' a => Eq' (List a) where
  eq = undefined

instance Ord' a => Ord' (List a) where
  lte = undefined

instance Show' a => Show' (List a) where
  show' = undefined

instance Eq' a => Eq' (Tree a) where
  eq = undefined

instance Ord' a => Ord' (Tree a) where
  lte = undefined

instance Show' a => Show' (Tree a) where
  show' = undefined