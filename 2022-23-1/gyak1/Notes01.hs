{-
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
    - max 3 hiányzás
    - óra eleji feladat (+/- néven ismert dolog, üresen is küldjétek be a feladatot!) + órai aktivitás

- Önálló feladatmegoldások
- Ha valaki el van maradva: lambda.inf.elte.hu kezdő Haskell jegyzet
                            Learn you a Haskell! könyv, stb.
- Órai fájlok: https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2022-23-1/gyak1
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
xor True  True  = False
xor False False = False
xor _     _     = True


-- "case" kifejezéssel
xor' :: Bool -> Bool -> Bool
xor' x y = case x of
  True  -> not y
  False -> y

xor'' :: Bool -> Bool -> Bool
xor'' = (/=)


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
id' x = x

-- polimorf függvény: tetszőleges a, b, c, d típusra működik
--    a, b, c, d: "típusváltozó", kisbetűs nevek típusparaméterek (tetszőleges típusok)
f1 :: (a, (b, (c, d))) -> (b, c)
f1 (a, (b, (c, d))) = (b, c)

   -- érték :: típus   : gyakran érdemes értéket a típusáról elnevezni

   -- hole:
   --   _-t teszünk definícióba, ghci megírja, hogy milyen típusú kifejezést
   --       kéne a helyére írni + lokális scope-beli típusokat
   -- _             _ :: (b, c)
   -- (_, _)        _ :: b     _ :: c


f2 :: (a -> b) -> a -> b
f2 f x = f x

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 f g a = f (g a)

f4 :: (a -> b -> c) -> b -> a -> c
f4 f b a = f a b

f5 :: ((a, b) -> c) -> (a -> (b -> c))
f5 f a b = f (a, b)

f6 :: (a -> b -> c) -> (a, b) -> c
f6 f (a,b) = f a b
-- f6 f x = f (fst x) (snd x)

f7 :: (a -> (b, c)) -> (a -> b, a -> c)
f7 f = (\a -> fst (f a) , \a -> snd (f a)) 

f8 :: (a -> b, a -> c) -> (a -> (b, c))
f8 (f,g) a = (f a, g a)

-- data Either a b = Left a | Right b

f9 :: (Either a b -> c) -> (a -> c, b -> c)
f9 f = (\a -> f (Left a) , \b -> f (Right b))

f10 :: (a -> c, b -> c) -> (Either a b -> c)
f10 (f,g) = \ab -> case ab of 
                    Left  a -> f a
                    Right b -> g b

f10' :: (a -> c, b -> c) -> (Either a b -> c)
f10' (f,g) (Left  a) = f a
f10' (f,g) (Right b) = g b

f11 :: Either (a, b) (a, c) -> (a, Either b c)
f11 (Left  (a,b)) = (a, Left b)
f11 (Right (a,c)) = (a, Right c)

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
composeAll []     a = a
composeAll (f:fs) a = f (composeAll fs a)

{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc []     = acc
foldr f acc (x:xs) = f x (foldr f acc xs)
-}

composeAll' :: [a -> a] -> a -> a
composeAll' fs a = foldr (\f acc -> f acc) a fs

composeAll'' :: [a -> a] -> a -> a
composeAll'' = flip (foldr ($))

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

data Color = Red | Green | Blue
data List a
data Tree a

-- írd meg a következő instance-okat
instance Eq Color where
  (==) Red   Red   = True
  (==) Green Green = True
  (==) Blue  Blue  = True
  (==) _     _     = False

instance Eq' Color where
  eq Red   Red   = True
  eq Green Green = True
  eq Blue  Blue  = True
  eq _     _     = False

instance Ord Color where
  Red   <= _     = True
  Green <= Green = True
  Green <= Blue  = True
  Blue  <= Blue  = True
  _     <= _     = False

instance Ord' Color where
  lte Red   _     = True
  lte Green Green = True
  lte Green Blue  = True
  lte Blue  Blue  = True
  lte _     _     = False

instance Show' Color where
  show' _ = "Red" 

instance Show Color where
  show Red   = "Red"
  show Green = "Green"
  show Blue  = "Blue"

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