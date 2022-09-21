{-
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
     - max 3 hiányzás
     - óra eleji feladat (+/- néven ismert dolog, üresen is küldjétek be a feladatot!) + órai aktivitás (képernyőmegosztás és órai feladatok végzése) számít jelenlétnek (távoktatásban!)
       - jelenléti oktatás: jelenlét számít jelenlétnek
- Ha valaki el van maradva: lambda.inf.elte.hu kezdő Haskell jegyzet
                            Learn you a Haskell! könyv, stb.
- Órai fájlok: https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2022-23-1/gyak2
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
  - {-# OPTIONS_GHC -Wincomplete-patterns #-}   ("options pragma", ez éppen a parciális mintaillesztésekre figyelmeztet)
  - case, let, where, mintaillesztés
  - hole-ok
-}


--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Ora1 where

-- Gyakorló feladatok (ismétlés, függvények, mintaillesztés, ADT-k, osztályok)
--------------------------------------------------------------------------------
-- Definiáld a "xor" műveletet Bool típuson. Használj mintaillesztést,
-- vagy Prelude-ből standard függvényt.
xor :: Bool -> Bool -> Bool
xor b1 b2 = b1 /= b2

xor' :: Bool -> Bool -> Bool
xor' True True = False
xor' False False = False
xor' _ _ = True


xor'' :: Bool -> Bool -> Bool
xor'' x y = case x of
  True -> not y
  False -> y

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
f1  (_, (b, (c, _))) = (b, c)

   -- érték :: típus   : gyakran érdemes értéket a típusáról elnevezni

   -- hole:
   --   _-t teszünk definícióba, ghci megírja, hogy milyen típusú kifejezést
   --       kéne a helyére írni + lokális scope-beli típusokat
   -- _             _ :: (b, c)
   -- (_, _)        _ :: b     _ :: c


f2 :: (a -> b) -> a -> b
f2 f a = f a
f2' f = f


f3 :: (b -> c) -> (a -> b) -> a -> c
f3 = (.)
--f3 f g a = f $ g a

f4 :: (a -> b -> c) -> b -> a -> c
f4 f  b a = f a b

f5 :: ((a, b) -> c) -> (a -> (b -> c))
f5 f a b = f (a,b)

f6 :: (a -> b -> c) -> (a, b) -> c
f6 f (a,b) = f a b 

f7 :: (a -> (b, c)) -> (a -> b, a -> c)
f7 f = (\x -> fst (f x), \x -> snd (f x))

f8 :: (a -> b, a -> c) -> (a -> (b, c))
f8 (a,b) x = (a x, b x)

-- data Either a b = Left a | Right b

f9 :: (Either a b -> c) -> (a -> c, b -> c)
f9 f = (\a -> f $ Left a, \b -> f $ Right b)

f10 :: (a -> c, b -> c) -> (Either a b -> c)
f10 (g1, g2) = \e -> case e of
  Left a -> g1 a
  Right b -> g2 b

f11 :: Either (a, b) (a, c) -> (a, Either b c)
f11 x = case x of
  Left (a,b) -> (a, Left b)
  Right (a,c) -> (a, Right c)

f12 :: (a, Either b c) -> Either (a, b) (a, c)
f12 (a,x) = case x of
  Left b -> Left (a,b)
  Right c -> Right (a,c)

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
applyMany (x:xs) a = x a : applyMany xs a
applyMany [] a = []

--applyMany' l a = map ($ a) l
--applyMany' l a = foldr (\f b -> (f a ) : b) [] l


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
composeAll (x:xs) a = composeAll xs (x a)
composeAll [] a = a
--- composeAll = foldr (.) id  


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
  lte :: a -> a -> Bool -- les than or equal (<=)

class Show' a where
  show' :: a -> String

data List a = Nil | Cons a (List a)
data Color = Green | Red | Blue | Yellow
data Tree a

-- írd meg a következő instance-okat
instance Eq' Color where
  eq Green Green = True
  eq Blue Blue = True
  eq Red Red = True
  eq Yellow Yellow = True
  eq _ _ = False

instance Ord' Color where
  lte _ Red = True
  lte Red Blue = False
  lte _ Blue = True


instance Show' Color where
  show' Green = "Green"
  show' Yellow = "Yellow"
  show' Red = "Red"
  show' Blue = "Blue"

instance Eq' a => Eq' (Maybe a) where
  eq = undefined

instance Ord' a => Ord' (Maybe a) where
  lte = undefined

instance Show' a => Show' (Maybe a) where
  show' = undefined

instance Eq' a => Eq' (List a) where -- = >
  eq Nil Nil = True
  eq (Cons x xs)  (Cons y ys) = eq x y && eq xs ys
  eq _ _ = False

instance Ord' a => Ord' (List a) where
  lte = undefined

instance Show a => Show' (List a) where
  show' Nil = "[]"
  show' (Cons x xs) = "[" ++ show x ++  show'' xs ++ "]"
    where
      show'' Nil = ""
      show'' (Cons x xs) = "," ++ show x ++ show'' xs

instance Eq' a => Eq' (Tree a) where
  eq = undefined

instance Ord' a => Ord' (Tree a) where
  lte = undefined

instance Show' a => Show' (Tree a) where
  show' = undefined