{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, LambdaCase #-}

module Gy01 where

{-


Tematika:
- Canvas EA oldalán olvasható a hosszab verzió
- Követelmény:
  - Óra elején KisZH (12 * 2 pont) canvasban kb 10-15 perc, csak gyakorlati feladatok
  - Félév folyamán 3 db házi (3 * 4 pont) (első kb félév közepén)
  - Az összes 36 pontból 13-at kell összeszedni, akkor lehet menni vizsgázni
  - A 13 ponton túl a KisZH és a Házik nem kötelezőek
  - Vizsgaidőszakban vizsga az egész féléves tananyagból
  - Elővizsga nincs
  - Előadás: Csütörtök 19:30-21:00, Lágymányosi Déli Tömb 2-502 (Nem kötelező)
  - Gyakorlatról max 3-szor lehet hiányozni

- A tárgyon tetszőleges IDE és szoftver használható (VSCode, Emacs, Neovim stb) beleértve a Haskell Language Servert
- KisZH folyamán tetszőleges segédezköz használható emberi segítségen kívül (Vizsga TBA)

- Órai file-ok: https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2023-24-1/gyak1
- GyXX_pre.hs = Óra előtti fájl
- GyXX.hs     = Óra utáni fájl

- A tárgy a Funkcionális Programozás (IP-18FUNPEG) tárgyra épül
- Aki el van maradva: lambda.inf.elte.hu

GHCi emlékeztető:
- :l <fájl>     - betölti a fájlt a GHCi-be
- :r            - újratölti a betöltött fájlokat
- :bro <modul>  - browse rövidítése, kiírja egy modul tartalmát
- :t <kif>      - megmondja egy kifejezés típusát
- :i <azon>     - kiírja egy fv/típus/stb információját (kötési erősség, hol van definiálva stb)
- :set <flag>   - bekapcsol egy flag-et (pl -Wincomplete-patterns)
- :q            - kilépés

Pragmák:
{-# <PRAGMA> <OPCIÓK> #-}
- Ez mindig a fájl tetejére megy
- Fontosabb pragmák:
  - OPTIONS_GHC: bekapcsol GHC flageket, pl -Wincomplete-patterns ami warningot ad ha egy mintaillesztés nem totális
  - LANGUAGE: Nyelvi kiegészítők bekapcsolása, pl InstanceSigs ami engedi az instance-ok függvényeinek az explicit típusozását

-}

alma :: Int
alma = 1

-- Mai téma: Ismétlés (függvények, mintaillesztés, algebrai adattípusok, típusosztályok)
-- Definiáld az "xor" függvényt a Bool típuson.
xor :: Bool -> Bool -> Bool
xor x y = not (x == y)

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' True True = False
xor' False False = False

xor'' :: Bool -> Bool -> Bool
xor'' True True = False
xor'' False False = False
xor'' _ _ = True

xor''' :: Bool -> Bool -> Bool
xor''' x y = case x of
  True -> not y
  False -> y

-- Több megoldás is lehet (mintaillesztés, beépített függvények)
-- Új "case" kifejezés
{-
case x of
  True -> ...
  False -> ...
-}

-- Let/Where kifejezések: lokális definíciók:
twelve :: Int
twelve = x + x
  where
    x = 6

twelve' :: Int
twelve' = let x = 6 in x + x

-- Polimorfizmus: A függvény tetszőleges típusokra működik
id' :: a -> a
id' x = x

-- lehet több típusváltozó is
f1 :: (a, (b, (c, d))) -> (b, c)
f1 (a , (b , (c , d))) = (b,c)

-- Segítség: Hole technológia!
-- Haskellben ha az egyenlőség jobb oldalára _-t írunk, a fordító megmondja milyen típusú kifejezés kell oda

-- Minden függvényre van több megoldás (beépített fügvénnyel pl)

f2 :: (a -> b) -> a -> b
f2 f a = f a

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 f g a = f (g a)

f4 :: (a -> b -> c) -> b -> a -> c
f4 f x y = f y x

-- Segédfüggvények:
-- fst :: (a,b) -> a
-- snd :: (a,b) -> b

--    |          |    O      O    X
f5 :: ((a,b) -> c) -> (a -> (b -> c)) -- Curryzés miatt a -> b -> c == a -> (b -> c)
f5 f a b = f (a,b)

f6 :: (a -> b -> c) -> (a,b) -> c
f6 x (y,z) = x y z

-- Ha az eredménybe függvényt kell megadni használj lambdákat!
-- pl.: \x -> x

f7 :: (a -> (b,c)) -> (a -> b, a -> c)
f7 f = (\a -> fst (f a) , snd . f)

f8 :: (a -> b, a -> c) -> (a -> (b, c))
f8 (x,y) = \a -> (x a, y a)

-- ADT-k emlékeztető:
-- Either adattípus. Két konstruktora van, Left és Right, ami vagy a-t vagy b-t tárol:
{-
:i Either
data Either a b = Left a | Right b
-}

data Fruit = Apple | Banana | Tomato

feither :: Either a b -> Either b a
feither (Left a) = Right a
feither (Right b) = Left b

f9 :: (Either a b -> c) -> (a -> c, b -> c)
f9 f = (\a -> f (Left a), \a -> f (Right a))

f10 :: (a -> c, b -> c) -> (Either a b -> c)
f10 (f, g) (Left a) = f a
f10 (f, g) (Right b) = g b

-- Bónusz

f11 :: Either (a,b) (a,c) -> (a, Either b c)
f11 = undefined

f12 :: (a, Either b c) -> Either (a, b) (a, c)
f12 = undefined

f13 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f13 = undefined

-- Listák emlékeztető
-- Listának két konstruktora van: [] és :

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (a:as) = f a : map' f as

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\a b -> f a : b) []

map''' :: (a -> b) -> [a] -> [b]
map''' f xs = [ f x | x <- xs ]
-- Definiálj egy függvényt, amely egy listányi függvényt kap paraméterül, és azokat
-- alkalmazza a második kapott paraméterre!
-- pl: applyMany [(+10), (*10)] 10 == [20, 100]

applyMany :: [a -> b] -> a -> [b]
applyMany = undefined

-- Gyakorlás ha valakinek nehézségei vannak a listákkal
-- Definiálj egy függvényt, amely egy listányi a -> a függvényeknek veszi a kompozícióját
-- Avagy composeAll [f, g, h] x == f (g (h x))
composeAll :: [a -> a] -> a -> a
composeAll = undefined

-- Daták és osztályok
-- Ismert típusosztályok: Eq, Ord, Show
-- Ezek definíciója:
{-
class Eq a where
  (==) :: a -> a -> Bool

class Eq a => Ord a where
  (<=) :: a -> a -> Bool

class Show a where
  show :: a -> String
-}

-- Daták

-- Egyszerű data, nincs típusparamétere
-- Legyen két konstruktora, RGB aminek három szám paramétere van és HSL aminek szintén három szám paramétere van
data Colour

-- Mik a lista konstruktorai és azok paraméterei?
-- GHCi meg tudja mondani
data List a -- = ???

-- Bináris fa
-- Ilyen nincs beépítve nekünk kell kitalálni
-- Minden belső csúcsnak pontosan 2 részfája legyen
data Tree a

-- Írjunk ezekre a datákra instance-okat!
instance Eq Colour where
  (==) :: Colour -> Colour -> Bool -- régebbi GHC-ben nem lehetett ezt kiírni InstanceSigs nélkül
  (==) = undefined

instance Show Colour where
  show :: Colour -> String
  show = undefined -- Elég szubjektív

instance Ord Colour where
  (<=) :: Colour -> Colour -> Bool -- Fura kérdés, színeket nem nagyon lehet így összehasonlítani
  (<=) = undefined

instance Show a => Show (List a) where -- Extra Show a kikötés különben nem tudnák kiírni az elemeket
  show ::  List a -> String
  show = undefined

instance Eq a => Eq (List a) where
  (==) :: List a -> List a -> Bool
  (==) = undefined

-- Bónusz
instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) = undefined

-- Jövő heti KisZh, egy hasonló fv mint az f1-f10 és egy instance írás valami egyszerűbb datára
