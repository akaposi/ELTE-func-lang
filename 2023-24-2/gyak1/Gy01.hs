{-# OPTIONS_GHC -Wincomplete-patterns #-}

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
  - Előadás: IDŐPONT IDE
  - Gyakorlatról max 3-szor lehet hiányozni

- A tárgyon tetszőleges IDE és szoftver használható (VSCode, Emacs, Neovim stb) beleértve a Haskell Language Servert
- KisZH folyamán tetszőleges segédezköz használható emberi segítségen kívül (Vizsga TBA)

- Órai file-ok: https://github.com/Akaposi/ELTE-func-lang/tree/master/2023-24-2/gyakX
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

-- Mai téma: Ismétlés (függvények, mintaillesztés, algebrai adattípusok, típusosztályok)
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

xor' :: Bool -> Bool -> Bool
xor' = (/=)


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
twelve' = let 
  x = 6 
  in x + x

-- Polimorfizmus: A függvény tetszőleges típusokra működik
id' :: a -> a
id' x = x

-- lehet több típusváltozó is
f1 :: (a, (b, (c, d))) -> (b, c)
f1 (_, (k, (h, _))) = (k, h)

-- Segítség: Hole technológia!
-- Haskellben ha az egyenlőség jobb oldalára _-t írunk, a fordító megmondja milyen típusú kifejezés kell oda

-- Minden függvényre van több megoldás (beépített fügvénnyel pl)

f2 :: (a -> b) -> a -> b
f2 = ($)

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 f g = \ a -> f $ g a  --(.)

f4 :: (a -> b -> c) -> b -> a -> c
f4 f h k = f k h

-- Segédfüggvények:
-- fst :: (a,b) -> a
-- snd :: (a,b) -> b

f5 :: ((a, b) -> c) -> (a -> (b -> c)) -- Curryzés miatt a -> b -> c == a -> (b -> c)
f5 f h k = f (h , k)

f6 :: (a -> b -> c) -> (a, b) -> c
f6 f t@(a, b) = f (fst t) (snd t)

-- Ha az eredménybe függvényt kell megadni használj lambdákat!
-- pl.: \x -> x

f7 :: (a -> (b, c)) -> (a -> b, a -> c)
f7 f = (fst . f , snd . f)

f8 :: (a -> b, a -> c) -> (a -> (b, c))
f8 (f, g) a = (f a, g a) -- (,) prefix módon is használható!

-- ADT-k emlékeztető:
-- Either adattípus. Két konstruktora van, Left és Right, ami vagy a-t vagy b-t tárol:
{-
:i Either
data Either a b = Left a | Right b
-}

--- Maybe a = Nothing | Just a

f9 :: Either a b -> Either b a
f9 (Left e ) = Right e
f9 (Right e) = Left e

f10 :: (Either a b -> c) -> (a -> c, b -> c)
f10 f = ( f . Left , \b -> f (Right b))
  

f11 :: (a -> c, b -> c) -> (Either a b -> c)
f11 (f , g) e = case e of
  Left h  -> f h 
  Right h -> g h

-- Bónusz

f12 :: Either (a, b) (a, c) -> (a, Either b c)
f12 (Left f)  = ( fst f , Left $ snd f )
f12 (Right f) = ( fst f , Right (snd f) )

f13 :: (a, Either b c) -> Either (a, b) (a, c)
f13 (h, k) = case k of
  Left j1  -> Left (h, j1)
  Right j2 -> Right (h , j2)

f14 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f14 f g = let h = g (\a -> f a a) in f h h

-- Listák emlékeztető
-- Listának két konstruktora van: [] és :

-- Definiáljuk a map függvényt listagenerátorral, rekurzióval és hajtogatással

map' :: (a -> b) -> [a] -> [b]
-- map' f ls = [f i | i <- ls]
{- map' f [] = []
map' f (x : xs) = f x : map' f xs
-}
map' f ls = foldr (\a ls -> f a : ls) [] ls 

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
data Colour = RGB Int Int Int | HSL Int Int Int

-- Mik a lista konstruktorai és azok paraméterei?
-- GHCi meg tudja mondani
data List a where
  Nil :: List a
  Cons :: a -> List a -> List a

-- Bináris fa
-- Ilyen nincs beépítve nekünk kell kitalálni
-- Minden belső csúcsnak pontosan 2 részfája legyen
data Tree a where
  Leaf :: a -> Tree a
  Node :: Tree a -> Tree a -> Tree a

-- Írjunk ezekre a datákra instance-okat!
instance Eq Colour where
  (==) :: Colour -> Colour -> Bool -- régebbi GHC-ben nem lehetett ezt kiírni InstanceSigs nélkül
  (==) (RGB r g b) (RGB r1 g1 b1) = (r == r1) && (g == g1) && (b == b1)
  (==) (HSL h s l) (HSL h1 s1 l1) = (h == h1) && (s == s1) && (l == l1)
  (==) _ _ = False

instance Show Colour where
  show :: Colour -> String
  show (RGB r g b) = "RGB: r = " ++ show r ++ " g = " ++ show g ++ " b = " ++ show b
  show _ = "Kiscica" -- Elég szubjektív

instance Ord Colour where
  (<=) :: Colour -> Colour -> Bool -- Fura kérdés, színeket nem nagyon lehet így összehasonlítani
  (<=) (RGB r g b) (RGB r1 g1 b1) = r <= r1 && g <= g1 && b <= b1 
  (<=) (HSL h s l) (HSL h1 s1 l1) = h <= h1 && s <= s1 && l <= l1
  (<=) (RGB _ _ _) (HSL _ _ _) = True
  (<=) (HSL _ _ _) (RGB _ _ _) = False

--        Kontextus    Instance head
--         v            v
instance (Show a) => Show (List a) where -- Extra Show a kikötés különben nem tudnák kiírni az elemeket
  show :: List a -> String
  show Nil = "Nil"
  show (Cons h hs) = show h ++ " : " ++ show hs

instance (Eq a) => Eq (List a) where
  (==) :: List a -> List a -> Bool
  (==) Nil Nil = True
  (==) Nil (Cons _ _) = False
  (==) (Cons _ _) Nil = False
  (==) (Cons l ls) (Cons h hs) = l == h && ls == hs

-- Bónusz
instance (Eq a) => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) = undefined
