{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant lambda" #-}

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
  - Előadás: Szerda 16:00-17:30
  - Gyakorlatról max 3-szor lehet hiányozni

- A tárgyon tetszőleges IDE és szoftver használható (VSCode, Emacs, Neovim stb) beleértve a Haskell Language Servert
- KisZH folyamán tetszőleges segédezköz használható emberi segítségen kívül (Vizsga TBA)

- Órai file-ok: https://github.com/Akaposi/ELTE-func-lang/tree/master/2024-25-01/gyakX
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
xor = (/=)

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
f1 (_ , (b , (c , _))) = (b , c)

-- Segítség: Hole technológia!
-- Haskellben ha az egyenlőség jobb oldalára _-t írunk, a fordító megmondja milyen típusú kifejezés kell oda

-- Minden függvényre van több megoldás (beépített fügvénnyel pl)

f2 :: (a -> b) -> a -> b
f2 = id

f3 :: (b -> c) -> (a -> b) -> a -> c
--f3 = (.)
f3 f g = \a -> f (g a)

f4 :: (a -> b -> c) -> b -> a -> c
f4 = flip

-- Segédfüggvények:
-- fst :: (a,b) -> a
-- snd :: (a,b) -> b

f5 :: ((a, b) -> c) -> (a -> (b -> c)) -- Curryzés miatt a -> b -> c == a -> (b -> c)
f5 f a b = f (a , b)

f6 :: (a -> b -> c) -> (a, b) -> c
f6 (+) (a , b) = a + b

-- Ha az eredménybe függvényt kell megadni használj lambdákat!
-- pl.: \x -> x

f7 :: (a -> (b, c)) -> (a -> b, a -> c)
f7 f = (fst . f , snd . f)

f8 :: (a -> b, a -> c) -> (a -> (b, c))
f8 (f , g) a = (f a , g a)

-- ADT-k emlékeztető:
-- Either adattípus. Két konstruktora van, Left és Right, ami vagy a-t vagy b-t tárol:
{-
:i Either
data Either a b = Left a | Right b
-}

f9 :: Either a b -> Either b a
f9 (Left a) = Right a
f9 (Right b) = Left b


f10 :: (Either a b -> c) -> (a -> c, b -> c)
f10 f = (\a -> f (Left a) ,\b ->  f (Right b))

f11 :: (a -> c, b -> c) -> (Either a b -> c)
f11 (f , g) x = case x of
  (Left a) -> f a
  (Right b) -> g b

-- Bónusz

f12 :: Either (a, b) (a, c) -> (a, Either b c)
f12 (Left (a , b)) = (a , Left b)
f12 (Right (a , c)) = (a , Right c)


f13 :: (a, Either b c) -> Either (a, b) (a, c)
f13 (a , eith) = case eith of
  Left b -> (Left (a , b))
  Right c -> (Right (a , c))

f14 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f14 f g = f (g (\a -> f a a)) (g (\a -> f a a))

-- Listák emlékeztető
-- Listának két konstruktora van: [] és (:)

-- Definiáljuk a map függvényt listagenerátorral, rekurzióval és hajtogatással

map' :: (a -> b) -> [a] -> [b]
--map' f a = [ f e | e <- a]
{-
map' _ [] = []
map' f (x : xs) = (f x) : map' f xs
-}
map' f xs = foldr (\a bs -> (f a) : bs) [] xs

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
data List a  = Nil | Cons a (List a)-- = ???

-- Bináris fa
-- Ilyen nincs beépítve nekünk kell kitalálni
-- Minden belső csúcsnak pontosan 2 részfája legyen
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- Írjunk ezekre a datákra instance-okat!
instance Eq Colour where
  (==) :: Colour -> Colour -> Bool -- régebbi GHC-ben nem lehetett ezt kiírni InstanceSigs nélkül
  (RGB r g b) == (RGB a s d) = r == a && g == s && b == d
  (HSL h s l) == (HSL a s1 d) = h == a && s == s1 && l == d
  _ == _ = False

instance Show Colour where
  show :: Colour -> String
  show (RGB r g b) = "RGB: r=" ++ show r ++ " g=" ++ show g ++ " b=" ++ show b -- Elég szubjektív
  show (HSL h s l) = "HSL: h=" ++ show h ++ " s=" ++ show s ++ " l=" ++ show l -- Elég szubjektív


instance Ord Colour where
  (<=) :: Colour -> Colour -> Bool -- Fura kérdés, színeket nem nagyon lehet így összehasonlítani
  (<=) (RGB r g b) (RGB a s d) = r <= a && g <= s && b <= d
  (<=) (HSL h s l) (HSL f g j) = h <= f && s <= g && l <= j
  (<=) _ _ = False

--        Kontextus    Instance head
--         v            v
instance (Show a) => Show (List a) where -- Extra Show a kikötés különben nem tudnák kiírni az elemeket
  show :: List a -> String
  show Nil = "[]"
  show (Cons a xs) = show a ++ ":" ++ show xs


instance (Eq a) => Eq (List a) where
  (==) :: List a -> List a -> Bool
  (==) Nil Nil = True
  (==) (Cons a xs) (Cons b ys) = a == b && xs == ys
  (==) _ _ = False

-- Bónusz
instance (Eq a) => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) (Leaf a) (Leaf b) = a == b
  (==) (Node l r) (Node g h) = l == g && r == h
  (==) _ _ = False

