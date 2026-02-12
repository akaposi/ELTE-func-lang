{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Gy01 where
import Data.Binary.Get (Decoder(Fail))

{-

Tematika:
- Githubon olvasható a hosszab verzió
- Követelmény:
  - Heti házi feladat, mindegyikre 2 hét van, kitöltésük kötelező
  - Félév folyamán 3 db nagybeadandó (3 * 4 pont), nem kötelezőek. 8 pont megszerzése esetén vizsgán +1 jegy ha megvan a kettes
  - Vizsgaidőszakban vizsga az egész féléves gyakorlati tananyagból
  - Előadás: Kedd 10:00
  - Gyakorlatról max 3-szor lehet hiányozni

- A tárgyon tetszőleges IDE és szoftver használható (VSCode, Emacs, Neovim stb) beleértve a Haskell Language Servert
- Vizsgán tetszőleges segédezköz használható emberi segítségen és AI-on kívül

- Órai file-ok: https://github.com/Akaposi/ELTE-func-lang/tree/master/2025-26-2/gyakX
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
xor True True = False 
xor False False = False 
xor _ _ = True

xor' :: Bool -> Bool -> Bool
xor' a b = a /= b

f :: Int -> Int -> Int
f x y
  | y < 4 = x - y
  | otherwise = case x + y of
                3 -> 136
                _ -> -1 
-- Több megoldás is lehet (mintaillesztés, beépített függvények)
-- Új "case" kifejezés
{-
case x + y of
  3 -> ...
  _ -> ...
-}

-- Let/Where kifejezések: lokális definíciók:
twelve :: Int
twelve = x + x
  where
    x = 6
    help :: a -> a
    help a = a

twelve' :: Int
twelve' = let x = 6 in x + x

-- Polimorfizmus: A függvény tetszőleges típusokra működik
id' :: Num a => a -> b -> a
id' x y = x

-- lehet több típusváltozó is
f1 :: (a, (b, (c, d))) -> (b, c)
f1 (a, (b, (c, d))) = (b, c)



-- Segítség: Hole technológia!
-- Haskellben ha az egyenlőség jobb oldalára _-t írunk, a fordító megmondja milyen típusú kifejezés kell oda

-- Minden függvényre van több megoldás (beépített fügvénnyel pl)

f2 :: (a -> b) -> a -> b
f2 f = f

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 f g = f . g -- f (g a)

f4 :: (a -> b -> c) -> b -> a -> c
f4 f b a = f a b 

-- Segédfüggvények:
-- fst :: (a,b) -> a
-- snd :: (a,b) -> b

f5 :: ((a, b) -> c) -> (a -> (b -> c)) -- Curryzés miatt a -> b -> c == a -> (b -> c)
f5 = curry

f5' :: ((a, b) -> c) -> (a -> (b -> c)) -- Curryzés miatt a -> b -> c == a -> (b -> c)
f5' f a b = f (a , b)

f6 :: (a -> b -> c) -> (a, b) -> c
f6 = uncurry

f6' :: (a -> b -> c) -> (a, b) -> c
f6' f (a,b) = f a b

-- Ha az eredménybe függvényt kell megadni használj lambdákat!
-- pl.: \x -> x

f7 :: (a -> (b, c)) -> (a -> b, a -> c)
f7 f = (\x -> fst (f x) , \x -> snd (f x))

f8 :: (a -> b, a -> c) -> (a -> (b, c))
f8 (f , g) a = (f a, g a)

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
f10 f = (\a -> f (Left a), \b -> f (Right b)) 

f11 :: (a -> c, b -> c) -> (Either a b -> c)
f11 (f,g) (Right b) = g b
f11 (f,g) (Left a) = f a

-- Bónusz

f12 :: Either (a, b) (a, c) -> (a, Either b c)
f12 = undefined

f13 :: (a, Either b c) -> Either (a, b) (a, c)
f13 = undefined

f14 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f14 = undefined

-- Listák emlékeztető
-- Hogyan is van a lista definiálva?

-- Definiáljuk a map, filter függvényeket listagenerátorral, rekurzióval és hajtogatással

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p l = [ x | x <- l, p x] 


-- Definiáljunk egyéb hasznos lista függvényeket, amelyek részei a standard librarynek.
-- !! Vizsgán érdemes nem újrainventálni a teljes Haskell stdlib-et !!

take', drop' :: Int -> [a] -> [a]

take' = undefined
drop' = undefined

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' = undefined

takeWhile', dropWhile' :: (a -> Bool) -> [a] -> [a]

takeWhile' = undefined
dropWhile' = undefined

span', partition' :: (a -> Bool) -> [a] -> ([a], [a])

span' = undefined
partition' = undefined

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' = undefined

cycle' :: [a] -> [a]
cycle' = undefined

iterate' :: a -> (a -> a) -> [a]
iterate' = undefined

repeat' :: a -> [a]
repeat' = undefined

replicate' :: Int -> a -> [a]
replicate' = undefined

nub' :: Eq a => [a] -> [a]
nub' = undefined

