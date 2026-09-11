{-# OPTIONS_GHC -Wincomplete-patterns -Woverlapping-patterns #-}

module Gy01 where

{-

Tematika:
- Githubon olvasható a hosszab verzió
- Követelmény:
  - Heti házi feladat, mindegyikre 2 hét van, kitöltésük kötelező
  - Félév folyamán 3 db nagybeadandó (3 * 4 pont), nem kötelezőek. 8 pont megszerzése esetén vizsgán +1 jegy ha megvan a kettes
  - Vizsgaidőszakban vizsga az egész féléves gyakorlati tananyagból
  - Előadás: Péntek 14:00
  - Gyakorlatról max 3-szor lehet hiányozni

- A tárgyon tetszőleges IDE és szoftver használható (VSCode, Emacs, Neovim stb) beleértve a Haskell Language Servert
- Vizsgán tetszőleges segédezköz használható emberi segítségen és AI-on kívül

- Órai file-ok: https://github.com/Akaposi/ELTE-func-lang/tree/master/2026-27-1/X
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
xor x y = case x of
  True -> case y of
    False -> True
    _ -> False
  _ -> y

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
twelve' = let x = 6
              y = 5
  in x + y

-- Polimorfizmus: A függvény tetszőleges típusokra működik
id' :: a -> a
id' x = x

-- lehet több típusváltozó is
f1 :: (a, (b, (c, d))) -> (b, c)
f1 (a, (b, (c, d))) = (b, c)

-- Segítség: Hole technológia!
-- Haskellben ha az egyenlőség jobb oldalára _-t írunk, a fordító megmondja milyen típusú kifejezés kell oda

-- Minden függvényre van több megoldás (beépített fügvénnyel pl)

f2 :: (a -> b) -> a -> b
f2 f a = f a  -- ($)   f $ 1 + 3

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 f g x = f (g x) -- <=> (f . g) x
f3 f g x = (f . g) x

f4 :: (a -> b -> c) -> b -> a -> c
f4 f b a = f a b -- flip

-- Segédfüggvények:
-- fst :: (a,b) -> a
-- snd :: (a,b) -> b

f5 :: ((a, b) -> c) -> (a -> (b -> c)) -- Curryzés miatt a -> b -> c == a -> (b -> c)
f5 f a b = f (a, b)

f6 :: (a -> b -> c) -> (a, b) -> c
f6 f (a, b) = f a b

-- Ha az eredménybe függvényt kell megadni használj lambdákat!
-- pl.: \x -> x

f7 :: (a -> (b, c)) -> (a -> b, a -> c)
f7 f = (\a -> fst (f a), \b -> snd (f b))

f8 :: (a -> b, a -> c) -> (a -> (b, c)) -- f8 :: (a -> b, b -> c) -> a -> (b, c)
f8 (f, g) a = (f a, g a)

-- ADT-k emlékeztető:
-- Either adattípus. Két konstruktora van, Left és Right, ami vagy a-t vagy b-t tárol:
{-
:i Either
data Either a b = Left a | Right b
-}

f9 :: Either a b -> Either b a
f9 (Left x) = Right x
f9 (Right y) = Left y
f9 x = case x of
  Left a -> Right a
  Right b -> Left b


f10 :: (Either a b -> c) -> (a -> c, b -> c)
f10 f = (\a -> f $ Left a , \b -> f $ Right b)

f11 :: (a -> c, b -> c) -> (Either a b -> c) -- ⇔ (a -> c, b -> c) -> Either a b -> c
f11 (f, _) (Left a) = f a
f11 (_, g) (Right b) = g b

-- Bónusz

f12 :: Either (a, b) (a, c) -> (a, Either b c)
f12 (Left (a, b)) = (a, Left b)
f12 (Right (a, c)) = (a, Right c)

f13 :: (a, Either b c) -> Either (a, b) (a, c)
f13 (a, Left b) = Left (a , b)
f13 (a, Right c) = Right (a, c)

f14 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f14 f g = f (g (\a -> f a a)) (g (\a -> f a a)) 

-- Listák emlékeztető
-- Hogyan is van a lista definiálva?

-- Definiáljuk a map, filter függvényeket listagenerátorral, rekurzióval és hajtogatással

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (a : as) = f a : map' f as 

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (a : as)
  | f a = a : filter' f as
  | otherwise = filter' f as


-- Definiáljunk egyéb hasznos lista függvényeket, amelyek részei a standard librarynek.
-- !! Vizsgán érdemes nem újrainventálni a teljes Haskell stdlib-et !!

take', drop' :: Int -> [a] -> [a]

take' 0 _ = []
take' n (l : ls) = l : take' (n - 1) ls
take' _ [] = []

drop' n [] = []
drop' n as@(_ : ls)
  | n > 0 = drop' (n - 1) ls
  | otherwise = as

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n ls = (take n ls, drop n ls)

takeWhile', dropWhile' :: (a -> Bool) -> [a] -> [a]

takeWhile' f [] = []
takeWhile' f (l : ls)
  | f l = l : takeWhile' f ls
  | otherwise = []

dropWhile' _ [] = []
dropWhile' f as@(l : ls)
  | f l = dropWhile' f ls
  | otherwise = as

span', partition' :: (a -> Bool) -> [a] -> ([a], [a])

span' = undefined
partition' _ [] = ([], [])
partition' f (l:ls)
  | f l = let (as, bs) = partition' f ls in (l:as, bs)
  | otherwise = let (as, bs) = partition' f ls in (as, l:bs)

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

