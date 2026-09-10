{-# OPTIONS_GHC -Wincomplete-patterns #-}

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

-- >>> :t 1 + "alma"
-- No instance for `Num String' arising from the literal `1'
-- In the first argument of `(+)', namely `1'
-- In the expression: 1 + "alma"

-- Mai téma: Ismétlés (függvények, mintaillesztés, algebrai adattípusok, típusosztályok)
xor :: Bool -> Bool -> Bool
xor False False = False
xor True True = False
xor _ _ = True

xor' a b = a /= b

xor'' a b = case a of
  True -> not b
  False -> b

--- >>> xor False False
-- False

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
f1 (a, (b, (c, d))) = (b, c)

-- Segítség: Hole technológia!
-- Haskellben ha az egyenlőség jobb oldalára _-t írunk, a fordító megmondja milyen típusú kifejezés kell oda

-- Minden függvényre van több megoldás (beépített fügvénnyel pl)

f2 :: (a -> b) -> a -> b
f2 f a = f a -- ($)

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 = undefined -- (.)

f4 :: (a -> b -> c) -> b -> a -> c
f4 f b a = f a b -- flip 

-- Segédfüggvények:
-- fst :: (a,b) -> a
-- snd :: (a,b) -> b

f5 :: ((a, b) -> c) -> (a -> (b -> c)) -- Curryzés miatt a -> b -> c == a -> (b -> c)
f5 f a b = f (a, b)

f6 :: (a -> b -> c) -> (a, b) -> c
f6 = undefined

-- Ha az eredménybe függvényt kell megadni használj lambdákat!
-- pl.: \x -> x

f7 :: (a -> (b, c)) -> (a -> b, a -> c)
f7 f = (\a -> fst (f a), \a -> snd (f a))

f8 :: (a -> b, a -> c) -> (a -> (b, c))
f8 = undefined

-- ADT-k emlékeztető:
-- Either adattípus. Két konstruktora van, Left és Right, ami vagy a-t vagy b-t tárol:
{-
:i Either
data Either a b = Left a | Right b
-}

f9 :: Either a b -> Either b a
f9 e = case e of
  Left a -> Right a
  Right b -> Left b

f10 :: (Either a b -> c) -> (a -> c, b -> c)
f10 f = (\x -> f (Left x), \x -> f (Right x)) 

f11 :: (a -> c, b -> c) -> (Either a b -> c)
f11 = undefined

-- Bónusz

f12 :: Either (a, b) (a, c) -> (a, Either b c)
f12 = undefined

f13 :: (a, Either b c) -> Either (a, b) (a, c)
f13 = undefined

f14 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f14 f g = let b = g (\a -> f a a) in f b b 

-- Listák emlékeztető
-- Hogyan is van a lista definiálva?

-- Definiáljuk a map, filter függvényeket listagenerátorral, rekurzióval és hajtogatással

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x : xs) = f x : map' f xs

map'' f a = [f x | x <- a]

map''' f a = foldr (\x as -> f x : as) [] a 

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) -- = if p x then x : filter' p xs else filter' p xs
  | p x = x : filter' p xs
  | otherwise = filter' p xs

-- >>> filter (>5) [1..10]
-- [6,7,8,9,10]


-- Definiáljunk egyéb hasznos lista függvényeket, amelyek részei a standard librarynek.
-- !! Vizsgán érdemes nem újrainventálni a teljes Haskell stdlib-et !!

take', drop' :: Int -> [a] -> [a]

take' = undefined
drop' = undefined

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' 0 xs = ([], xs)
splitAt' _ [] = ([], [])
splitAt' i (x:xs) = let (l, r) = splitAt' (i - 1) xs in (x:l, r)

-- >>> splitAt' 3 "almafa"
-- ("alm","afa")

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

