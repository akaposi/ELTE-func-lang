{-# language BangPatterns #-}

module Notes12 where

import Data.List
import Data.Foldable

-- Haskell fordítás/optimalizálás
--------------------------------------------------------------------------------

-- Optimalizálás: fordító által
--                programozó által

-- Haskell-ben:
--    - GHC jelentősen átalakítja a kódot,
--    - (több lehetség adott transzformálására, mellékhatás-mentes kódot szabadon átrendezi a fordító)
--       példa: C kód, hívunk egy fordítási egységen kívüli függvényt)
--    - Nehéz ránézésre kitalálni, hogy mire fordul adott Haskell kód
--    - Programozó általi optimalizáláshoz szükséges a fordítás és runtime system ismerete
--    - GHC sokat javít, de ha komolyan akarunk optimalizálni, akkor jelentős tudás kell ehhez, a kapott támogatás
--      a GHC-től pedig nem túl sok.
--    - Összességében compiler jellegű alkalmazásoknál a GHC jó választás az RTS teljesítménye és optimalizációs
--      profilja miatt (hatékony kis allokációkhoz és szintaxis-transzformációhoz)


-- Alap futási modell
--------------------------------------------------------------------------------

-- 1. magasabbrendű függvények

-- egyszerűsítés: Bool ~ 64 bit érték, vagy 0 vagy 1 (lustaság miatt: 0 vagy 1 vagy 2)

f :: Bool -> Bool -> Bool
f b1 b2 = if b1 then b2 else True

-- szaturált hívás (egy függvényt az összes paraméterrel hívjuk)
-- pl: f True False (ugyanaz az impl, mint Java/C)

-- parciális applikáció: closure létrehozása
-- let g = f True

h :: Bool -> Maybe (Bool -> Bool)
h x = if x then Just (f True)       -- closure:   | f pointer | 1 | True |           (fv ptr, arg. száma, argumentumok)
           else Just not

--  case mf of
--    Just f -> f False   -- (f lehet top-level (Bool -> Bool), lehet parciális applikáció, lehet más fajta closure is)
                          -- (ha parciális applikáció, akkor meg kell vizsgálni, hogy hány paraméter hiányzik)
                          --    (ha van összes param, akkor meghívjuk a fv-t, egyébként új closure-t csinálunk)


-- closure létrehozása:

h2 :: Int -> Maybe (Int -> Int)
h2 x = Just (\y -> x + y + x + 100)

-- data Int = Int# Int#
-- Int#  -- primitív 64 bites Int (nem pointer! maga a szám)
-- data Int = I# Int#
--  általános notáció: #-es függvények és típusok és literálok "primitívek" (nem lehet semmi thunk! konkrét gépi műveletek)

-- myFun :: Int -> Int -> Int
-- myFun x y = x + y + y            -- (inline függvény)


-- worker-wrapper transformation: gyors, primitív verzió vs lassú verzió

-- gyors verzió
wmyFun :: Int# -> Int# -> Int#
wmyFun x y = x +# y +# y

-- wrapper
myFun :: Int -> Int -> Int
myFun (I# x) (I# y) = I# (wmyFun x y)

-- További "unboxing" példa: (Int, Int)  -->  stack-allokált Int# Int# pár


-- függvény definíció hivatkozik lokális paraméterre/definícióra
-- fordítás: minden függvényből top-level függvény lesz
-- (lokális "capture"-t megkapja minden ilyen függvény, mint extra paraméter)

-- fenti lambda, mint top függvény: lam1 = \x y -> x + y + x + 100

-- Just (\y -> x + y + x + 100)   -->    | lam1 ptr | 1 (arg száma) | x |       (closure)


-- szuperszaturált hívás: több arg, mint a "hivatalos" kód paraméter
--    pl: id not True        (id: 1 formális paraméter, 2 konkrét paraméter a hívásban)
--

-- fordító 2 paraméteres top függvényre fordít
h2' :: Int -> Int -> Int
h2' x = if (x == 0) then \y -> x + y + x + 100
                    else \y -> x + y + x + 90

h2'' :: Int -> Int -> Int
h2'' x y = if (x == 0) then x + y + x + 100
                       else x + y + x + 90

--  átalakítás ami mellékhatásokkal nem feltétlenül helyes:
--   f         :: Bool -> Bool
--   \x -> f x :: Bool -> Bool


-- Lustaság:
--------------------------------------------------------------------------------

-- függvényhívás csak akkor fut le, ha a végeredményre mintát illesztünk

h3 :: Int -> Maybe Int
h3 x = Just (x + x + x + x + x)

-- thunk: lusta hívás

-- thunk:    | kód ptr | <hely kihagyva a végeredménynek> | paraméterek száma | capture (tömb értékekkel)

-- thunk1: return (1000 + 1000)
--

--   let foo = (+) 1000 1000     --->   | thunk1 |   | 0 | []

--   case foo of    -- minden mintaillesztés: extra ág
--    thunk -> ...   (meghívjuk a ptr-t a tárolt paraméterekkel, és próbáljuk a fennmaradó eseteket)
--                    mutáció: kihagyott mezőbe beírjuk a hívás eredményét)  (max egyszer értékelünk ki thunk-ot)
--                     (szemétgyűjtő talál egy kiértékelt thunk-ot: thunk-ot kidobja, és kicseréli a végeredménnyel)
--      0   -> ...
--      _   -> ...


-- strictness analízis: minél több thunk létrehozást megpróbál elkerülni


-- optimalizálás: strictness annotáció / strict adattípusokat
--------------------------------------------------------------------------------

-- strict fa típus:
data StrictTree a = Leaf !a | Node !(StrictTree a) !(StrictTree a)

-- konstruktorok reprezentációja:
--                       64 bit             64 bit
--   Leaf 10 --->  | konstruktor tag |   (ptr 10-re)

-- Leaf-ben és Node-ban nem lehet thunk, csak konkrét érték
--    n :: Int (lehet thunk)
--    Leaf n
--       (Leaf létrehozása előtt force-oljuk n-t)

--    case t of
--      (Leaf n) -> _   -- tudjuk, hogy n nem lehet thunk


-- strict függvény paraméterek:  {-# language BangPatterns #-}

f2 :: Int -> Int -> Int
f2 !x !y = y                -- f2 hívásnál ! argumentumokat force-oljuk

-- -- foldl szigorú verziója
-- foldl' :: (b -> a -> b) -> b -> [a] -> b
-- foldl' f b []     = b
-- foldl' f b (a:as) = let !b' = f b a in foldl' f b' as

take' :: Int -> [a] -> [a]
take' n ((:) a !as)  | n > 0 = a : take' (n - 1) as   -- ADT-force-olás: legkülső konstruktorig értékel ki
take' _ !as = []


--------------------------------------------------------------------------------
-- Optimalizáláshoz hasznos: Core output

-- ghc -O2 -ddump-simpl -dsuppress-all -dno-suppress-type-signatures
-- (GHC Core language)
--    még alacsonyabb szintű: STG --> Cmm --> assembly / LLVM

-- lens / monád trans / template Haskell / adatszerkezetek (Set, Map, HashMap, stb)

-- monád transzformerek + adatszerkezetek
