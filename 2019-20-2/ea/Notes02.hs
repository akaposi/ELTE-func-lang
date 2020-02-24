
{-# language TypeApplications, RankNTypes, ScopedTypeVariables #-}

import Prelude hiding (id, const)

-- ADT-k folytatás, polimorf függvények, logika
------------------------------------------------------------

-- 1 elemű típus
data One = One

-- 0 elemű típus
data Zero

-- (+)         std: Either
data Sum a b = Inj1 a | Inj2 b

-- (*)
data Prod a b = Prod a b

-- Ha "a" típus, akkor "|a|" a lehetséges értékek száma

-- |Sum a Zero| = |a|
-- |Prod a One| = |a|

-- hatványozás?
-- feltéve: a és b típusok
-- Hogyan definiáljuk a^b típust, amire igaz, hogy
--   |a^b| = |a|^|b|

type Exp a b = b -> a

-- Példa: 4 darab lehetséges (Bool -> Bool) függvény
-- 4 = 2^2
f1, f2, f3, f4 :: Bool -> Bool
f1 = \_ -> True
f2 = \_ ->  False
f3 = id
f4 = not


-- |Int| 64 bites egész szám, 2^64 lehetséges értéke van.
-- példa: Bool -> Int
-- |Bool -> Int| = |Int|^2 = |Int|*|Int|

-- (Bool -> Int) ugyanannyi adatot tartalmaz, mint (Int, Int)
-- Mivel True-hoz, és False-hoz is kell egy Int-et rendelni.

-- demostráló bijekció:
to   :: (Bool -> Int) -> (Int, Int)
to f = (f True, f False)

from :: (Int, Int) -> (Bool -> Int)
from (n1, n2) = \b -> if b then n1 else n2

-- Általában: (Bool -> x) ekvivalens azzal, hogy (x, x)

data Three = T1 | T2 | T3

-- (Three -> x) ekvivalens (x, x, x) -el

-- (a -> b) ekvivalens (b, b, b, b, b, ....) -vel
--        b szorzata önmagával |a|-szor

-- Ez lényegében a hatványozás: b^a


-- Emlékezzünk a curry/uncurry függvényre: ezek
-- megadnak egy algebrai azonosságot: c^(a*b) = (c^b)^a
-- |(a, b) -> c| = |a -> (b -> c)|
-- curry   :: ((a, b) -> c) -> (a -> b -> c)
-- uncurry :: (a -> b -> c) -> ((a, b) -> c)


-- További azonosság:
-- |(Sum a b -> c)| = |((a -> c), (b -> c))|
-- c^(a+b) = (c^a) * (c^b)

to2 :: (Sum a b -> c) -> ((a -> c), (b -> c))
to2 f = (\a -> f (Inj1 a), \b -> f (Inj2 b))

from2 :: ((a -> c), (b -> c)) -> Sum a b -> c
from2 (f, g) (Inj1 a) = f a
from2 (f, g) (Inj2 b) = g b


-- Polimorf függvények
------------------------------------------------------------

-- map :: (a -> b) -> [a] -> [b]

-- egyszerű polimorf függvények

-- (std: id)
-- id :: a -> a
-- id x = x

-- (std: const)
-- const :: a -> b -> a
-- const x y = x

-- kompozíció:
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- (f . g) x = f (g x)

-- ha nem tudjuk valaminek a típusát, akkor meg lehet kérdezni :t-vel
-- :t comp
comp f g x = f (g x)

-- ($)
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

-- zárójelek elkerülésére hasznos:
--    not $ not $ not $ not True

-- Példa: olyan felhasználás ($)-hoz, ami nem zárójelek eltüntetése:

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- példa: zipWith (+) [0, 1, 2] [0, 1, 2] == [0, 2, 4]

-- zipWith ($) [(+10), (+20), (+30)] [0, 1, 2] == [10,21,32]


id :: forall a. a -> a           -- típusváltozó explicit bevezetése
id x = x

f5 :: Bool -> Bool     -- expliciten alkalmazzunk egy típus paramétert
f5 = id @Bool          -- emlékezzünk C++ template-re: id<bool>(x)

f6 :: Int -> Int
f6 = id @Int

-- összesen 4 darab paraméter, 2 típus és 2 érték
const :: forall a b. a -> b -> a
const x y = x

f7 :: Bool -> Int -> Bool
f7 = const @Bool @Int

-- megcserélem a típusparaméterek sorrendjét
const' :: forall b a. a -> b -> a
const' x y = x

f8 :: Int -> Bool -> Int
f8 = const' @Bool @Int


-- id True        -- rejtett paraméter: "a" típus
-- id @Bool True  -- belső reprezentáció

-- C++-ban generikus id függvény:
--   id<bool>(true)

-- elég ritka egyszerű Haskell kódban, hogy nem lehet @a applikációt
-- kikövetkeztetni.
-- Ha elég egyszerű Haskell-ben dolgozunk, akkor tétel: mindig minden
-- típus kikövetkeztethető (Hindley-Milner kikövetkeztetés).


-- Higher-rank polimorfizmus:
f9 :: (forall a. a -> a) -> (Bool, Int)
f9 f = (f True, f 0)

f10 :: (Bool, Int)
f10 = f9 id

-- Tétel:
--    - ha max. 2 mély forall egymásba ágyazás van, akkor
--      még létezik kikövetkeztető algoritmus.
--
--    - 3 vagy nagyobb mélység esetén eldönthetetlen probléma


-- Típusok logikája
--------------------------------------------------

-- típuselmélet nevű MSc-s tárgyban központi téma

-- még egy mód, hogy hogyan lehet típusokra gondolni:
--   típus ~ logikai állítás

-- forall a. a -> a     ~    ∀ a. a → a
-- (minden állításra: állításból következik ugyanaz az állítás)

-- modus ponens: függvényalkalmazás
-- ($) :: forall a b. (a -> b) -> a -> b   ~  ∀ a b. ((a → b) ∧ a) → b

-- minden típus ~ logikai állítás
-- minden érték ~ állítás bizonyítása

id' :: forall a. a -> a
id' = \x -> x

modusponens :: forall a b. ((a -> b), a) -> b
modusponens (f, a) = f a

-- MSc-s típuselmélet + formális szemantika tárgyaknál
-- minden számítógépes formális bizonyítás így fog működni.

--    állítások    típusok            típusalgebra
--    a ∧ b        (a, b)             a * b
--    a ∨ b        Either a b         a + b
--    a → b        a -> b             b^a
--    ⊤            One                1
--    ⊥            Zero               0
--    ∀ a. b       forall a. b        az összes specializáció szorzata
--                                    (Π(i∈A). Bᵢ)
--    ∃ a. b       NINCS Haskell-ben  (indexált összeg)


-- Példa nem triviális állítás biznyítására:
-- ∀ a b c. (a ∨ b → c) → (a → c) ∧ (b → c)
to2' :: (Sum a b -> c) -> ((a -> c), (b -> c))
to2' = \f -> (\a -> f (Inj1 a), \b -> f (Inj2 b))

-- ∀ a b c. (a → c) ∧ (b → c) → (a ∨ b → c)
from2' :: ((a -> c), (b -> c)) -> Sum a b -> c
from2' (f, g) (Inj1 a) = f a
from2' (f, g) (Inj2 b) = g b

-- tanulság:
--   85%-ban gondolkodás nélkül lehet definiálni

-- Milyen típus hamis mint logikai állítás? Ha nincs teljes program
-- azzal a típussal.
-- (pl: One -> Zero)
