{-# language BangPatterns #-}

module Notes12 where

-- Haskell optimalizáció
------------------------------------------------------------

-- Röviden adat-reprezentáció + fordítás (függvények + lustaság)

f :: Int -> Int
f x = x + 10

-- Core: GHC belső minimalista AST-je
--   optimalizáció után kinyomtatni a Core-ját

{-
GHC pipeline:
   forrás                  (parsing)
   nyers AST               (scope checking)
   nyers AST(well-scoped)  (desugaring)
   desugared AST           (type checking)
   Core                    (több opt pass Core-on)
   ...
   Core
   STG (típusozatlan lambda kalkulus)
   Cmm
   LLVM vagy asm

Kérdés: mit kell ebből ismerni
a hatékony Haskell programozáshoz?

- vélemények:
  - semmit, csak benchmarkoljunk
  - Core-t  (András: ez a tuti)
  - (extrém: STG)
-}

-- Int:
--  data Int = I# Int#
--
-- Int# : gépi 64-bites Int

-- I#
-- |
-- tag | 64-bit Int#

-- Általánosan ADT konstruktorok
--    Első mező: metaadat (64-bit)
--    Többi mező: tényleges mezők

-- Dobozolás nem jó hatékonyság szempontjából
-- miért:
--   Haskell: polimorfizmus "type erasure" megoldás (mint Java)
--   (másik: monomorfizáció, új kódot generálunk polimorf függvény-
--           alkalmazásokhoz)

-- data Foo = Foo (forall a. a -> a)
-- case x of Foo f -> ...

-- GHC: "unboxing":
--    - létrehozza a függvényeknek egy unboxed verzióját
--    - ezt kiexportálja a modulból, minél inkább ezt próbálja használni
--    rendszer neve: "worker-wrapper"

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

-- foo :: (Int, Int) -> Int
-- foo (x, y) = fact x * y   -- nem-rekurzív kicsi függvény, mindig inline

-- minden rekurzív hívásnál ki-be dobozolást csinál
fact' :: (Int, Int) -> Int
fact' (x, 0) = x
fact' (x, n) = n * fact' (x, n - 1)

-- "thunk": lusta érték

-- szigorú paraméter: BangPatterns
fact'' :: (Int, Int) -> Int
fact'' (!x, 0) = x    -- ezen a ponton "x" ki van értékelve
fact'' ( x, n) = n * fact'' (x, n - 1)

------------------------------------------------------------

-- Lustaságot érdemes minimalizálni
--   - lusta értékek nem lehetnek primitív unboxed értékek
-- Unboxing maximalizálni

-- Másik dolog, amit minimalizálni szeretnénk:
--   closure ("függvény objektum")

-- Haskell-ben függvényhívásból
--   két különböző kód tud generálódni
--     - 1. "ismert hívás"
--            foo'' (x, y) forráskódban, ismert "foo''" kódja
--            - hívhatjuk az unbox-olt verziót
--            - látjuk, hogy hány paramétere van a függvénynek
--              (parciális applikáció!)
--       ismert hívás, pont megfelelő mennyiségű paraméter:
--         nincs overhead

--    - 2. Closure-t hívok:
--        extra indirekció + paraméterek számát is futásidőben
--        kell vizsgálni

-- Egyszerű példa closure-re

mkFunList :: Int -> [Int -> Int]
mkFunList x =
  [\y -> y + x + x,
   \y -> y * x * x]

  -- futásidőben a két lambda:
  --    struct, tárolja magát a kód pointert + "x" változó értékét
  -- általánosan: closure tárolja a kódot + csak dinamikusan ismert
  --              értéket, amire a kód hivatkozik

-- (tudok olyat csinálni, hogy több értékre alkamazok egy függvényt,
--  mint ahány lambdát írtam a definícióban?
--  klasszikus példa: id (+) 10 20   (id = \x -> x)

-- Closure-ök eliminálása kódból: elsősorban inlining segítségével
--   példa: lista map függvény
--   optimalizáció nélkül:


-- map' :: (a -> b) -> [a] -> [b]
-- map' f [] = []
-- map' f (a:as) = f a : map' f as

-- bar :: [Int] -> [Int]
-- bar = map' (+100)

map' :: (a -> b) -> [a] -> [b]
map' f = go where
  go [] = []
  go (a:as) = f a : go as
{-# inline map' #-}

bar :: [Int] -> [Int]
bar = map' (+100)

------------------------------------------------------------
-- Haskell performance probléma: "space leak"
--   lusta értékeken keresztül memóriában élve marad az is,
--   aminek nem kéne

--   let x = delay (myArray !! i)
--   ...
--  case force x of
--     ... -> _


-- lusta kiértékelés egy csomó adatfüggőséget bevezet "láthatatlan"
--   módon

-- Idris nyelvben:
--   lustaság explicit típusokban: Lazy a
--   (én ezt preferálom)
