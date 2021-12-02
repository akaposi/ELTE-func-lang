
-- (extra téma) Haskell programok fordítása

-- (következő EA: konkrét vizsgasor)
--    (  + 1 EA felvétel: még egy extra téma (szavazni, témát javasolni lehet) )

f1 :: Int -> Int
f1 x = x + 200

-- int64_t f1(int64_t x) {return x + 200;}

-- boxed vs. unboxed
-- alapból:
-- data Int = I# Int#       -- Int# : gépi 64 bites Int

-- 200 :: Int

-- I# 200#

--  I#    |  200#
--  64bit |  64bit

data Pair = Pair Int Int

data List a = Nil | Cons a (List a)

-- Pair x y

--  Pair    |   x   |    y
--  (tag)     1.ptr    2.ptr

-- Alapból: minden objektum egy ptr Haskell-ben.
-- GHC opt fordítás: Int "unboxing" --> sok ptr-t eltűntet
--   Hasonló: Java: alapból minden ptr, viszont optimalizálás eltűntet ezek közül sokat

f2 :: Pair -> Int
f2 (Pair x y) = x + 200


-- Első osztályú függvények + lustaság
------------------------------------------------------------

-- 1. első osztályú függvények reprezentálása

f3 :: Int -> (Int -> Int, Int -> Int)
f3 x = ((\y -> x + 10), (\y -> x * 10))

-- "closure" alkalmazása: függvényt ptr + valahány darab érték elmentve

--    closure
--      |
--    fv ptr | x1 | x2 | x3 | .... | xN

-- lambda kifejezésekhez létrehozunk *top-level* függvények

f3a :: Int -> Int -> Int
f3a y x = x + 10

f3b :: Int -> Int -> Int
f3b y x = x * 10

-- f3 10 kiértékelés:

--     (,) (closure f3a 10) (closure f3b 10)      (pár konstruktor)

-- closure függvény meghívása:

g :: (Int -> Int) -> Int -> Int
g f x = f (f (f x))

--  (fptr | x1 | x2 | ... xN)    alkalmazzuk   arg1, arg2, ... argN   értékekre

--     closure tárolja: belső függvény aritása + hány paraméter van tárolva
--        hívás esetén három eset lehetséges:
--           1. ha pont fptr aritása számú érték gyűlik össze: meghívjuk a függvényt
--              x1 .. xN arg1 .. argN  paraméterekkel
--           2. ha kevesebb érték gyűlik össze: új closure-t csinálunk, tárolja az új
--              argumentum értékeket is (parciális applikáció is így működik)
--           3. ha több érték gyűlt össze, mint a függvény aritása.
--              Példa: id (+20)
--              Meghívom a függvényt annyi értékkel, mint ami az aritása, és
--              utána rekurzívan újra hívom az eredményt a fennmaradó paraméterekkel

-- id :: a -> a
-- id x = x

-- g1 :: Int -> Int -> Int -> Int
-- g1 x y z = x + y

-- Closure hívás nem olyan hatékony, mint pl C függvény hívás.
-- Fordító: closure hívásokat megpróbálja "ismert" hívásra alakítani

-- ismert hívások:
-- - top-level függvényt meghívok annyi paraméterrel, ami a definícióban meg van adva

-- applyFuns :: [a -> a] -> a -> [a]
-- applyFuns fs a = map (\f -> f a) fs     -- minden függvényt alkalmazunk egy értékre


-- Python, Javascript, Java, C# (closure-ök kvázi ugyanúgy mint Haskell-ben)

-- Máshol az is működik, hogy a closure-ben tárolt értéket destruktívan mutáljuk:
--   function (x) {function (y) { x = x + 1; return x + y}}


-- Lustaság
--------------------------------------------------------------------------------

-- függvényhívás: csak akkor jön létre, ha az eredményt "force"-oljuk
--   force-olás: mintaillesztlésnél történik

-- "thunk" : késleltett hívás objektum

--     (fvptr | x1 | x2 | ... | xN)

-- thunk force-olása:   meghívjuk a fvptr-t az összes tárolt értékkel

-- let list = [ 1000 * 2000 , 3000 * 10 ]
--            [ thunk (*) 1000 200, thunk (*) 3000 10) ]

-- head list

-- *Minden* mintaillesztésnek van egy extra esete, amikor az érték thunk.
--     (minden mintaillesztés egy *loop*, ami addig értékeli a thunk-okat, amíg
--      adatkonstruktorba nem ütközünk)

-- case b of True -> _; False -> _; thunk fptr args -> _;

-- fordító : megpróbálja eltűntetni a felesleges thunk allokációt
--   optimalizáció *nem módosíthatja* a program viselkedését
--      (nem alakíthat át helyes programot végtelen loop-á)

-- Példa *explicit* lustaságra:
--    Idris:    (Lazy a)     *típus*, ami a lusta "a"-értékek típusa
--              default: szigorú kiértékelés van

--    GHC: default: minden lusta, bizonyos annotációval kérhetjük a szigorú kiértékelést
--                  (+ fordító próbál lustaságot eltűntetni)

-- "Filozófiai/elvi" háttér, hogy miért lusta alapból a GHC:

--    lusta nyelvben típusozatlan mellékhatás borzalom

-- szigorú + mellékhatásos nyelv:

--   let n = readLine(); let _ = print("foobar"); let y = f(); ....

--   let n = readLine; let x = print "foobar"; let y = f(); ....

--   a függvényt csak akkor hívom meg, ha force-olva van
--     --> programra ránézve fogalmunk sincs, hogy milyen sorrendben jönnek létre a hatások
--     --> rá vagyunk kényszerűlve, hogy erősen típusozott, strukturált mellékhatást
--         használjunk
--        (megoldások: Monádok, Monád transzformerek, Algebrai hatások, stb...)

--------------------------------------------------------------------------------

-- Fordítási pipeline GHC-ben:

--         Parsing             típus/scope ellőrzés            optimalizálások
-- Forrás    -->     Nyers AST         -->              Core        -->          Core    -->
--

--    -->   STG    -->     Cmm    -->    LLVM vagy assembly


-- Core nyelv: - *csak típushelyes kifejezések*
--             - sokkal egyszerűbb, kevesebb feature, mint a forrásnyelvben

-- Core nyelv: case kifejezés, lambda, konstruktorok, let, függvényalkalmazás

boolEq :: Bool -> Bool -> Bool
boolEq True True = True
boolEq False False = True
boolEq _ _ = False

-- Core nyelvben:
-- (pattern compilation)

-- boolEq :: Bool ->  Bool -> Bool
-- boolEq = \ x y -> case x of
--   True -> case y of
--     True -> True
--     _ -> False
--   False -> case y of
--     False -> True
--     _ -> False

-- Core output nyomtatása: -ddump-simpl -dsuppress-all -dno-suppress-type-signatures
--                         (fenti option-öket adjuk hozzá a "ghc FILE" híváshoz)
--                         (érdemes -O, -O1, -O2 option-öket is nézni)

-- STG : Core-hoz hasonló, viszont típusozatlan, lustaság/force-olás + memóriakiosztás
--         explicitebb

-- Cmm : magas szintű assembly

-- LLVM / assembly


--------------------------------------------------------------------------------

-- típusosztályok fordítása : rekordokra + függvényekre fordul
--   GHC megpróbálja a metódusokat inline-olni

--------------------------------------------------------------------------------
