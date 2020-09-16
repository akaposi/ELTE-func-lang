
-- Info: https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2020-21-1
--      + gyakorlófeladatok, pl előző félév gyak_1-ből, minta vizsgák

-- Ajánlott ismétlés:
--    http://lambda.inf.elte.hu/   "Kezdő Haskell" (listák, mintaillesztés, magasabbredű függvény, függvények)

-- A Haladó Haskell

------------------------------------------------------------

-- Miért érdemes Haskell-t tanulni?
--   - Kapu témák felé: logika, típusrendszerek elmélete, típuselmélet, kategóriaelmélet,
--                      (mindegyik kötődik a szoftvertechnológiához)
--
--   - Gyakorlatban használni.
--       Példa ipari használatra:
--           - Facebook (spam szűrő logika),
--           - pénzügyi világ, bankok (pl Standard Chartered)
--           - blockchain/kripto rendszerek (pl Cardano)
--       Compiler-ek:
--           - GHC, Agda, Idris, Elm, PureScript


-- Osztályok
--------------------------------------------------------------------------------

-- egyenlőség-vizsgálat többféle típuson
-- C++-ban ad-hoc overloading: ugyanazon a néven több függvény definiálása

-- példa: osztályok nélkül
eqBool :: Bool -> Bool -> Bool
eqBool True True   = True
eqBool False False = True
eqBool _     _     = False

-- lista egyenlősége függ a listaelem egyenlőségétől
eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList eqa []     []     = True
eqList eqa (x:xs) (y:ys) = eqa x y && eqList eqa xs ys
eqList eqa _      _      = False

-- párok egyenlősége:
eqPair :: (a -> a -> Bool) -> (b -> b -> Bool) -> (a, b) -> (a, b) -> Bool
eqPair eqa eqb (x, y) (x', y') = eqa x x' && eqb y y'

-- (Bool, Bool) egyenlősége
eqBoolBool :: (Bool, Bool) -> (Bool, Bool) -> Bool
eqBoolBool = eqPair eqBool eqBool       -- parciális applikáció!
-- alternatív: eqBoolBool bb bb' = eqPair eqBool eqBool bb bb'

eqListBool :: [Bool] -> [Bool] -> Bool
eqListBool = eqList eqBool

f1 :: [[[Bool]]] -> [[[Bool]]] -> Bool
f1 = eqList (eqList (eqList eqBool))

f2 :: [(Bool, [Bool])] -> [(Bool, [Bool])] -> Bool
f2 = eqList (eqPair eqBool (eqList eqBool))
-- (f1, f2: ki lehet próbálni)

-- Osztállyal: tetszőleges lista/pár/Bool típus egyenlőségre ugyanazt a
-- függvénynevet lehet használni. Az implementáció automatikusan
-- generálódik a típus alapján.

-- 1. deklaráció:
-- (standard: Eq, saját Eq', hogy ne ütközzön)
class Eq' a where          -- Eq' : osztály neve     a : osztály paraméter
  eq :: a -> a -> Bool     -- osztály metódus (lehet 1 vagy több metódus)


-- instance definiálása:
instance Eq' Bool where
  -- eq :: Bool -> Bool -> Bool
  eq True  True  = True
  eq False False = True
  eq _     _     = False
  -- eq = eqBool

-- (Eq' a) =>  : instance constraint (megszorítás)
instance Eq' a => Eq' [a] where
  -- eq :: [a] -> [a] -> Bool
  eq []     []     = True
  eq (x:xs) (y:ys) = eq x y && eq xs ys      -- mivel tudjuk (constraint-ből), hogy van (Eq' a), ezért
  eq _      _      = False                   -- használhatjuk eq-t "a" típusú paraméterekkel
                                             -- eq x y   : lista elemre alkalmazva
                                             -- eq xs ys : listára alkalmazva

-- eq [True, True] []  == False
-- eq [[[True]]] [[[True]] == True

-- Két instance-ból származik végtelen sok túlterhelés: Bool, [Bool], [[Bool]],
-- [[[Bool]]], stb...  Különbség típusosztály és Java/C# interface között: az
-- utóbbiban nem lehet tetszőlegesen sok implementációt generálni. A
-- Haskell/Rust/Swift jellegű típusosztály viszont véges sok szabállyal tetszőlegesen
-- sok típusra generál implementációt, szükség szerint.

-- két megszorítás (zárójelben, vesszővel elválasztva)
instance (Eq' a, Eq' b) => Eq' (a, b) where
  eq (x, y) (x', y') = eq x x' && eq y y'


-- ADT (algebrai adattípusok)
--------------------------------------------------------------------------------

-- BSc-funckprog: Maybe típus, enum típusok, fák, Either
-- általános megoldás új adattípusok definiálására: ADT

-- data <típus neve> = <konstruktorok felsorolása>

-- Bool újradefiniálása:
-- data Bool = True | False

-- típus egy konstruktorral (enum egy értékkel)
data One = One
-- típus "One" névvel
-- érték "One" névvel

one :: One  -- "One" típus
one = One   -- "One" konstruktor

-- f :: Bool -> Int
-- f True = 10
-- f False = 200

fone :: One -> Int   -- csak egy lehetőség van, tehát f helyett a "200" értéket is használhatnám
fone One = 200

-- érték nélküli típus (nincs konstruktora)
data Zero
-- enum: n-konstruktor, n lehetséges érték


-- típus szorzása (paraméteres adattípus)
-- pár típus!
-- (a, b) pár típus, (e1, e2) pár érték
data Prod a b = Prod a b

-- miért szorzás?
-- Prod a b lehetséges értékek száma = a értékek száma * b értékek száma
-- rövidítés: |Prod a b| = |a| * |b|       (Descartes szorzat: párok halmaza)
-- |Prod Bool Bool| = 2 * 2 = 4
-- |Prod Bool One| = 2 * 1 = 2 = |Bool|

-- One egységelem Prod-ra
-- |Prod a One| = |a|
-- |Prod One a| = |a|

-- Zero zéruselem Prod-ra
-- |Prod a Zero| = 0


-- típus összeadása
-- standard megfelelő: data Either a b = Left a | Right b
data Plus a b = Inl a | Inr b     -- ("in-left", "in-right")

-- példák
plus1 :: Plus Int Bool
plus1 = Inl 100

plus2 :: Plus Int Bool
plus2 = Inr True

-- konstruktor: értéket létrehoz
-- mintailesztés: értékek feldogol/megvizsgál
plusFun :: Plus Int Bool -> Int
plusFun (Inl n) = n + 100
plusFun (Inr b) = if b then 200 else 300

-- |Plus a b| = |a| + |b|
-- |Plus Bool Int| = |Bool| + |Int| = 2 + 2^64
-- |Plus a Zero| = |a|

-- kommutativitás:
-- |Plus a b| = |Plus b a|
-- |Prod a b| = |Prod b a|

plusComm :: Plus a b -> Plus b a
plusComm (Inl a) = Inr a
plusComm (Inr b) = Inl b

-- disztribúció:
-- a*(b+c) = a*b + a*c

-- (egyfajta típus-refaktorálás)
-- |Plus (Prod a b) (Prod a c)| = |Prod a (Plus b c)|
-- (opcionális házi feladat)
distrib1 :: Prod a (Plus b c) -> Plus (Prod a b) (Prod a c)
distrib1 = undefined

distrib2 :: Plus (Prod a b) (Prod a c) -> Prod a (Plus b c)
distrib2 = undefined

-- Hatványozás:
-- |Exp a b| = |a|^|b|
data Exp a b = Exp (b -> a)

-- (Bool -> a)  értelmezhetem úgy, mint  (a, a)

to :: (Bool -> a) -> (a, a)
to g = (g True, g False)

from :: (a, a) -> (Bool -> a)
from (x, y) True  = x
from (x, y) False = y

-- |Bool -> a| = |a|*|a|
-- |Four -> a| = |a|*|a|*|a|*|a|
-- |b    -> a| = |a|^|b|

-- |(a, b) -> c| = |a -> (b -> c)|
-- c^(a*b) = (c^b)^a
-- BSc-n: curry-zés
-- curry oda
-- uncurry vissza
