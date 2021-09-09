
-- jegyzetek: https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2021-22-1

-- konz: 2.620 terem + discord szerda 15:00-16:00

-- követelmény: gyak eleji kis feladatok (0-2 pont / feladat)
--              min 13 pont: jelentkezhet vizsgára
--              házi feladatok: 3 x 4 point

-- vizsga: adja a jegyet, 2 óra gépes feladatmegoldás

------------------------------------------------------------

-- Áttenkintés:

--   tárgy: "közepes Haskell"

-- "tisztán" funkcionális nyelv
--    - van mellékhatás a Haskell-ben
--    - kivételek, mutáció, párhuzamosság, I/O műveletek, stb..
--    - Típusos mellékhatások.
--      - tiszta függvény: Int -> Int
--      -                  Int -> IO Int       (függvény, ami IO művelet hajthat)
--                         Int -> Except e Int  (kivételt dobhat)
--    - Első osztályú fogalom a mellékhatás
--      - saját, custom mellékhatásokat tudunk definiálni
--      - pl: IO műveletek listája
--        (dinamikusan létrehozzuk a műveleteket, elkülönül a végrehajtástól)

--   Mainstream imperatív nyelv

-- Haskell-t miért érdemes tanulni:
--  - 1. programot szeretnénk írni: Facebook spam-szűrés Haskell-ben
--        (lásd: Haxl library)
--       - compilerek : GHC, Elm, PureScript, Agda
--  - 2. ízelítő: típuselmélet, kategóriaelmélet, formális szemantika
--       (ELTE-n bizonyító rendszer: Agda, Coq)
---      (már a Haskell-ben is lehet tételeket formalizálni!)

------------------------------------------------------------

-- Típusosztályok:

-- egyenlőséget vizsgálunk:

-- eq :: a -> a -> Bool
-- eq = ?

eqBool :: Bool -> Bool -> Bool
eqBool True True = True
eqBool False False = True
eqBool _ _ = False

eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList eqa []     []     = True
eqList eqa (x:xs) (y:ys) = eqa x y && eqList eqa xs ys
eqList _   _      _      = False

eqPair :: (a -> a -> Bool) -> (b -> b -> Bool) -> (a, b) -> (a, b) -> Bool
eqPair eqa eqb (a, b) (a', b') = eqa a a' && eqb b b'

-- végetelen sok típus:
--    Bool, [Bool], [[Bool]], [[[Bool]]], ([Bool], Bool), ....

eqBoolPair :: (Bool, Bool) -> (Bool, Bool) -> Bool
eqBoolPair = eqPair eqBool eqBool


eqBoolPairPair :: ((Bool, Bool), (Bool, Bool))
               -> ((Bool, Bool), (Bool, Bool))
               -> Bool
eqBoolPairPair = eqPair eqBoolPair eqBoolPair  --

-- típusosztály: automatikus kódgenerálás, típusok struktúrája szerint

-- 1. deklarálok osztály

-- standard megfelelő: Eq
class Eq' a where            -- "a" osztály paraméter
  eq :: a -> a -> Bool       -- metódus (akárhány lehet belőle)

instance Eq' Bool where
  eq = eqBool

-- instance constraint
instance (Eq' a) => Eq' [a] where
  eq :: [a] -> [a] -> Bool
  eq []     []     = True
  eq (x:xs) (y:ys) = eq x y && eq xs ys
  eq _      _      = False

  -- alternatív definíció
  -- eq = eqList eq -- "eq" : elemek egyenlősége


-- több megszorítás: vesszővel elválasztva
instance (Eq' a , Eq' b) => Eq' (a, b) where
  eq (a, b) (a', b') = eq a a' && eq b b'


eqBoolPairPair' :: ((Bool, Bool), (Bool, Bool))
               -> ((Bool, Bool), (Bool, Bool))
               -> Bool
eqBoolPairPair' = eqPair eqBoolPair eqBoolPair

-- Alapvető osztályok:

-- class Eq a where
--   (==) :: a -> a -> Bool

-- standard: Ord
class Eq' a => Ord' a where        -- (Eq' a): superclass megszorítás
  lt :: a -> a -> Bool             -- "less than"

-- superclass megszorítás:
---  csak olyan Ord' a instance-t adhatunk meg, amire már
--   van Eq' instance

-- instance Ord' Bool where  -- OK

-- instance Ord' Int where   -- Nem OK, nincs Eq' Int instance

-- függvény megszorítással:
f1 :: Eq' a => a -> a -> a -> a
f1 x y z = if eq x y then x else z

-- superclass: "Ord' a"-ból következik a "Eq' a"
f2 :: Ord' a => a -> a -> Bool
f2 x y = eq x y

-- speciális: ghci-ben csak Show értékeket lehet kinyomtatni
-- class Show a where
--   show :: a -> String

-- ADT (algebrai adattípusok)
------------------------------------------------------------

  -- típus konstruktor     n-darab érték konstruktor
data Color               =  Red | Green | Blue
      deriving (Show, Eq, Ord)

-- Color: enum: n-darab lehetéges érték

c1 :: Color
c1 = Red

-- két dolog amit kapok egy "data" deklarációból:
--  - megkapom a konstruktorokat       -- létrehozás
--  - mintaillesztés konstruktorokon   -- felhasználás

f3 :: Color -> Int
f3 c = case c of
  Red -> 100
  _   -> 200

-- lista definíció mint ADT

--   data típus konstruktor  n-darab típusparaméter  = konstruktorok
data List a = Nil | Cons a (List a)   -- rekurzív ADT!
  deriving (Eq, Show, Ord)

-- egyszeresen láncolt lista típus

l1 :: List Int
l1 = Nil

l2 :: List Int
l2 = Cons 10 Nil

l3 :: List Bool
l3 = Cons False (Cons True Nil)

-- mintaillesztés:

-- (standard név: map)
map' :: (a -> b) -> List a -> List b
map' f Nil         = Nil
map' f (Cons a as) = Cons (f a) (map' f as)

-- map :: (a -> b) -> List a -> List b
-- map f []       = []
-- map f (a : as) = (f a) : (map' f as)  -- csak szintaktikus cukor a (:)!

-- példa fa struktúrára:

-- bináris leveles fa, "a" értékkel a levélben
data Tree a =
    Leaf a                 -- 1 darab "a" típusú adatmező
  | Node (Tree a) (Tree a) -- 2 darab részfa

t1 :: Tree Int
t1 = Leaf 20

t2 :: Tree Int
t2 =
  Node
   (Node
     (Leaf 10)
     (Leaf 20))
   (Node
     (Leaf 30)
     (Leaf 40))

t3 :: Tree Int
t3 =
  Node
   (Node
     (Leaf 10)
     (Leaf 20))
   (Leaf 30)

-- extra házi feladat: definiáljuk a *teljes* bináris leveles fák ADT-jét!

------------------------------------------------------------
-- Miért "algebrai" típus?

-- algebrai képlet megadja, hogy hány lehetséges értéke van egy típusnak

-- 1-elemű típus
data One = One   -- típuskonstruktor: One
                 -- értékkkonstruktor: szintén One névvel

-- érték nélküli típus
data Zero

-- típusok szorzata

-- lehetésges értékek száma: |a|

-- |Mul a b| = |a| * |b|

data Mul a b = Pair a b    -- lásd: Descartes szorzat

-- |Mul Bool Bool| = 4

-- |Plus a b| = |a| + |b|    -- standard: Either
data Plus a b = Inl a | Inr b

p1 :: Plus Bool Int
p1 = Inl True

p2 :: Plus Bool Int
p2 = Inr 300

-- aritmetika típusokkal:

-- a + 0 = a
--   |Plus a Zero| = |a| ?   OK
-- Plus Bool Zero

-- (a + b) + c = a + (b + c)

-- "beláthatók" az azonosságok, ha írunk oda-vissza függvényeket

-- a * (b * c) = (a * b) * c

to :: Mul a (Mul b c) -> Mul (Mul a b) c
to (Pair a (Pair b c)) = Pair (Pair a b) c

-- from :: Mul (Mul a b) c -> Mul a (Mul b c)
-- from = ?

-- extra házi:
--  disztributivitás:
--   a * (b + c) = a * b + a * c
