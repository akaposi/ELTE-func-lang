
-- Típusosztályok, ADT-k (~ ismétlés), GHCI használat

-- Típusosztályok
--------------------------------------------------------------------------------

-- kérdés: hogyan implementáljunk egyelnőségvizsgálatot?

-- alaptípusokra egyszerű:
eqBool :: Bool -> Bool -> Bool
eqBool True  True  = True
eqBool False False = True
eqBool _     _     = False

-- listára?
-- az alábbi nem működik, mivel az "a" típusról nem tudunk semmit:
-- eqList :: [a] -> [a] -> Bool
-- eqList [] [] = True
-- eqList (x:xs) (y:ys) = x == y && eqList xs ys
-- eqList _ _ = False

-- kérdés: milyen "a" típusok vannak, amelyekre
-- nincsen (a -> a -> Bool) egyenlőségvizsgálat?
--    Int -> Bool    -- nem hatékony
--    [Bool] -> Bool -- lehetetlen, mert végtelen sok [Bool] érték van


eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList eqa []     []     = True
eqList eqa (x:xs) (y:ys) = eqa x y && eqList eqa xs ys
eqList _   _      _      = False

-- példa:
--   eqList eqBool [True, False] [False, False] == False


eqPair :: (a -> a -> Bool) -> (b -> b -> Bool)
          -> (a, b) -> (a, b) -> Bool
eqPair eqa eqb (a, b) (a', b') = eqa a a' && eqb b b'

-- példa:
-- eqList (eqPair (eqList eqBool) (eqList eqBool)) [([], [True])] [([], [True])] == True

-- Végtelen sok típusra nem akarjuk kézzel összrakni az egyenlőség-vizsgálatot.
-- Ez mechanikus feladat.
-- Típusosztály: kódgenerálás típusok struktúrája szerint.

-- névütközés elkerülésére: '

class Eq' a where      -- osztály deklaráció
  eq :: a -> a -> Bool -- osztály metódus
  -- osztály deklaráció: szeretnénk eq :: a -> a -> Bool-t
  -- többféle típusra használni (bizonyos "a"-kra)

-- implementáció valamely típusra:
instance Eq' Bool where
  eq True  True  = True
  eq False False = True
  eq _     _     = False
  -- eq = eqBool   -- hivatkozunk korábbi függvényre

-- feltételezzük (Eq' a) implementációt
instance Eq' a => Eq' [a] where
  eq [] [] = True
  eq (x:xs) (y:ys) = eq x y           &&    eq xs ys
               --    ^ eq "a" típusra       ^ rekurzív hívás
  eq _ _ = False

-- példák:
--   eq [True, False] [True, False]
--   ^ ez a kód ez lefordul arra, hogy:
--     eqList eqBool [True, False] [True, False]

-- (az Eq' [], Eq' Bool, Eq' (a, b) instance-ok, ugyanarra a kódra fordulnak,
--  mint amit korábban eqList, eqBool, eqPair néven írtunk)


instance (Eq' a, Eq' b) => Eq' (a, b) where
  eq (a, b) (a', b') = eq a a' && eq b b'

-- standard osztály:

--   class Eq a
--     (==) :: a -> a -> Bool

-- Eq derive-olható, tehát nem érdemes kézzel írni

-- data MyTree a = MyLeaf a | MyNode (MyTree a) (MyTree a)
--   deriving Eq

-- derive-olható dolgok:
--   Eq    -- egyenlőség
--   Show  -- String-re alakítás
--   Ord   -- rendezés

-- class Show a where
--   show :: a -> String

-- (csak Show instance-os típust tud GHCI kinyomtatni)

-- class Ord a where
--   (<) (<=)  stb...


-- GHCI-ről
--------------------------------------------------------------------------------

-- alap parancsok:
--  :t <kifejezés>      -- típus lekérdezés
--  :i <név>            -- információ névről
--     :i működik: típus, típusosztály, definíció, operátor

--     legkritikusabb: operátor keresése
--        pl: :i (+)  (kiderül: típus, definíció helye,
--                     precedencia)

-- type hole-ok:
--   írunk _-t egy kifejezésben, akkor
--   megkapjuk, hogy milyen típusú kifejezést kéne
--   az _ helyére írni


-- példa:
comp :: (b -> c) -> (a -> b) -> a -> c
comp =   \f g a -> f (g a)
  -- lépések (játsszuk vissza ghci-ben)
  -- _
  -- \f g a -> _
  -- \f g a -> f _
  -- \f g a -> f (g _)
  -- \f g a -> f (g a)

-- bizonyos esetben a type hole nem segít sokat, példa:

-- f :: Bool -> Bool
-- f x = _


-- ADT-k
--------------------------------------------------------------------------------

-- ajánlott:
--   átnézés: lambda.inf.elte.hu: Haladó Haskell/típusdefiníciók

-- feladat:
-- definiáljuk újra data-val a standard lista típust

-- két konstruktor: üres lista, kiegészített lista
-- standard lista: []      (:)
data List a =      Empty | Extend a (List a)

-- példák:
l1 = Empty                      -- []          :: [a]
l2 = Extend True Empty          -- True : []   :: [Bool]
l3 = Extend 0 (Extend 10 Empty) -- 0 : 10 : [] :: [Int]

-- standard lista függvény:
-- f :: [a] -> ...
-- f [] = ...
-- f (x:xs) = ...

-- saját lista függvény:
-- f :: List a -> ...
-- f Empty         = ...
-- f (Extend x xs) = ...
