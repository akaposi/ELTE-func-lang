{-# LANGUAGE TypeFamilies #-}
-- ^ LANGUAGE az nyelvi funkciókat kapcsol ki és be

import Data.Kind
-- Haskell
-- Lusta
-- Funkcionális
-- Deklaratív
-- Erősen Statikusan típusozott

-- Lusta kiértékelés

--       V [1,2,3,4,...]
-- head [1..]   - ez lustán működik
-- Ez teljesen jól működik

-- sum [1..] * 0 -- ez viszont miért nem működik?
-- length [1..] > 0 -- ez se működik?

-- Hogyan vannak a számok és a listák definiálva?

-- Algebrai adatszerkezetek
-- :i, :info

-- én most típust definiálok
-- V
-- data List a = [] | a : [a]
---     ^ típus neve
-- Láncolt lista


-- Az magában, hogy "List" az nem egy típus
-- Az hogy "List Int" viszont már igen


-- Számok hogyan vannak definiálva
-- data Int = I# Int#
-- data Int#
-- Csak deklarálva van
-- Elképzelve 32 db 1es vagy 0 egymást után

f :: Int -> Int
f 1 = 1
-- Mi történik itt?

-- A mintaillesztés számokra az == vizsgálatra fordul
-- f x = if x == 1 then 1 else undefined

-- A bit szerinti egyenlőség vizsgálat pl C-ben megírva az nem lusta

-- Ez az első probléma


-- Haskellnek a kiértékelési szemantikája


-- head [1..]
-- Ez hogyan van kiértékelve lépésenként?

myHead :: [a] -> a
myHead (x:xs) = x

-- 1 : 2 : 3 : _


--- myHead (1 : 2 : 3 : ...)


-- 1. Első ötlet
-- Értékeljük ki a paramétereket először
-- (1 : 2 : 3 : ....)


-- Kézenfekvőbb példa
-- head [1, undefined]
-- [1, undefined]
-- undefinedot ekkor ki kéne értékelni, ami hibába vezetne
-- Strict kiértékelés / Call-by-value

-- 1.5 ötlet
-- Call-by-reference

-- 2. Második ötlet
-- Call-by-name

-- myHead (1 : 2 : 3 : ...)
-- (\(x:xs) -> x) (1 : 2 : 3 : ...)
-- [x = 1; xs = 2 : 3 : ...]
-- 1

-- Lusta kiértékelés így működik
-- sum [1..] * 0 ; length [1..] > 0
-- Miért nem lusta?

-- Mert a számtípusok nem lusták és a függvénydefiníciók nem használják ki a CbN szemantika lustaságát


-- length [1..] > 0
-- ^ 1 + (1 + (1 + (1 + ....))) > 0

data CustomInteger = PlusOne CustomInteger | Zero
-- 0 = Zero
-- 1 = PlusOne Zero
-- 2 = PlusOne (PlusOne Zero)
--     |_______________|

customLength :: [a] -> CustomInteger
customLength [] = Zero
customLength (x : xs) = PlusOne (customLength xs)

lessThanEq :: CustomInteger -> CustomInteger -> Bool
lessThanEq Zero _ = True
lessThanEq (PlusOne x) (PlusOne y) = lessThanEq x y -- x + 1 <= y + 1 <=> x <= y
lessThanEq (PlusOne _) Zero = False -- x + 1 <= 0


-- Sima számokat lustaságra ne használjunk

-- behelyettesítjük / redukáljuk az f-nek a definícióját
-- f x y z a b ...

-- Mit jelentett az hogy a haskell Funkcionális?
-- Mi NEM függvény?
-- A típus függvény*

h :: Int
h = 0 -- ez egy függvény
-- sőt, 0 is egy függvény, szintén 0 aritású

-- a where az nem egy függvény, a where az egy nyelvi konstrukció
-- let-in nem függvény
-- case-of nem függvény
-- :: szintén nem függvény
-- = se függvény
-- -> legyen függvény
-- => nem függvény, de lehetne!!!
-- Eq a => a -> a -> Bool

-- Int -> Int

-- Int :: Végtelen spirál???
-- :k, :kind
-- :seti -XNoStarIsType
-- :k Int :: Type
-- :k Type :: Type
-- :k (->) :: Type -> Type -> Type

-- :k Maybe :: Type -> Type
-- :k Either :: Type -> Type -> Type
-- :k List :: Type -> Type

idType :: Type -> Type
idType x = x

-- Típuscsalád

type IdType :: Type -> Type
type family IdType x where
  IdType x = x

id2 :: a -> a
id2 x = x

-- f :: Type -> Type
id3 :: f Int -> f Int
id3 x = x

-- Valami olyat keresünk, hogy (Type -> Type) -> Constraint
class EnKedvencTipusosztalyom f where
  doSomethingCool :: f Int -> f Bool

instance EnKedvencTipusosztalyom Maybe where
  doSomethingCool (Just n) = Just (even n)
  doSomethingCool Nothing = Nothing

-- instance Show (IdType Int)


-- Erősen típusozott
-- Statikusan típusozott - Fordítási időben tudjuk a típusokat

i = [1, 2, id 3]

-- i :: ?a
-- [1, 2, id 3] :: [?b]
-- ?a == [?b] (Unifikáció)
-- 1, 2, id 3 :: ?c
-- ?b == ?c
-- 1 :: Integer
-- 2 :: Integer
-- id 3 :: ?c
-- ?c == Integer
-- id 3 :: Integer
-- ^ megpróbáljuk kitalálni mi a típusa
-- összevetjük a paraméter típusával

--- f a :: B
--- f ==> A -> B
--- a :: A

-- id 3 :: Integer
-- id ===> ?d -> Integer
-- ?d == Integer
-- id == Integer -> Integer
-- 3 :: Integer



-- "alma" + 3 :: ?a
-- ((+) "alma") 3 :: ?a
-- (+) "alma" ===> ?b
-- (+) ===> Int -> (Int -> Int)
-- "alma" :: Int
-- Ez viszont típushiba


-- 0 :: Num a => a
-- 0 :: Dict (Num a) -> a

-- 1 + 2
-- 1 + 2 :: ?a
-- ((+) 1) 2 :: ?a
-- (+) 1 ==>
-- (+) ==> Num ?b => ?b -> ?b -> ?b
-- 1 :: ?b
-- ((+) 1) :: Num ?b => ?b -> ?b
-- 2 :: ?b
-- 1 + 2 :: Num ?b => ?b


-- 1 + 2.0
-- ...
--- ((+) 1) :: Num ?b => ?b -> ?b
-- 2.0 :: Fractional ?c => ?c
-- ?c ==> ?b
-- (1 + 2.0) :: Fractional ?c => ?c
