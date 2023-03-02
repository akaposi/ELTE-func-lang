
{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

eqBool :: Bool -> Bool -> Bool
eqBool True True = True
eqBool False False = True
eqBool _ _ = False

eqInt :: Int -> Int -> Bool
eqInt x y = (x == y)

eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList eqa []     []     = True
eqList eqa (x:xs) (y:ys) = eqa x y && eqList eqa xs ys
eqList _   _      _      = False

eqPair :: (a -> a -> Bool) -> (b -> b -> Bool) -> (a, b) -> (a, b) -> Bool
eqPair eqa eqb (x, y) (x', y') = eqa x x' && eqb y y'

eqListBool :: [Bool] -> [Bool] -> Bool
eqListBool = eqList eqBool  -- parciális applikáció

   -- eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
   -- eqBool :: Bool -> Bool -> Bool
   -- eqList eqBool :: [Bool] -> [Bool] -> Bool

eqPairListBoolListBool :: ([Bool], [Bool]) -> ([Bool], [Bool]) -> Bool
eqPairListBoolListBool = eqPair (eqList eqBool) (eqList eqBool)

-- típusosztály : típus alapján automatikusan levezeti a fenti
--  definíciókat (kódot generál)

-- deklaráció
class Eq' a where           -- "a" osztály paraméter
  eq :: a -> a -> Bool      -- "eq" : osztály metódus

-- instance
instance Eq' Bool where

  eq :: Bool -> Bool -> Bool
  eq = eqBool

instance Eq' Int where
  eq = eqInt

-- eq (0 :: Int) 0 == True


--  megszorítás:
--    Eq' a =>
instance Eq' a => Eq' [a] where

  eq :: [a] -> [a] -> Bool
  eq []     []     = True
  eq (x:xs) (y:ys) = eq x y && eq xs ys
  eq _      _      = False

instance (Eq' a, Eq' b) => Eq' (a, b) where
  eq (x, y) (x', y') = eq x x' && eq y y'


eqPairListBoolListBool' :: ([Bool], [Bool]) -> ([Bool], [Bool]) -> Bool
eqPairListBoolListBool' = eq
   -- eqPair (eqList eqBool) (eqList eqBool)
     -- (ezt a fordító még tovább inline-olja és optimalizálja)

-- típusok struktúrája alapján kódot generálunk


-- Gyakori, egyszerű osztályok

-- class Eq a where
--   (==) :: a -> a -> Bool


-- "Eq' a" =>       "superclass" megszorítás
--                  minden instance-nál, csak akkor adhatunk (Ord' a) instance-t
--                  ha már van egy (Eq' a) instance.

class Eq' a => Ord' a where
  le :: a -> a -> Bool       -- "le": less or equal

instance Ord' Bool where
  le True False = False
  le _    _     = True

-- hiba:
-- instance Ord' Char where
--   le = undefined


-- függvényben osztályok használata:
f1 :: Bool -> Bool
f1 b = eq b True

sort :: Ord' a => [a] -> [a]
sort = undefined

f2 :: Eq' a => a -> a -> a -> a
f2 a b c = if eq a b then a else c

f3 :: Ord' a => a -> a -> a -> a      -- (Ord' a)-ből következik (Eq' a)
f3 a b c = if eq a b then a else c    --   (superclass-ozás miatt)


-- standard
-- class Eq a => Ord a where
--   (<) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   stb.   (lásd ghci-ben: ":i Ord")

-- standard  (ghci-ben csak Show értékeket lehet kinyomtatni!)
-- class Show a where
--   show :: a -> String


-- ADT-k (algebrai adattípusok)
------------------------------------------------------------

-- enumeráció:

--   "Color" : típus konstruktor
--   "Red", "Green", "Blue" : érték konstruktorok
data Color = Red | Green | Blue
  deriving (Eq, Show, Ord)

-- fejlettebb deriving feature: DerivingVia (utána lehet nézni)

c1 :: Color
c1 = Red

-- + mintaillesztés
f4 :: Color -> Int
f4 Red = 100
f4 _   = 200

-- általános eset:
-- "a"       : típus paraméter
-- "Just' a" : "a" típusú adatmezőt tartalmaz a konstruktor

data Maybe' a = Nothing' | Just' a
  deriving (Eq, Show)

m1 :: Maybe' Int
m1 = Nothing'

m2 :: Maybe' Int
m2 = Just' 100

-- Nothing' :: Maybe' a
-- Just'    :: a -> Maybe' a

-- mintaillesztés:
f5 :: (a -> b) -> Maybe' a -> Maybe' b
f5 f Nothing'  = Nothing'
f5 f (Just' x) = Just' (f x)


-- rekurzív típus
-- definiáljuk újra a listák típusát:

data List a =
    Nil               -- üres lista
  | Cons a (List a)   -- fej + farok (rekurzívan)
  deriving (Eq, Show)

-- [a]    ~ List a
-- []     ~ Nil
-- (x:xs) ~ Cons x xs

map' :: (a -> b) -> List a -> List b
map' f Nil         = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

-- standard listára ugyanez:
-- map :: (a -> b) -> List a -> List b
-- map f []     = []
-- map f (x:xs) = f x : map f xs

l1 :: List Int      -- [0, 1, 2, 3] vagy (0:1:2:3:[])
l1 = Cons 0 (Cons 1 (Cons 2 (Cons 3 Nil)))

-- általánosan: fa típusok definiálhatók

data BinTree a = Leaf a | Node (BinTree a) (BinTree a)
  deriving (Eq, Show)

t1 :: BinTree Int
t1 = Leaf 200

t2 :: BinTree Int
t2 = Node (Leaf 100)
          (Node t1 t1)

t3 :: BinTree Int
t3 = Node
       (Node
         (Leaf 0)
         (Leaf 1))
       (Node
         (Leaf 2)
         (Leaf 3))

------------------------------------------------------------
-- Miért "algebrai" adattípus?
-- Röviden: algebrai képlet megadja a lehetséges értékek számát

-- jelölés: |a| az "a" típus lehetséges értékeinek száma

-- üres adattípus: nincs konstruktor   |Zero| = 0
data Zero

data One = One     -- ne keverjük össze a "One" típust és a "One" értéket!
                   -- One :: One
                   -- |One| = 1

-- |Mul a b| = |a| * |b|       -- |Mul Bool Bool| = |Bool|*|Bool| = 4
data Mul a b = Pair a b

-- |Add a b| = |a| + |b|
data Add a b = Left' a | Right' b

-- standard típus: Either a b
-- data Either a b = Left a | Right b

-- köv előadás: hatványozás + aritmetikai azonosságok típusokon
