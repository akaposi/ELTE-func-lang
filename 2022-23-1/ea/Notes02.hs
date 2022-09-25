
-- BSc: kezdő
-- Ez a tárgy: középhaladó Haskell
--             nincs: haladó Haskell

-- miért érdemes FP/Haskell-t tanulni?
--    - programverifikáció / típuselmélet / kategóriaelmélet
--      PL formális tárgyalása / logika
--    - praktikusan is lehet Haskell-t használni:
--      interpreter/fordítóprogramok implementálása
--       - Haskell-ben írt: Elm, PureScript, GHC, Agda, pandoc
--       - ipari környezetben: Meta, pénzügyi szektor, kripto
--         (Cardano)

------------------------------------------------------------
-- ADT ismétlés, típusosztályok, Functor
------------------------------------------------------------

-- típusok/típusműletek ~ algebrai műveleteknek

data One = One -- 1 lehetséges érték
data Zero      -- 0 lehetséges érték

data Sum a b = Inl a | Inr b -- összeadjuk a lehetséges értékeket
data Mul a b = Mul a b       -- szorozzuk a lehetséges értékeket

-- |Mul Bool Bool| = |Bool| * |Bool| = 4
-- |Sum Bool (Sum Bool Bool)| = 2 + 2 + 2 = 6

-- |Mul a (Mul b c)| = |Mul (Mul a b) c|
--  = |Mul (Mul b a) c|

-- kommutatív & asszociatív szorzás
-- a * b = b * a
-- a * (b * c) = (a * b) * c

-- refaktorálás:

-- |Sum (Mul a b) (Mul a c)| = |Mul a (Sum b c)|

getA :: Sum (Mul a b) (Mul a c) -> a
getA (Inl (Mul a _)) = a
getA (Inr (Mul a _)) = a

getA' :: Mul a (Sum b c) -> a
getA' (Mul a _) = a

-- függvénytípus ~ hatványozás:

type Pair a = Bool -> a

p :: Pair Int     -- két darab "a" értéket tárol
p True  = 10
p False = 20

fst' :: Pair a -> a
fst' p = p True

snd' :: Pair a -> a
snd' p = p False

-- |Bool -> a| = |a|*|a| = |a|^2
-- |b    -> a| = |a|*|a|*|a|*...*|a| = |a|^|b|
--                      ˇ
--                   |b|-szer


-- rekurzív ADT
data List a =  Nil  | Cons a (List a)
  --           []        x : xs

empty :: List Int
empty = Nil

list1 :: List Int
list1 = Cons 10 (Cons 20 (Cons 30 Nil))
    -- [10, 20, 30]
    -- 10 : 20 : 30 : []
    -- (:) 10 ((:) 20 ((:) 30 []))

    -- emlékeztető: operátorok
    -- infix: 10 + 20
    -- prefix: (+) 10 20

map' :: (a -> b) -> List a -> List b
map' f Nil         = Nil
map' f (Cons a as) = Cons (f a) (map' f as)

-- map :: (a -> b) -> [a] -> [b]
-- map f []     = []
-- map f (a:as) = f a : map f as

-- bináris, leveles fa
------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)

t1 :: Tree Int
t1 = Leaf 20

t2 :: Tree Int
t2 = Node (Leaf 10) (Leaf 20)

t3 :: Tree Int
t3 = Node (Leaf 10) (Node (Leaf 20) (Leaf 30))

t4 :: Tree Int
t4 = Node
       (Node
         (Leaf 10)
         (Leaf 20))
       (Node
         (Leaf 30)
         (Leaf 40))

-- listával elágazó fa típus ("tetszőlegesen elágazó")
data RoseTree a = RNode a [RoseTree a]

rsingle :: RoseTree Int  -- fa 1 értékkel
rsingle = RNode 10 []

r2 :: RoseTree Int
r2 = RNode 10 [RNode 10 [], RNode 20 []]

-- szintaxisfák
data Exp
 = IntLit Int
 | AddExp Exp Exp
 | MulExp Exp Exp

e1 :: Exp
e1 = AddExp (IntLit 100) (IntLit 100) -- 100 + 100

e2 :: Exp
e2 = AddExp (MulExp (IntLit 10) (IntLit 20)) (IntLit 30)
  -- (10 * 20) + 30

-- típusosztályok, Functor osztály
------------------------------------------------------------

-- osztályok:
-- Eq, Show, Ord
-- Eq a => ...
-- Ord a => ...

-- cél: egyenlőség-vizsgálat, osztályok nélkül

eqBool :: Bool -> Bool -> Bool
eqBool True True = True
eqBool False False = True
eqBool _ _ = False

eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList eqa (x:xs) (y:ys) = eqa x y && eqList eqa xs ys
eqList eqa []     []     = True
eqList _   _      _      = False

eqPair :: (a -> a -> Bool) -> (b -> b -> Bool)
       -> (a, b) -> (a, b) -> Bool
eqPair eqa eqb (a, b) (a', b') =
  eqa a a' && eqb b b'

eqListList :: [[Bool]] -> [[Bool]] -> Bool
eqListList = eqList (eqList eqBool)

eqComplicated :: [([Bool], [Bool])] -> [([Bool], [Bool])] -> Bool
eqComplicated = eqList (eqPair (eqList eqBool) (eqList eqBool))

-- típus struktúrája alapján mechanikusan megadható
-- a definíció:

-- osztály deklaráció,
class Eq' a where       -- Eq' : osztály neve,  "a": paraméter
  eq :: a -> a -> Bool  -- "eq" : metódus

instance Eq' Bool where
  -- eq :: Bool -> Bool -> Bool
  eq True True = True
  eq False False = True
  eq _ _ = False

--  "Eq' a": instance constraint
--  nulla vagy több hasonló constraint-et
--  írhatok zárójelben, vesszővel elválasztva
instance (Eq' a) => Eq' [a] where
  -- eq :: [a] -> [a] -> Bool
  eq (x:xs) (y:ys) = eq x y && eq xs ys
    -- eq-t használjuk "a" típusú értékeken
    -- és "[a]" típusú értékeken is
  eq []     []     = True
  eq _      _      = False

instance (Eq' a, Eq' b) => Eq' (a, b) where
  eq (a, b) (a', b') = eq a a' && eq b b'

-- "interface" : C#, Java (nem ugyanaz mint a typeclass)
-- Rust: trait, Swift: protocol - ugyanaz mint a typeclass

-- superclass: "Eq' a" szuperosztály megszorítás
class Eq' a => Ord' a where
  lt :: a -> a -> Bool     -- "less than"

-- minden (Ord' a) instance csak akkor adható meg, ha
-- már (Eq' a) létezik.

-- típuskövetkeztetés: ha van (Ord' a), akkor van (Eq' a) is

instance Ord' Bool where
  lt False True = True    -- False < True
  lt _     _    = False

-- instance megszorítás felvehető bármilyen definícióhoz
foo :: Eq' a => a -> a -> a -> a
foo x y z = if eq x y then x else z

bar :: Ord' a => a -> a -> a -> a
bar x y z = if lt x y then x else z

-- superclass!
--  ha Ord' a van, akkor Eq' a is
bar' :: Ord' a => a -> a -> a -> a
bar' x y z = if eq x y then x else z

-- standard osztályok
-- Eq a    : (==), (/=)
-- Ord a   : (<), (>), (<=), (>=)

-- class Show a where
--   show :: a -> String
-- ghci-ben csak (Show a) értékeket lehet kinyomtatni!

-- feature: instance automatikus generálása
data MyTree a = MyLeaf a | MyNode (MyTree a) (MyTree a)
  deriving (Eq, Show, Ord)


{-
class Semigroup a
  (<>) :: a -> a -> a
  -- konvenció: (<>) asszociatív
  -- ∀ x y z.  x <> (y <> z) = (x <> y) <> z

  -- instance Semigroup [a] where
  --   (<>) = (++)


-- adjunk egységelemet a <> művelethez
class Semigroup a => Monoid a
  mempty :: a
  -- konvenció:
  -- mempty <> x = x
  -- x <> mempty = x

  -- instance Monoid [a] where
  --   mempty = []
-}

-- nem véssük kőbe egyik instance-ot sem

-- instance Semigroup Int where
--   (<>) = (+)

-- instance Semigroup Int where
--   (<>) = (*)

-- ehelyett: wrapper típusokat megadunk,
--  ugyanazt az adatot tárolják az instance-jaik
--  viszont különböznek

-- newtype: a "data" alternatívája,
--   - nincs futásidejű költsége
--   - csak akkor lehet newtype, ha egy konstruktor van
--     egy mezővel

newtype MulWrap = MulWrap Int

instance Semigroup MulWrap where
  (<>) (MulWrap x) (MulWrap y) = MulWrap (x * y)
