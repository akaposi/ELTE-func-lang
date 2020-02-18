
-- Figyelmeztetés nem teljes mintaillesztéseknél. Érdemes mindig bekapcsolni!
{-# options_ghc -Wincomplete-patterns #-}

-- technikai / gyakorlati info
------------------------------------------------------------

-- workflow:
--   szövefálj.hs
--   parancssorból: ghci
--     :l szövegfájl.hs           betöltés
--     :r                         újratöltés
--     :?                         help
--     :i név                     információ névről (lehet class, típus, definíció)
--     :t kifejezés               kifejezés típusa

------------------------------------------------------------

--    :i definíciónév           általános info
foo = "foo"  -- :i foo

--    :t kifejezés              kifejezés típusa
-- pl:
--      > :t (\x -> x)
--      > (\x -> x) :: p -> p


-- Type holes
------------------------------------------------------------

-- Ha kifejezésben _-t használunk, akkor ghci üzenetben megjelenik
-- a lyuk típusa.

-- map' :: (a -> b) -> [a] -> [b]
-- map' f []     = []
-- map' f (a:as) = _ : _

------------------------------------------------------------
-- Előzetes ismeretek: BSc funkc prog.
-- lambda.inf.elte.hu : jegyzet (Kezdő Haskell) ismétlés!

--    magasabbrendű függvények, fold-ok
--    már félig új: algebrai adattípusok


-- típusosztályok
------------------------------------------------------------

-- szeretnénk (eq :: a -> a -> Bool)
--           függvényt "túlterhelni" különféle típusokra
-- Prelude-ben: Eq


class Eq' a where       -- class deklaráció
  eq :: a -> a -> Bool  -- class method (egy vagy több megadható)

-- definálom eq-t abban az esetben, ahol a = Bool
instance Eq' Bool where
  -- eq :: Bool -> Bool -> Bool
  eq True  True  = True
  eq False False = True
  eq _     _     = False

-- definiálom, ha a = [a]
-- feltételes instance  (constrained instance)
--    abban az esetben definiált, ha bizonyos további instance-ok
--    definiáltak
instance (Eq' a) => Eq' [a] where
  -- eq :: [a] -> [a] -> Bool
  eq []     []     = True
  eq (x:xs) (y:ys) = eq x y && eq xs ys -- az "eq x y" az (Eq' a) instance-ból jön,
                                        -- az "eq xs ys" rekurzív  hívás

  eq _      _      = False

instance (Eq' a, Eq' b) => Eq' (a, b) where
  eq (x, y) (x', y') = eq x x' && eq y y'

-- példa: eq [(True, False)] [(True, False)] == True


-- Prelude-ben: Eq a, metódus: (==) :: a -> a -> Bool
class Eq' a => Ord' a where    -- feltételes deklaráció
  -- less-or-equal             -- Eq' superclass-ja Ord'-nak
  lte :: a -> a -> Bool

-- superclass: csak olyan a-ra adhatunk Ord' instance-t, amire van Eq' a

-- példa:
-- data Maybe a = Nothing | Just a

-- instance Ord' a => Ord' (Maybe a) where
--   lte = _

-- A fenti hibát dob: hiányzik az Eq' (Maybe a) instance!

instance Eq' a => Eq' (Maybe a) where
  eq Nothing  Nothing  = True
  eq (Just x) (Just y) = eq x y
  eq _        _        = False

instance Ord' a => Ord' (Maybe a) where
  lte Nothing  _        = True
  lte (Just _) Nothing  = False
  lte (Just x) (Just y) = lte x y

instance Ord' a => Ord' [a] where
  -- lexikografikus rendezés
  -- [] kisebb (x:xs)-nél
  lte []     _      = True
  lte (_:_)  []     = False
  lte (x:xs) (y:ys) = lte x y && lte xs ys


-- Incomplete pattern warning:
--  a következőt adjuk a fájl elejére:
--  {-# options_ghc -Wincomplete-patterns #-}

-- pl. warning-ot ad:
-- foo :: Bool -> Bool
-- foo True = True

-- Prelude-ben Ord:
--    (<), (<=), (>), (>=)


-- Show (String-ként megjelenítés)
------------------------------------------------------------

-- class Show a where
--   show :: a -> String

-- ghci-ben fontos Show instance:
--  csak akkor lehet kifejezést írni ghci-ben, ha van rá
--  Show instance


-- deriving
------------------------------------------------------------

-- bizonyos instance-ok automatikusan generálhatók:
--   Eq, Show, Ord, Read
-- megjegzés: függvényre nincs Eq, Ord, Show

data Maybe' a = Nothing' | Just' a
  deriving (Eq, Ord, Show)


-- ADT-k (algebraic data type)
------------------------------------------------------------

-- ismétlés: Maybe
-- listák definíciója (std lista újradefiniálható)

-- std lista: két konstruktor:
--   üres lista: []
--   nemüres   : (x:xs)
data List a = Empty | Cons a (List a)
  deriving (Eq, Ord, Show)

l1 :: List Int
l1 = Empty

l1' :: [Int]
l1' = []

l2 :: List Int
l2 = Cons 10 (Cons 20 (Cons 30 Empty))

l2' :: [Int]
l2' = (:) 10 ((:) 20 ((:) 30 []))
   -- 10 : 20 : 30 : []

-- map :: (a -> b) -> [a] -> [b]
-- map f []     = []
-- map f (x:xs) = f x : map f xs

map' :: (a -> b) -> List a -> List b
map' f Empty       = Empty
map' f (Cons x xs) = Cons (f x) (map' f xs)


-- bináris fa
------------------------------------------------------------

data Tree = Leaf | Branch Tree Tree
  deriving (Eq, Show, Ord)

t1 :: Tree
t1 = Leaf

t2 :: Tree
t2 = Branch Leaf Leaf

t3 :: Tree
t3 = Branch
       (Branch
         Leaf
         Leaf)
       (Branch
         Leaf
         (Branch
           Leaf
           Leaf))
