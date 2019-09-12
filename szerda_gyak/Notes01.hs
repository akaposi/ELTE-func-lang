
-- Téma: típusosztályok bevezetés, ADT-k
-- Ajánlott jegyzet: http://lambda.inf.elte.hu/Classes.xml

--------------------------------------------------------------------------------

-- Probléma: hogyan írjunk egyenlőség-vizsgálatot?

-- alaptípusokra egyszerű:
eqBool :: Bool -> Bool -> Bool
eqBool True  True  = True
eqBool False False = True
eqBool _     _     = False

-- összetett típusoknál viszont feltételeznünk kell, hogy
-- a résztípusokra is van egyenlőség-vizsgálat
eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList eqa []     []     = True
eqList eqa (x:xs) (y:ys) = eqa x y && eqList eqa xs ys
eqList _   _      _      = False

eqPair :: (a -> a -> Bool) -> (b -> b -> Bool)
          -> (a, b) -> (a, b) -> Bool
eqPair eqa eqb (a, b) (a', b') = eqa a a' && eqb b b'

-- kényelmetlen viszont, ha minden specifikus összetett típusra
-- kézzel kell összerakni az egyenlőséget vizsgáló függvényt.

-- példák:
eqListBool :: [Bool] -> [Bool] -> Bool
eqListBool = eqList eqBool

eqListListBool :: [[Bool]] -> [[Bool]] -> Bool
eqListListBool = eqList (eqList eqBool)

bigEq :: [[(Bool, [Bool])]] -> [[(Bool, [Bool])]] -> Bool
bigEq = eqList (eqList (eqPair eqBool (eqList eqBool)))

-- látszik, hogy az "eq" függvények összerakása teljesen mechanikus
-- feladat.

-- a típusosztályok segítségével automatikusan generálni tudjuk ezeket
-- a függvényeket a típusok struktúrája alapján


class Eq' a where        -- osztály deklaráció
  eq :: a -> a -> Bool   -- osztály metódus

instance Eq' Bool where  -- instance
  eq True  True  = True
  eq False False = True
  eq _     _     = False
  -- eq = eqBool

-- példa: eq True False == False
--        eq True True  == True
--        eq [True] []   -- hiba ha nincs Eq [a] instance

-- feltételes instance
-- csak akkor van eq listára, ha az elemekre is van
instance Eq' a => Eq' [a] where
  eq []     []     = True
  eq (x:xs) (y:ys) = eq x y && eq xs ys
              --     Eq' a     Eq' [a]
  eq _      _      = False

  -- eq = eqList eq

-- A fenti instance-ot a GHC lefordítja az alábbi, korábban már
-- látottt kódra:

-- eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
-- eqList eqa []     []     = True
-- eqList eqa (x:xs) (y:ys) = eqa x y && eqList eqa xs ys
-- eqList _   _      _      = False

instance (Eq' a, Eq' b) => Eq' (a, b) where
  eq (a, b) (a', b') = eq a a' && eq b b'

-- Eq' standard megfelelője
--     Eq osztály
--     metódus: (==) operátor

-- kérdés: milyen típusra *nincs* Eq instance?
--   - nagyon nem hatékony: Int -> Int
--   - egyáltalán nincs: [Bool] -> Int
--       mert [Bool]-nak végtelen sok értéke van


-- GHCI kitérő/ismétlés
------------------------------------------------------------

-- :r             újratöltés
-- :l <file>      fájl betöltése
-- :t <kifejezés> kifejezés típusa
-- :i <név>       információ névről

-- operátorok keresésére :i fontos (nehéz operátort google-zni!)
-- osztály instance-jainak listázása: szintén :i

-- type hole-ok:
-- bármilyen kifejezésben ha _-t használunk, akkor
--   GHC üzenetben megkapjuk a "hole" típusát

map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs
  -- a hole típusa alapján lépésenként finomíthatjuk a programot

  -- 1. lépés = _
  -- 2.       = _ : _
  -- 3.       = f x : _
  -- 4.       = f x : map' f xs

f1 :: (b -> c) -> (a -> b) -> a -> c
f1 = \f g a -> f (g a)
   --     = _
   --     = \f g a -> _
   --     = \f g a -> f _
   --     = \f g a -> f (g _)
   --     = \f g a -> f (g a)


-- Rekurzív ADT-k
--------------------------------------------------------------------------------

data List a = Empty | Cons a (List a)

-- bináris fák:
--   levél
--   bináris elágazás
data BinTree = BTLeaf | BTNode BinTree BinTree

t1 :: BinTree
t1 = BTLeaf

t2 = BTNode BTLeaf BTLeaf
t3 = BTNode (BTNode BTLeaf BTLeaf) BTLeaf
t4 = BTNode t3 t3
t5 = BTNode t4 t4

-- Annotált bináris fa:
data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

-- példa végtelen bináris fára:
tree :: Tree Int
tree = go 0 where
  -- (Haskell konvenció: lokális segédfüggvény neve
  --  gyakran go)
  go n = Node n (go (n + 1)) (go (n + 1))

  -- pl:
  -- go 0 = Node 0 (go 1) (go 1)
  -- go 1 = Node 1 (go 2) (go 2)
  -- go 2 = Node 2 (go 3) (go 3)
  -- stb.

-- hasonló, mint a lista "take"
-- vesszük egy fa első n rétegét:
takeTree :: Int -> Tree a -> Tree a
takeTree 0 t              = Leaf
takeTree n Leaf           = Leaf
takeTree n (Node a t1 t2) =
  Node a (takeTree (n - 1) t1) (takeTree (n - 1) t2)

-- feladat: írjunk Eq instance-ot Tree-hez

instance Eq a => Eq (Tree a) where
  Leaf       == Leaf          = True
  Node a l r == Node a' l' r' = a == a' && l == l' && r == r'
  _          == _             = False
