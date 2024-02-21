-- Show

{-
data Nat    = Zero | Suc    Nat
data List a = Nil  | Cons a (List a)
data []   a = []   | (:)  a ([] a)
data [a]    = []   | a:[a]

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

(+) :: Nat -> Nat -> Nat
Zero   + ys = ys
Suc xs + ys = Suc (xs + ys)
-}

-- |xs| = a, |ys| = b, |zs| = c
-- lepes((xs ++ ys) ++ zs) = a + (a + b)
-- lepes(xs ++ (ys ++ zs)) = a + b
-- ((((++)++)++)++)

type DiffList a = [a] -> [a]

-- [1,2,3]
egykettoharom :: DiffList Int
egykettoharom = \ xs -> [1,2,3] ++ xs

(+++) :: DiffList a -> DiffList a -> DiffList a
-- xs' ++' ys' = \zs -> xs' (ys' zs)
-- xs' :: [a] -> [a]
-- ys' :: [a] -> [a]
-- ?   :: [a] -> [a]
(+++) = (.)

fromList :: [a] -> DiffList a
fromList xs = \ys -> xs ++ ys
toList :: DiffList a -> [a]
toList xs' = xs' []

xs, ys, zs :: [Int]
xs = [1,2..1000000]
xs' = [1,2..1000000]
ys = [1,2..1000]
zs = [1,2..1000]

test1 = length (xs' ++ (xs ++ (ys ++ zs)))
test2 = length (((xs' ++ xs) ++ ys) ++ zs)

testList = toList
  (fromList xs +++ fromList ys +++ fromList zs)

len = length testList

{-
((f . g) . h) = \x -> (f . g) (h x) = \x -> f(g(h x))
(f . (g . h)) = \x - f ((g.h) x) =  \x -> f(g(h x))

toList (fromList xs . fromList ys . fromList zs) =
toList (\qs-> fromList xs (fromList ys (fromList zs qs)))) =
toList (\qs-> xs++(ys++(zs++qs))) =
xs++(ys++(zs++[]))

HF: megcsinalni fromList'-t, ami nem ele, hanem moge rakja a
reprezentalt listat, es az a kevesbe hatekonyra asszocialja
-}

-- Yoneda lemma: strictification

-- (xs ++ ys) ++ zs = xs ++ (ys ++ zs)

-- definicio szerinti asszociativitast kaptunk a listakra

instance Eq a => Eq (DiffList a) where
  xs' == ys' = toList xs' == toList ys'

instance Eq (Bool -> Bool) where
  f == g = f True == g True && f False == g False

class Finite a where
  elems :: [a]
-- elems-nek vegesnek kell lennie

instance Finite Bool where
  elems = [False, True]

instance (Finite a, Finite b) => Finite (Either a b) where
  elems = map Left elems ++ map Right elems
  -- +

instance (Finite a, Finite b) => Finite (a,b) where
  elems = [ (a,b) | a <- elems, b <- elems ]
  -- *

ex2 :: [Either Bool Bool]
ex2 = elems
ex3 :: [(Bool,Either Bool Bool)]
ex3 = elems

-- instance (Finite a, Finite b) => Finite (a->b) where
  -- HF
  -- b^a

-- instance Finite a => Eq a where -- NEM MUKODIK ALTALANOSSAGBAN

indexOf :: Eq a => a -> [a] -> Integer
indexOf x xs = helper xs 0
  where
    helper (y:ys) i | x == y = i
                    | otherwise = helper ys (i+1)

data Pack a = Pack a
  deriving Eq

instance (Eq a, Finite a) => Ord (Pack a) where
  compare (Pack x) (Pack y) = compare
    (indexOf x elems) (indexOf y elems)
    
whatIs :: Bool
whatIs = Pack (Left True, False) <
  Pack (Right False, True)

-- 0,1,2,3,...,+,*,^,

data Empty -- 0
-- 1 = Maybe Empty
-- 2 = Maybe 1
-- 3 = Maybe 2
-- 4 = Maybe 3
-- ...
-- + = Either
-- * = (,)
-- ^ = "flip (->)"
-- 3 + 3 = 2 * 3
type Three = Maybe Bool
-- Either Bool Bool ≅ (Bool,Bool)
oda :: Either Bool Bool -> (Bool,Bool)
oda (Left b) = (False, b)
oda (Right b) = (True, b)
vissza :: (Bool,Bool) -> Either Bool Bool
vissza (False, b) = Left b
vissza (True , b) = Right b
{-
oda (vissza x) = x:
vissza (oda x)) = x
-}
izoE :: Bool
izoE =
  map (oda.vissza) elems == elems &&
  map (vissza.oda) elems == elems

-- Either Three Three ≅ (Bool,Three)

-- term :: tipus :: kind
-- 1    :: Int   :: *
-- minden tipus kindja *
-- tipusoperatorok kindja nem *
-- tipusfuggvenyek
-- [] :: * -> *
-- Either :: * -> * -> *
-- :: (* -> *) -> *

data List (a :: *) = Nil | Cons a (List a)
data Fura (t :: * -> *) = Con (t Int) (t Int)

-- Fura [] ≅ ([Int],[Int])

plFura :: Fura []
plFura = Con [1,2,3] [2,3,4]

-- f fixpontja egy a, melyre f a = a
-- ... = f (f (f a)) = f (f a) = f a = a

data Fixpont (f :: * -> *) = Mk (f (Fixpont f))

oda' :: Fixpont f -> f (Fixpont f)
oda' (Mk x) = x
vissza' :: f (Fixpont f) -> Fixpont f
vissza' = Mk
{-
oda' (vissza' x) = oda' (Mk x) = x
vissza' (oda' (Mk x)) = vissza' x = Mk x
-}

-- Fixpont f ≅ f (Fixpont f)

type Nat = Fixpont Maybe
zero :: Nat
zero = Mk Nothing
suc  :: Nat -> Nat
suc n = Mk (Just n)

-- osszeadas HF ezen

-- altalanos fold-ok

-- polinomialis funktorok:
-- 1 + 2*x^2 + 3*x^4

-- jovo oran Functor

-- 