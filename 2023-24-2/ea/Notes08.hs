import Prelude hiding (head, tail)

-- induktiv tipusok polinomokkal, generikus programozas, egyenlosegek nelkuli univerzalis algebra

-- ciklikus listan egy fgv az f : List A -> C
-- f [a,b,c] = f [b,c,a] = f [c,a,b]

data List a = Nil | Cons a (List a)
data Stream' a = Conss a (Stream a)

-- egy (ko)induktiv tipust egy polinom hataroz meg
-- minden polimomnak van legkisebb es legnagyobb fixpontja
{-
F :: * -> *
F x = x^2 + 2*x + 1

F a ≅ a   <- F fixpontja

minden F-nek van legkisebb fixpontja (μF) es legnagyobb is (νF)

altipus relacio:

a ⊂ b      x :: a
-----------------
     x :: b


-------------    ----------
Int ⊂ Integer    Bool ⊂ Int


A ⊂ A'   B ⊂ B'       A ⊂ A'             B ⊂ B'
---------------       -------------------------
(A,B) ⊂ (A',B')       Either A B ⊂ Either A' B'

A ⊂ A'      B ⊂ B'
-------------------
(A' → B) ⊂ (A → B')

A           B ⊂ B'
------------------
(A → B) ⊂ (A → B')

A' ⊂ A      B             Int ⊂ Integer
------------------        -------------------------------
(A → B) ⊂ (A' → B)        (Integer → Bool) ⊂ (Int → Bool)

------     ---------
A ⊂ ()     Empty ⊂ A


F X = X + 1 = Maybe X

μF = Nat
νF = Conat

con :: Maybe Nat -> Nat
con Nothing = Zero
con (Just n) = Suc n
-}

-- A az f fixpontja, ha A ≅ f A
data Fix f = Con { des :: f (Fix f) }   -- Con a konstruktor, des a destruktor

-- m: methods
-- a: target
ite :: Functor f => (f a -> a) -> Fix f -> a
ite m (Con h) = m $ fmap (ite m) h
-- m :: f a -> a
-- h :: f (Fix f)
-- ? :: f a
-- fmap (ite m) :: f (Fix f) -> f a
-- fmap (ite m) h :: f a
-- ite m :: Fix f -> a

type Nat = Fix Maybe

zero :: Nat
zero = Con Nothing
suc :: Nat -> Nat
suc n = Con (Just n)

-- (a,(a -> a)) = (a^1,a^a) = a^(1+a) = Maybe a -> a
iteNat :: a -> (a -> a) -> Nat -> a
iteNat z s (Con Nothing) = z
iteNat z s (Con (Just n)) = s (iteNat z s n)

showNat :: Nat -> Int
showNat (Con Nothing) = 0
showNat (Con (Just n)) = 1 + showNat n

plus :: Nat -> Nat -> Nat
plus m n = iteNat n suc m

three, four :: Nat
three = suc $ suc $ suc zero
four = suc three

----------------------------------------------

-- Con :: F Tree -> Tree
-- Leaf :: Int -> Tree
-- Node :: Tree -> Tree -> Tree
-- F x ≅ Either Int (x,x)
-- F x = Int + x^2
-- Con :: Either Int (Tree,Tree) -> Tree
-- Con (Left i) = Leaf i
-- Con (Right (t1,t2)) = Node t1 t2

newtype F x = MkF (Either Int (x,x))

instance Functor F where
  fmap :: (a -> b) -> F a -> F b
  fmap f (MkF (Left i)) = MkF (Left i)
  fmap f (MkF (Right (x,y))) = MkF (Right (f x,f y))

type Tree = Fix F

leaf :: Int -> Tree
leaf i = Con $ MkF $ Left i
node :: Tree -> Tree -> Tree
node t1 t2 = Con $ MkF $ Right (t1,t2)

-- ((Int -> a),(a -> a -> a)) = (a^Int,a^(a,a)) = (a^Int,a^(a^2)) = a^(Int+a^2) = F a -> a
iteTree :: (Int -> a) -> (a -> a -> a) -> Tree -> a
iteTree l n (Con (MkF (Left i))) = l i
iteTree l n (Con (MkF (Right (t1,t2)))) = n (iteTree l n t1) (iteTree l n t2)

tree :: Tree
tree = node (leaf 10) (node (leaf 3) (node (leaf 1) (leaf 2)))

sumTree :: Tree -> Int
sumTree = iteTree id (+)

-- F x = (Nat -> x) + 1
-- F x = x^Nat + 1
-- μF az a vegtelenfele agazodo, veges melysegu fak tipusa

{-
F polinom, akkor funktor:

F x = an * x^n + ... + a1 * x + a0

fmap :: (b -> c) -> F b -> F c
fmap f (In{i} (w,(x0,...,x{i-1}))) = (In{i} (w,(f x0,...,f x{i-1})))
-- w :: ai

F x = Nat -> x = Reader Nat x
-}

-- μ = ν

-- des :: Fix f -> f (Fix f)    gen :: (a -> f a) -> a -> Fix f
-- con :: f (Fix f) -> fix f    ite :: (f a -> a) -> Fix f -> a

-- Stream a-nak az F-je: F x = (a,x)
type Stream a = Fix ((,) a)

head :: Stream a -> a
head = fst . des

tail :: Stream a -> Stream a
tail = snd . des

gen :: Functor f => (a -> f a) -> a -> Fix f
gen h a = Con $ fmap (gen h) (h a)
-- ? :: f (Fix f)
-- h :: a -> f a
-- a :: a
-- h a :: f a
-- gen h :: a -> Fix f
-- fmap (gen h) :: f a -> f (Fix f)
-- fmap (gen h) (h a) :: f (Fix f)

from :: Int -> Stream Int
from = gen (\i -> (i,i+1))

-- F x = (Bool -> x)  ennek a legnagyobb fixpontja a vegtelen mely binaris fak tipusa
