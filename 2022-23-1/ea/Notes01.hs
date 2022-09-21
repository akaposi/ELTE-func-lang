{-# LANGUAGE EmptyCase #-}

module Notes01 where

-- algebrai adattipusok

data List a = Nil | Cons a (List a)
--   [a]      []    _:_

-- felsorolas tipus
data Egtaj     =   Eszak | Nyugat | Del | Kelet
  deriving Show
--   ^ tipus neve  ^ konstruktorok
-- minden tipus is nagybetus, konstroktorok is

-- mintaillesztes: mindig megteheto, ha a fuggveny bemenete data-val megadott tipus
fordul :: Egtaj -> Egtaj
fordul Del = Nyugat
fordul Nyugat = Eszak
fordul Eszak = Kelet
fordul Kelet = Del

fordul' :: Egtaj -> Egtaj
fordul' e = case e of
  Del -> Nyugat
  Nyugat -> Eszak
  Eszak -> Kelet
  Kelet -> Del


-- data Int = 0 | 1 | -1 | 2 | -2 | 3 | -3 | ...

f :: Int -> Int
f 0 = 2
f 1 = -1
f _ = 100

-- case elonye a mintailleszteshez kepest
f' :: Int -> Int
f' x = case (x+3) of
  0 -> 2
  1 -> -1
  _ -> 100

g :: (Int -> Int) -> Int
g f = f (f (f (f 3)) + 4)

data One = One

f1 :: One -> Bool
f1 One = True

f2 :: One -> Bool
f2 One = False

data Zero

-- eddig a felsorolas tipusok

-- Descartes szorzat A × B = { (a,b) | a ∈ A, b ∈ B }
data Prod a b = Pair a b
  deriving Show
--   ^ parameteres tipus, ket parameterrel
-- Prod a b = (a,b)
-- data (a,b) = (a,b)

{-
tipusok      Prod, One
----------------------------------------
termek       konstruktorok, Pair, One, \, applikacio,...
-}


f3 :: Prod Int Bool -> Bool
f3 w = case w of
  Pair i b -> b

-- *   Prod
-- 2   Bool
-- 1   One
-- 0   Zero

-- Prod a b elemeinek a szama = a elemeinek a szama * b elemeinek a szamaval

type Four = Prod Bool Bool
zero, one, two, three :: Four
zero  = Pair False False
one   = Pair False True
two   = Pair True  False
three = Pair True True
four :: Four
four = four -- ezt nem szamitjuk bele az elemek szamaba

type Bool' = Prod Bool One
true', false' :: Bool'
true' = Pair True One
false' = Pair False One

-- data Either a b = Left a | Right b
data Sum a b = Inl a | Inr b

-- *   Prod
-- +   Sum
-- 2   Bool
-- 1   One
-- 0   Zero

-- osszeadas kommutativ: a+b = b+a
--                       2+4 = 4+2
--         Sum Bool Four -> Sum Four Bool
--         Sum Four Bool -> Sum Bool Four

comm :: Sum a b -> Sum b a
comm x = case x of
  (Inl b) -> Inr b
  (Inr f) -> Inl f

-- 0+a = a

leftzero1 :: Sum Zero a -> a
leftzero1 (Inl x) = case x of
leftzero1 (Inr y) = y   -- y :: a

leftzero2 :: a -> Sum Zero a
leftzero2 = Inr

type Bool'' = Sum One One

true'', false'' :: Bool''
true'' = Inl One
false'' = Inr One

ifthenelse :: Bool'' -> a -> a -> a
ifthenelse (Inl One)  x y = x
ifthenelse (Inr One) x y = y

-- 0 Zero
-- 1 One
-- + Sum
-- * Prod
-- ^ (->)
-- a^b * a^c = a^(b+c)

-- (a^b)^c=a^(b*c)   c->(b->a) = Prod c b -> a

curry' :: (Prod c b -> a) -> c->b->a
curry' f c b = f (Pair c b)
--               ^::Prod c b
--                     ^::c
--                       ^::b
-- c :: c
-- b :: b
-- (a,b) :: (a,b)
-- \ x -> y
--   a -> b

-- a^b * a^c = a^(b+c)

explaw1 :: Prod (b -> a) (c -> a) -> Sum b c -> a
explaw1 = undefined

explaw2 :: (Sum b c -> a) -> Prod (b -> a) (c -> a)
explaw2 = undefined

-- 2^3 = (1+1)^((1+1)+1)
e0,e1,e2 :: Sum (Sum One One) One -> Sum One One
e0 (Inl (Inl One)) = Inl One
e0 (Inl (Inr One)) = Inl One
e0 (Inr One)       = Inl One

e1 (Inl (Inl One)) = Inl One
e1 (Inl (Inr One)) = Inl One
e1 (Inr One)       = Inr One

e2 (Inl (Inl One)) = Inl One
e2 (Inl (Inr One)) = Inr One
e2 (Inr One)       = Inl One

-- HF: megadni e3,e4,e5,e6,e7 -ket

main = undefined

-- nemrekurziv data-kat neztunk, mitol algebrai? tipusok ugy viselkednek, mint a termeszetes szamok
-- 0,1,2,3,4,..., +, *, ^
-- ugyanazok az egyenlosegek teljesulnek, oda-vissza fuggvenyek formajaban
