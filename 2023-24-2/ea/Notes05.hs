-- ez az ora 2 perccel rovidebb

data A :: * where
  C1 :: A -> A -> A
  C2 :: A

type ChurchA = forall a . (a -> a -> a) -> a -> a

chA :: ChurchA
chA node leaf = node (node leaf leaf) leaf
-- chA csak node es leaf kombinacioja lehet

iteChurchA :: ChurchA -> (a -> a -> a) -> a -> a
iteChurchA x = x

-- universal property

length :: [a] -> Int
length = foldr (const (+1)) 0

-- (Bool -> b) ≅ (b,b)                    -- x := (... y...) :: b       x = if y then x[y↦true] else x[y↦false]
{-
f :: (Bool -> b) -> (b,b)
f h = (h True,h False)

g :: (b,b) -> Bool -> b
g (t,f) b = if b then t else f

f (g (t,f)) =
f (\b -> if b then t else f) =
((\b -> if b then t else f) True, (\b -> if b then t else f) False) =
(if True then t else f, if False then t else f) =
(t,f)

g (f h) =
g (h True,h False) =
\b -> if b then h True else h False =
  (1) (\b -> if b then h True else h False) True = h True
  (2) (\b -> if b then h True else h False) False = h False
h :: Bool -> b

masik fajta erveles:
\b -> if b then h True else h False =
\b -> if b then h b else h b =
\b -> h b =
h
-}

data Nat = Zero | Suc Nat
  deriving Show
-- (Nat -> b) ≅ ((b -> b),b)          -- x = (... n ...) :: b   ->   x = if n==0 then x[n↦0] else if n==1 then x[n↦1] else if n==2 then x[n↦2] else ...
-- ez hulyeseg, mert ha b=Bool, a bal oldal vegtelen, a jobb oldal veges
{-
f :: (Nat -> b) -> ((b -> b),b)   
f h = (\x -> ? ,)
-}

iteNat :: ((b -> b),b) -> Nat -> b
iteNat (s,z) Zero = z
iteNat (s,z) (Suc n) = s (iteNat (s,z) n)

-- (Nat -> b) ≅ ((Nat -> b -> b),b)

recNat :: ((Nat -> b -> b),b) -> Nat -> b
recNat (s,z) Zero = z
recNat (s,z) (Suc n) = s n (recNat (s,z) n)

pred, pred' :: Nat -> Nat
pred = recNat ((\n _ -> n),Zero)
pred' = fst . iteNat ((\ (x,y) -> (y,Suc y)) , (Zero,Zero))

-- iteNat (z,s) (Suc (Suc (Suc Zero))) = s (s (s z))
-- s (s (s z)) = s (s (s (0,0))) = s (s (0,1)) = s (1,2) = (2,3)
-- s (x,y) = (y,y+1)



-- ([a] -> b) ≅ ((a -> b -> b),b)
{-
f :: ([a] -> b) -> ((a -> b -> b),b)
f h = (???,h [])

g :: ((a -> b -> b),b) -> ([a] -> b)
g (c,n) xs = foldr c n xs


-}

-- ezek a szep universal property-k:
-- (c -> (a -> b)) ≅ ((c,a) -> b)
-- (c -> (a,b)) ≅ ((c -> a),(c -> b))

-- Hindley-Milner:
-- forall a1,a2,a3,...an, (a2 -> a3 -> (a4 -> a1) -> a1)
-- BOOL = (forall b .b -> b-> b)
-- nincs Hindley-Milnerben: BOOL -> BOOL = (forall b .b -> b-> b) -> (forall b .b -> b-> b)
-- van Hindley-Milnerben: forall b1 b2 . (b1 -> b1 -> b1) -> b2 -> b2 -> b2

-- derive Functor, Contravariant
{-
data f a = ((a -> bool) -> int)   functor
data g a = (a -> bool)   contravariant
-}

-- Monad:
-- - Exp, eval, div
{-
data Exp = Lit Int | Plus Exp Exp | Mul Exp Exp
  deriving (Show)

eval :: Exp -> Int
eval (Lit n) = n
eval (Plus e1 e2) = eval e1 + eval e2
eval (Mul  e1 e2) = eval e1 * eval e2
-}
type ExpShallow = Int
lit :: ExpShallow -> ExpShallow
lit n = n
plus :: ExpShallow -> ExpShallow -> ExpShallow
plus = (+)
mul :: ExpShallow -> ExpShallow -> ExpShallow
mul = (*)

data Exp = Lit Int | Plus Exp Exp | Mul Exp Exp | Div Exp Exp
  deriving (Show)

safeDiv :: Int -> Int -> Maybe Int
safeDiv a b | b == 0 = Nothing
            | otherwise = Just (a `div` b)

eval :: Exp -> Maybe Int
{-
eval (Lit n) = Just n
eval (Plus e1 e2) = case (eval e1, eval e2) of
  (Just n1,Just n2) -> Just (n1 + n2)
  _                 -> Nothing
eval (Mul  e1 e2) = case (eval e1, eval e2) of
  (Just n1,Just n2) -> Just (n1 * n2)
  _                 -> Nothing
eval (Div  e1 e2) = case (eval e1, eval e2) of
  (Just n1,Just n2) -> n1 `safeDiv` n2
  _                 -> Nothing
-}
eval (Lit n) = return n
eval (Plus e1 e2) = do
  n1 <- eval e1
  n2 <- eval e2
  return (n1 + n2)
eval (Mul  e1 e2) = do
  n1 <- eval e1
  n2 <- eval e2
  return (n1 * n2)
eval (Div  e1 e2) = do
  n1 <- eval e1
  n2 <- eval e2
  n1 `safeDiv` n2


-- a jovo ora 5 perccel rovidebb
