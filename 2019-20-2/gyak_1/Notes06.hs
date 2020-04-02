
-- Feladat 1: tekintsük a következő típust, aminek az elemei egyszerű kifejezésfák:
data Exp = IntLit Int | BoolLit Bool | Add Exp Exp | Not Exp | Eq Exp Exp
  deriving (Show)

-- pl.
e1 = Add (IntLit 0) (IntLit 10) -- 0 + 10
e2 = Eq (IntLit 0) e1           -- 0 == (a + 10)
e3 = Not e2                     -- not (0 == (0 + 10))

-- Írjunk egy függvényt, ami egy Exp kifejezést kiértékel Either Int Bool-ra!
-- A kiértékelés legyen értelemszerű, azaz Add összeadás, Eq egyenlőségvizsgálat,
-- és Not Bool negáció legyen. Minden olyan esetben, ha a kifejezés típushibás,
-- legyen az eredmény Nothing.

-- Példák:
-- eval (Eq (IntLit 0) (BoolLit True)) == Nothing
-- eval (IntLit 10) == Just (Left 10)
-- eval (Add (IntLit 10) (IntLit 20)) == Just (Left 30)
-- eval (Not (IntLit 10)) == Nothing


-- Feladat 2: definiáld a következő függvényeket
------------------------------------------------------------

f1 :: Monad m => m (a -> b) -> m a -> m b
f1 = undefined

f2 :: Monad m => m (a -> b -> c) -> m a -> m b -> m c
f2 = undefined

f3 :: Monad m => m (m (m a)) -> m a
f3 = undefined

f4 :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f4 = undefined

f5 :: Monad m => m a -> (a -> b) -> m b
f5 = undefined

f6 :: Monad m => m a -> m b -> (a -> b -> m c) -> m c
f6 = undefined


-- State monád
------------------------------------------------------------

-- Feladat: számozzuk meg balról jobbra egy bináris fa leveleit!
-- Tipp: használj rekurzív segédfüggvényt a következő típussal:
--   Tree a -> Int -> (Tree (a, Int), Int)
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

labelTree :: Tree a -> Tree (a, Int)
labelTree = undefined

-- példák a működésre:

-- labelTree (Leaf True) == Leaf (True, 0)
-- labelTree (Node (Leaf True) (Leaf True)) == Node (Leaf (True, 0)) (Leaf (True, 1))
-- labelTree (Node (Node (Leaf True) (Leaf True)) (Leaf True))
--           == Node (Node (Leaf (True, 0)) (Leaf (True, 1))) (Leaf (True, 2))
-- labelTree (Node (Node (Leaf True) (Leaf True)) (Node (Leaf True) (Leaf True)))
--     == (Node (Node (Leaf (True, 0)) (Leaf (True, 1))) (Node (Leaf (True, 2)) (Leaf (True, 3))))
