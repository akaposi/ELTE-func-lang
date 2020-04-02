

import Control.Monad.State

-- Feladat 1: tekintsük a következő típust, aminek az elemei egyszerű kifejezésfák:
data Exp = IntLit Int | BoolLit Bool | Add Exp Exp | Not Exp | Eq Exp Exp
  deriving (Show)

-- pl.
e1 = Add (IntLit 0) (IntLit 10) -- 0 + 10
e2 = Eq (IntLit 0) e1           -- 0 == (a + 10)
e3 = Not e2                     -- not (0 == (0 + 10))
e4 = Not (BoolLit True)



-- Írjunk egy függvényt, ami egy Exp kifejezést kiértékel Either Int Bool-ra!
-- A kiértékelés legyen értelemszerű, azaz Add összeadás, Eq egyenlőségvizsgálat,
-- és Not Bool negáció legyen. Minden olyan esetben, ha a kifejezés típushibás,
-- legyen az eredmény Nothing.

-- Példák:
-- eval (Eq (IntLit 0) (BoolLit True)) == Nothing
-- eval (IntLit 10) == Just (Left 10)
-- eval (Add (IntLit 10) (IntLit 20)) == Just (Left 30)
-- eval (Not (IntLit 10)) == Nothing

-- használjuk Maybe (>>=), pure/return, (<$>)-is elérhető     (return = Just = pure)
eval :: Exp -> Maybe (Either Int Bool)
eval (IntLit n)  = pure (Left n)
eval (BoolLit b) = pure (Right b)
eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e1
  case (v1, v2) of
    (Left n1, Left n2) -> pure (Left (n1 + n2))
    _                  -> Nothing
  -- eval e1 >>= \v1 ->
  -- eval e2 >>= \v2 ->
  -- case (v1, v2) of
  --   (Left n1, Left n2) -> pure (Left (n1 + n2))
  --   _                  -> Nothing
eval (Not e) =
  eval e >>= \v ->
  case v of
    Left _  -> Nothing
    Right b -> pure (Right (not b))
eval (Eq e1 e2) =
  eval e1 >>= \v1 ->
  eval e2 >>= \v2 ->
  case (v1, v2) of
    (Left n1,  Left n2)  -> pure (Right (n1 == n2))
    (Right b1, Right b2) -> pure (Right (b1 == b2))
    _                    -> Nothing
-- (példák működnek)
-- (3 vagy több dologra minta: tuple-re case)

-- do notáció:
--    szintaktikus cukorka (>>=)-ra és (>>)


-- első cukorka:

-- do
--   a <- e

-- fordítás:

-- e >>= \a ->


-- második:

-- do
--   e1
--   e2

-- e1 >>
-- e2

-- konkrét példa IO-ban:

ex1 :: IO ()
ex1 = do
  putStrLn "hello"
  putStrLn "hello"
  putStrLn "hello"

ex1' :: IO ()
ex1' =
  putStrLn "hello" >>
  putStrLn "hello" >>
  putStrLn "hello"

ex2 :: IO ()
ex2 = do
  l <- getLine
  putStrLn (l ++ l)

ex2' :: IO ()
ex2' =
  getLine >>= \l ->
  putStrLn (l ++ l)


-- Feladat 2: definiáld a következő függvényeket
------------------------------------------------------------

-- superclass: Functor superclass-ja a Monad-nak
-- ha Monad m, akkor Functor m
-- Functor => Applicative => Monad

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- pure  :: Monad m => a -> m a
-- fmap  :: Monad m => (a -> b) -> m a -> m b

f1 :: Monad m => m (a -> b) -> m a -> m b
f1 mf ma =
  mf >>= \f ->
  ma >>= \a ->
  pure (f a)

-- f1 mf ma = do
--   f <- mf
--   a <- ma
--   pure (f a)

f2 :: Monad m => m (a -> b -> c) -> m a -> m b -> m c
f2 mf ma mb = do
  f <- mf
  a <- ma
  b <- mb
  pure (f a b)

  -- mf >>= \f ->
  -- ma >>= \a ->
  -- mb >>= \b ->
  -- pure (f a b)

f3 :: Monad m => m (m (m a)) -> m a
f3 mmma =
  mmma >>= \mma ->
  mma  >>= \ma ->
  ma   >>= \a  ->
  pure a                -- monád törvény szerint: bind után pure elhagyható

-- f3' mmma = do
--   mma <- mmma
--   ma  <- mma
--   ma

  -- mmma >>= (\mma ->
  -- mma  >>= (\ma ->
  -- ma   >>= (\a  ->
  -- pure a)))

-- kompozíció
-- előadáson: monádikus kompozíció
-- standard neve: (>=>)  (fish)
f4 :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f4 amb bmc a = amb a >>= bmc

f5 :: Monad m => m a -> (a -> b) -> m b  -- fmap
f5 ma f = fmap f ma
-- f5 = flip fmap

-- (>>=) :: m a -> (a -> m b) -> m b
f6 :: Monad m => m a -> m b -> (a -> b -> m c) -> m c
f6 ma mb f = ma >>= \a ->
             mb >>= \b ->
             f a b

-- f6 ma mb f = do
--   a <- ma
--   b <- mb
--   f a b

-- State monád
------------------------------------------------------------

-- newtype State s a = ?

-- instance Monad (State s) where
-- put      :: s -> State s ()             -- beír egy értéket állapotnak
-- get      :: State s s                   -- olvassa a jelenlegi állapot értékét
-- runState :: State s a -> s -> (a, s)    -- State műveletnek kezdő állapotot adunk,
--                                            akkor megkapjuk a végeredményt


-- Feladat: számozzuk meg balról jobbra egy bináris fa leveleit!
-- Tipp: használj rekurzív segédfüggvényt a következő típussal:
--   Tree a -> Int -> (Tree (a, Int), Int)
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

labelTree :: Tree a -> Tree (a, Int)
labelTree t = fst (go t 0) where

  -- (rekurzív segédfüggvény neve gyakran "go")
  go :: Tree a -> Int -> (Tree (a, Int), Int)
  go (Leaf a)   n = (Leaf (a, n), n+1)
  go (Node l r) n = case go l n of
    (l', n') -> case go r n' of
      (r', n'') -> (Node l' r', n'')
  -- macerás: kézzel kell tovább adni extra paramétert
  -- mindig a "jó" paramétert kell visszaadni (nehéz refaktorálni, mert
  --     át kell írni az extra paramétereket)
  -- ebben az esetben kényelmesebb és világosabb, ha mutációt használunk

-- State-el:
labelTree' :: Tree a -> Tree (a, Int)
labelTree' t = fst (runState (go t) 0) where
  go :: Tree a -> State Int (Tree (a, Int))
  go (Leaf a) = do
    n <- get
    put (n + 1)
    pure (Leaf (a, n))
  go (Node l r) = do
    l' <- go l
    r' <- go r
    pure (Node l' r')


-- példák a működésre:

-- labelTree (Leaf True) == Leaf (True, 0)
-- labelTree (Node (Leaf True) (Leaf True)) == Node (Leaf (True, 0)) (Leaf (True, 1))
-- labelTree (Node (Node (Leaf True) (Leaf True)) (Leaf True))
--           == Node (Node (Leaf (True, 0)) (Leaf (True, 1))) (Leaf (True, 2))
-- labelTree (Node (Node (Leaf True) (Leaf True)) (Node (Leaf True) (Leaf True)))
--     == (Node (Node (Leaf (True, 0)) (Leaf (True, 1))) (Node (Leaf (True, 2)) (Leaf (True, 3))))
