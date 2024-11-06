import Control.Monad

-- Divianszky Peti jon nov 6-an!

-- Monad
{-
data Exp = Lit Int | Add Exp Exp | Mul Exp Exp

eval :: Exp -> Int
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

instance Show Exp where
  show (Lit i) = show i
  show (Add e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  show (Mul e1 e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"

exExp :: Exp
exExp = Add (Add (Lit 2) (Lit 3)) (Mul (Lit 0) (Lit 3))
-}

data Exp = Lit Int | Add Exp Exp | Mul Exp Exp | Div Exp Exp

safeDiv :: Int -> Int -> Maybe Int
safeDiv i j | j == 0    = Nothing
            | otherwise = Just (i `div` j)
{-
eval :: Exp -> Maybe Int
eval (Lit i) = Just i
eval (Add e1 e2) = case (eval e1, eval e2) of
  (Just i1, Just i2) -> Just (i1+i2)
  _                  -> Nothing
eval (Mul e1 e2) = case (eval e1, eval e2) of
  (Just i1, Just i2) -> Just (i1*i2)
  _                  -> Nothing
eval (Div e1 e2) = case (eval e1, eval e2) of
  (Just i1, Just i2) -> safeDiv i1 i2
  _                  -> Nothing
-}

-- monadok bevezetesevel ilyen szep kodot tudunk irni:
{-
eval :: Exp -> Maybe Int
eval (Lit i) = return i
eval (Add e1 e2) = do
  i1 <- eval e1
  i2 <- eval e2
  return (i1 + i2)
eval (Mul e1 e2) = do
  i1 <- eval e1
  i2 <- eval e2
  return (i1 * i2)
eval (Div e1 e2) = do
  i1 <- eval e1
  i2 <- eval e2
  safeDiv i1 i2
-}
{-
eval :: Exp -> Int
eval (Lit i) = i
eval (Add e1 e2) = (+) (eval e1) (eval e2)
eval (Mul e1 e2) = (*) (eval e1) (eval e2)
eval (Div e1 e2) = div (eval e1) (eval e2)
-}

-- applikativ funktor
eval :: Exp -> Maybe Int
eval (Lit i) = pure i
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2
{-
(+) :: Int -> Int -> Int
<$> (+) = fmap (+) :: Maybe Int -> Maybe (Int -> Int)
(+) <$> eval e1 :: Maybe (Int -> Int)
<*> :: Maybe (a -> b) -> Maybe a -> Maybe b

f :: a -> b -> c -> d
a :: Maybe a
b :: Maybe b
c :: Maybe c
--------------------------
f <$> :: Maybe a -> Maybe (b -> c -> d)
f <$> a : Maybe (b -> c -> d)
f <$> a <*> :: Maybe b -> Maybe (c -> d)
f <$> a <*> b :: Maybe (c -> d)
f <$> a <*> b <*> :: Maybe c -> Maybe d
f <$> a <*> b <*> c :: Maybe d

applikativ torveny: f <$> a = pure f <*> a
f            :: a -> b
pure f       :: Maybe (a -> b)
a            :: Maybe a
f <$> a      :: Maybe b
pure f <*> a :: Maybe b

-}
eval (Mul e1 e2) = (*) <$> eval e1 <*> eval e2
eval (Div e1 e2) = join (safeDiv <$> eval e1 <*> eval e2)
{-
safeDiv :: Int -> Int -> Maybe Int

(<*>) :: Maybe (Int -> Int -> Int) -> Maybe Int -> Maybe (Int -> Int)

(safeDiv <$> eval e1 <*> eval e2) :: Maybe (Maybe Int)

join :: Maybe (Maybe Int) -> Maybe Int

eval e1 :: Maybe Int
eval e2 :: Maybe Int
-}

{-
mellekhatasok nem explicitek:
  eval :: Exp -> Int
  eval (Lit i) = i
  eval (Add e1 e2) = (+) (eval e1) (eval e2)
  eval (Mul e1 e2) = (*) (eval e1) (eval e2)
  eval (Div e1 e2) = (div) (eval e1) (eval e2)                -- ez kivetelt dobhat

mellekhatasok explicitek:
  eval :: Exp -> Maybe Int
  eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2
  eval (Mul e1 e2) = (*) <$> eval e1 <*> eval e2
  eval (Div e1 e2) = join (safeDiv <$> eval e1 <*> eval e2)

Conor McBride-nak a Frank nevu kiserleti programozasi nyelve
-}

-- fmap :: (a -> b)       -> Maybe a -> Maybe b
maybeApp  :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeApp (Just f) (Just a) = Just (f a)
maybeApp _ _ = Nothing

-- Maybe = Error, Maybe a = a, de lehet mellekhatas is
-- [] = Nondeterminism, [a] = a, de lehet tobbfele is
--

data Id a = MkId { unId :: a } deriving (Functor, Show)

-- explicit applikacio:
egyInt, egyInt' :: Int
egyInt = (+3) 2
egyInt' = unId ((+3) <$> MkId 2)

-- funktor, de nem applikativ:

data Tag t a = MkTag a t
  deriving (Functor)

{-
instance Applicative (Tag t) where
  pure :: a -> Tag t a
  pure a = undefined -- nem tudjuk megadni
  (<*>) = undefined
-}

instance Monoid t => Applicative (Tag t) where
  pure :: a -> Tag t a
  pure a = MkTag a mempty -- kell a t-nek egy eleme
  (<*>) :: Tag t (a -> b) -> Tag t a -> Tag t b
  MkTag f t <*> MkTag a t' = MkTag (f a) (t `mappend` t')
  
-- Functor torvenyek
-- fmap id a = id <$> a = a
-- (f . g) <$> a = f <$> (g <$> a)

-- Functor instance a Maybe-re definicioja:
--   f <$> Nothing = Nothing
--   f <$> Just a  = Just (f a)
-- ellenorzom a funktor torvenyeket:
--   id <$> Nothing = Nothing
--   id <$> Just a  = Just (id a) = Just a
--   (f . g) <$> Nothing = Nothing = f <$> Nothing = f <$> (g <$> Nothing)
--   (f . g) <$> Just a = Just ((f . g) a) = Just (f (g a)) = f <$> (Just (g a))  = f <$> (g <$> Just a)

-- Functor instance listara, definicio:
--   f <$> []     = []
--   f <$> (a:as) = f a : f <$> as
-- ellenorzom a funktor torvenyeket, strukturalis indukcioval:
--   id <$> as = as
--   id <$> [] = []
--   id <$> (a:as) = id a : id <$> as = a : id <$> as =(i.h.) a : as

-- "fast and loose reasoning is morally correct"
