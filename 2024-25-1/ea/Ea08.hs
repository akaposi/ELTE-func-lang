{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (gcd)

-- pure  :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b

-- applikativ torvenyek:
-- pure id <*> v = v
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure f <*> pure x = pure (f x)
-- u <*> pure y = pure ($ y) <*> u

-- Tag x = (x,Int)

-- applikativ a funktor megszoritasa:
fmap0 :: Applicative f => a                         -> f a
fmap1 :: Applicative f => (a -> b)           -> f a -> f b
fmap2 :: Applicative f => (a -> b -> c)      -> f a -> f b -> f c
-- funktorral csak:       ((a,b)->c)         -> f (a,b)    -> f c
-- applikativ funktor lenyege, hogy (f a,f b) -> f (a,b)
-- funktorral tudunk f(a,b) -> (f a,f b)
fmap3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- ...

fmap0 = pure
fmap1 f a = pure f <*> a
fmap2 f a b = pure f <*> a <*> b
-- pure f :: f (a -> b -> c)
-- pure f <*> :: f a -> f (b -> c)
-- pure f <*> a :: f (b -> c)
-- pure f <*> a <*> : f b -> f c
fmap3 f a b c = pure f <*> a <*> b <*> c

prod :: [Int]->[Int]->[Int]
prod xs ys = pure (*) <*> xs <*> ys
--        pure (*) <*> xs <*> ys
--             (*) xs ys

-- Monad

-- return :: a -> f a
-- (<$>)      :: (a -> b)     -> f a -> f b
-- (<*>)      :: f (a -> b)   -> f a -> f b
-- id         :: (f a -> f b) -> f a -> f b
-- flip (>>=) :: (a -> f b)   -> f a -> f b

-- <*> -> <$>
-- f (a -> b)   -> f a -> f b
-- (a -> b)
-- f a
-----------------------------
-- f b
-- g <$> a := pure g <*> a

-- (>>=) -> <*>
-- f (a -> b)    ->  (a -> f b)
-- g             |->  \a -> fmap ($a) g
-- ($a) :: (a -> b) -> b
-- fmap ($a) :: f (a -> b) -> f b

-- minden monad funktor
bindToFun :: forall f .
             (forall a. a -> f a) ->
             (forall a b . (a -> f b) -> f a -> f b) ->
             forall a b .
             (a -> b) -> f a -> f b
bindToFun ret bind g u = bind (\a -> ret (g a)) u

-- minden monad applikativ
bindToApp :: forall f .
             (forall a. a -> f a) ->
             (forall a b . (a -> f b) -> f a -> f b) ->
             forall a b .
             f (a -> b) -> f a -> f b
bindToApp ret bind g u = bind (\a -> fmapp ($ a) g) u
  where
    fmapp = bindToFun ret bind

-- return = Just :: a -> Maybe a
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- Nothing >>= f = Nothing
-- Just a  >>  f = f a

-- return = (:[]) :: a -> [a]
-- (>>=) :: [a] -> (a -> [b]) -> [b]
-- []   >>= f = []
-- a:as >>= f = f a ++ as >>= f
-- as >>= f = concat $ map f as

-- Reader b = egy szamitas, amely fugg egy globalis valtozotol

newtype Reader b = Mk { un :: Int -> b }

-- a reader monad kulonleges kepessege: kiolvassuk a globalis valtozo erteket
ask :: Reader Int
ask = Mk id

instance Functor Reader where
  fmap f (Mk g) = Mk (f . g)

instance Applicative Reader where
  pure a = Mk (\_ -> a)
  Mk f <*> Mk a = Mk (\i -> f i (a i))

instance Monad Reader where
  (>>=) :: Reader a -> (a -> Reader b) -> Reader b
  Mk a >>= f = Mk (\i -> un (f (a i)) i)
{-
data Expr = Const Int | Expr :+: Expr

eval :: Expr -> Int
eval (Const n) = n
eval (e1 :+: e2) = eval e1 + eval e2
-}

{-
-- tobbvaltozos kifejezesnyelv
data Expr = Const Int | Expr :+: Expr | Var String

eval :: Expr -> (String -> Int) -> Int
eval (Const n)   env = n
eval (e1 :+: e2) env = eval e1 env + eval e2 env
eval (Var s)     env = env s
-}
-- egyvaltozos kifejezesnyelv
data Expr = Const Int | Expr :+: Expr | VarX
{-
eval :: Expr -> Int -> Int
eval (Const n) i = n
eval (e1 :+: e2) i = eval e1 i + eval e2 i
eval VarX i = i
-}
eval :: Expr -> Reader Int
eval (Const n) = return n
eval (e1 :+: e2) = pure (+) <*> eval e1 <*> eval e2
{-
eval (e1 :+: e2) = do
  i1 <- eval e1
  i2 <- eval e2
  return $ i1 + i2
-}
eval VarX = ask
