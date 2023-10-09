{-# OPTIONS_GHC -Wno-tabs -Wno-noncanonical-monad-instances #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor #-}

import Prelude
import Control.Applicative
import Data.Functor

data HTree a = HLeaf a | HNode (HTree a) (HTree a) deriving (Eq, Ord, Show, Functor)
data STree a = SLeaf | SNode (STree a) a (STree a) deriving (Eq, Ord, Show, Functor)

-- Monad
--------------------------------------------------------------------------------

-- Monad típusosztály:

{--

class Applicative m => Monad m where

	(>>=)  :: m a -> (a -> m b) -> m b

	return :: a -> m a
	return = pure

--}

-- Monad szabályok:

-- Bal identitás:    return a >>= k          == k a
-- Jobb identitás:   m >>= return            == m
-- Asszociativitás:  m >>= ((>>= h) . k)     == (m >>= k) >>= h

-- Monad & Functor szabály:

-- f <$> xs    == xs >>= return . f

-- Monad & Applicative szabály:

-- pure a      == return a

-- Monad & Applicative & Functor szabály:

-- <*> esetén:            m1 <*> m2          == m1         >>= (<$> m2)
-- liftA2-vel fogalmazva: liftA2 f m1 m2     == (f <$> m1) >>= (<$> m2)

-- Amit ebből meg kell jegyezni:
-- A return és >>= műveletekből a Functor és Applicative műveletek egyértelműen definiálhatók!

-- Ezt az alábbi példákban szemléltetjük.

data Maybe' a = Nothing' | Just' a deriving (Eq, Ord, Show)

instance Monad Maybe' where
	return :: a -> Maybe' a
	return = undefined

	(>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
	(>>=) = undefined


-- >>= és return használatával definiáljuk az alábbiakat.
liftM :: Monad m => (a -> b) -> (m a -> m b)
liftM = undefined

liftM2 :: Monad m => (a -> b -> c) -> (m a -> m b -> m c)
liftM2 = undefined

ap :: Monad m => m (a -> b) -> m a -> m b
ap = undefined

-- A fenti függvények segítségével. Ne használj konstruktort, se mintaillesztést!
instance Functor Maybe' where
	-- HLint: Használj <$>-t. Na persze...
	fmap :: (a -> b) -> (Maybe' a -> Maybe' b)
	fmap = undefined

-- Szintén a fenti függvények segítségével
instance Applicative Maybe' where
	pure :: a -> Maybe' a
	pure = undefined

	(<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
	(<*>) = undefined

	liftA2 :: (a -> b -> c) -> (Maybe' a -> Maybe' b -> Maybe' c)
	liftA2 = undefined

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

infixr 5 +++

-- Adott: Lista összefűzés operátor
(+++) :: List a -> List a -> List a
(+++) Nil c = c
(+++) (Cons a b) c = Cons a (b +++ c)

instance Monad List where
	return :: a -> List a
	-- Egy elemű lista
	return = undefined

	(>>=) :: List a -> (a -> List b) -> List b
	-- "bind" függvény előző óráról.
	-- Minden elemre alkalmazni kell a műveletet és az eredményeket össze kell fűzni.
	(>>=) = undefined

-- Gyakorlás: Definiáljuk az alábbiakat a Monad műveletek segítségével.
instance Applicative List where
	pure :: a -> List a
	pure = undefined

	(<*>) :: List (a -> b) -> List a -> List b
	(<*>) = undefined

	liftA2 :: (a -> b -> c) -> (List a -> List b -> List c)
	liftA2 = undefined

instance Functor List where
	fmap :: (a -> b) -> (List a -> List b)
	fmap = undefined

-- Monad feladatok
--------------------------------------------------------------------------------

-- Maybe - Képvisel egy determinisztikus számítást ami lehet hogy nem sikerül
-- Például: függvény értelmezési tartományán kívüli értékre van meghívva

guardMaybe :: Bool -> Maybe ()
guardMaybe False = Nothing
guardMaybe True = Just ()

-- Ha a függvény bármire Nothing-ot ad, az eredmény legyen Nothing.
-- Egyébként a függvény alapján módosítjuk a fát.
-- Ezúttal Monad vagy Applicative műveletekkel!
mapMaybeHTree :: (a -> Maybe b) -> HTree a -> Maybe (HTree b)
mapMaybeHTree = undefined

-- Do notation példa:
mapMaybeList :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybeList f [] = Just []
mapMaybeList f (x:xs) = do
	y <- f x                    -- f x               >>= \y ->
	ys <- mapMaybeList f xs     -- mapMaybeList f xs >>= \ys ->
	return (y:ys)               -- return (y:ys)

-- Do notation használat!
mapMaybeSTree :: (a -> Maybe b) -> STree a -> Maybe (STree b)
mapMaybeSTree = undefined

-- Ha a függvény bármire Nothing-ot ad, az eredmény legyen Nothing.
-- Egyébként a függvény alapján zip-elünk.
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe = undefined

-- A / operátorral osszuk az első paramétert a másodikkal, kivéve ha a második 0.
-- Használjuk a guardMaybe függvényt!
safeDivide :: (Eq a,Fractional a) => a -> a -> Maybe a
safeDivide = undefined

-- [] - Képvisel egy nem determinisztikus számítást aminek több lehetséges eredménye is lehet
-- Tehát 0, 1, vagy több eredmény van és minden lehetséges értéket tovább viszünk

-- Mindegyik listából kiválasztunk egy elemet. Monad műveletekkel!
-- Példa: combinations [[1,2,3],[4,5]] == [[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]]
combinations :: [[a]] -> [[a]]
combinations = undefined

-- A bemenet lista minden lehetséges részlistája álljon elő.
-- Példa: sublists [1, 2]    == [[], [1], [2], [1,2]]
-- Példa: sublists [1, 2, 3] == [[], [1], [2], [3], [1,2], [1,3], [2,3], [1,2,3]]
sublists :: [a] -> [[a]]
sublists = undefined

-- Bónusz: A bemenet lista minden lehetséges permutációja álljon elő.
-- Emlékeztető: take, drop és [n..k] szintaxis
shuffle :: [a] -> [[a]]
shuffle = undefined

-- A lista elemeit összegezzük
foldTraverse :: (Monad m,Monoid k) => [m k] -> m k
foldTraverse = undefined

-- State Monad
--------------------------------------------------------------------------------

-- Eddig volt:
-- Maybe - Képvisel egy determinisztikus számítást ami lehet hogy nem sikerül
-- List - Képvisel egy nem determinisztikus számítást aminek több lehetséges eredménye is lehet

-- Most lesz:
-- State - Képvisel egy számítást ami valamilyen állapoton változat

-- Jövőben:
-- IO - Képvisel egy számítást ami a "való világ" állapotán változtat
-- Parser - Képvisel egy számítást ami egy bemenet szöveget értelmez

-- runState: Egy függvény bemenet állapottal
newtype State s a = State {runState :: s -> (a, s)} deriving Functor

instance Applicative (State s) where
	(<*>) :: State s (a -> b) -> State s a -> State s b
	(<*>) = ap
	pure :: a -> State s a
	pure = State . (,)

instance Monad (State s) where
	return :: a -> State s a
	return = State . (,)
	(>>=) :: State s a -> (a -> State s b) -> State s b
	(>>=) (State f1) f = State $
		\s0 -> let
			(x1, s1) = f1 s0
			(State f2) = f x1
			in f2 s1

-- A jelenlegi állapot értéket kiolvassuk
get :: State s s
get = State (\s -> (s, s))

-- Beállítunk egy új állapot értéket
put :: s -> State s ()
put s = State (const ((), s))

-- Eredmény érdekel minket
evalState :: State s a -> s -> a
evalState ma = fst . runState ma

-- Végső állapot érdekel minket
execState :: State s a -> s -> s
execState ma = snd . runState ma

-- Alkalmazzunk egy függvényt az állapotra
modify :: (s -> s) -> State s ()
modify = undefined

-- A listának minden értékét címkézzük fel egy számmal
-- A State kezdőállapota legyen az első címke, a többi címke ennek a léptetése.
labelList :: (Num n) => [a] -> State n [(n,a)]
labelList = undefined

-- A fának minden értékét címkézzük fel egy számmal
-- A State kezdőállapota legyen az első címke, a többi címke ennek a léptetése.
labelHTree :: (Num n) => HTree a -> State n (HTree (n,a))
labelHTree = undefined

-- További State feladatok
--------------------------------------------------------------------------------

-- A "verem" tetejére teszünk egy elemet
push :: a -> State [a] ()
push = undefined

-- A "verem" tetejéről leveszünk egy elemet
pop :: State [a] (Maybe a)
pop = undefined

-- A veremből kiveszünk egy elemet, majd vissza tesszük a függvény eredményét
fn1 :: (a -> a) -> State [a] ()
fn1 = undefined

-- A veremből kiveszünk két elemet, majd vissza tesszük a függvény eredményét
fn2 :: (a -> a -> a) -> State [a] ()
fn2 = undefined

-- Postfix notation
data StackOp n = Lit n | Neg | Add | Mul

runStackOp :: (Num n) => StackOp n -> State [n] ()
-- Egy értéket beteszünk a verembe
runStackOp (Lit n) = undefined
-- A verem tetején levő elemet negáljuk (negate függvény)
runStackOp Neg = undefined
-- A verem tetején levő két elemet összegezzük. Pl: [2,3,...] -> [5,...]
runStackOp Add = undefined
-- A verem tetején levő két elemet összeszorozzuk. Pl: [2,3,...] -> [6,...]
runStackOp Mul = undefined

-- Hajtsuk végre a listában levő műveleteket egymás után
executeStackOps :: (Num n) => [StackOp n] -> State [n] ()
executeStackOps = undefined

e1 :: (Num n) => [StackOp n]
e1 = [Lit 12,Neg,Lit 3,Lit 1,Lit 2,Lit 3,Mul,Add,Mul,Lit 2,Mul,Add]

