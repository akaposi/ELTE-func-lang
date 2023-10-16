{-# OPTIONS_GHC -Wno-tabs -Wno-noncanonical-monad-instances #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor #-}

import Data.ByteString.Internal
import Control.Monad(ap)
import Prelude hiding (Traversable(..))

data HTree a = HLeaf a | HNode (HTree a) (HTree a) deriving (Eq, Ord, Functor, Foldable)
data STree a = SLeaf | SNode (STree a) a (STree a) deriving (Eq, Ord, Functor, Foldable)

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
modify f = get >>= put . f

-- IO monád
--------------------------------------------------------------------------------

-- Input függvények

-- getChar     :: IO Char   -- Be olvas egy karaktert
-- getLine     :: IO String -- Be olvas egy sort

-- Output függvények

-- putChar  :: Char -> IO ()        -- Ki ír egyetlen karaktert
-- putStr   :: String -> IO ()      -- Ki ír egy szöveget
-- putStrLn :: String -> IO ()      -- Ki ír egy szöveget majd sortörést
-- print    :: Show a => a -> IO () -- Ki ír egy értéket majd sortörést

-- Az IO monád egy számítást képvisel ami a "való világ" állapotán változtat.
-- Két módon lehet IO-t futtatni:
-- - GHCI-ben kiértékeled
-- - main :: IO () függvény ha ghc fordítót használod

-- IO egy mágikus dolog.
-- De ahol van mágia ott van fekete mágia is.
-- ??? :: IO a -> a

io0 :: IO ()
io0 = do
	putStrLn "Hogy hívnak?"
	név <- getLine
	putStrLn $ "Szia " <> név <> "!"

-- Írj egy függvényt, ami beolvas egy sort, majd ki írja a sorban található
-- 'a' betűk számát.
io1 :: IO ()
io1 = undefined

-- Írj egy függvényt, ami beolvas egy sort, majd ki írja annyiszor,
-- ahány karakter van a sorban!
io2 :: IO ()
io2 = undefined

-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert.
io3 :: IO ()
io3 = undefined

-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert. Ha a sorban 'x' van, akkor a program nyomtassa ki az
-- összes eddig beolvasott sort.
io4 :: IO ()
io4 = undefined

-- A következőt ismételd végtelenül: olvass be egy sort, majd nyomtasd ki a
-- sorban a kisbetűk számát.  A Ctrl + C kombinációval lehet megszakítani a futtatást
-- ghci-ben.
io5 :: IO ()
io5 = undefined

-- Traversable
--------------------------------------------------------------------------------

-- Bejárható adattípusok

-- Adatszerkezetek amik elemein végig tudunk menni egy monadikus művelettel.

class (Functor t, Foldable t) => Traversable t where
	{-# MINIMAL traverse | sequenceA #-}

	traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
	traverse f = sequenceA . fmap f

	sequenceA :: Applicative f => t (f a) -> f (t a)
	sequenceA = traverse id

	mapM :: Monad m => (a -> m b) -> t a -> m (t b)
	mapM = traverse

	sequence :: Monad m => t (m a) -> m (t a)
	sequence = sequenceA

-- Functor-hoz hasonlóan a bejárás nem változtathat a szerkezeten, csak az elemeket cseréli le!
-- Functor-ral ellentétben bejáráskor az eredmények függhetnek egymástól!

-- Példa listára
instance Traversable [] where
	sequenceA :: Applicative f => [f a] -> f [a]
	sequenceA [] = pure []
	sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

	traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
	traverse f [] = pure []
	traverse f (x:xs) = (:) <$> f x <*> traverse f xs

	mapM :: Monad m => (a -> m b) -> [a] -> m [b]
	mapM f [] = return []
	mapM f (x:xs) = do
		y <- f x
		ys <- mapM f xs
		return (y:ys)
	
	sequence :: Monad m => [m a] -> m [a]
	sequence [] = return []
	sequence (x:xs) = do
		y <- x
		ys <- sequence xs
		return (y:ys)

-- A Functor és Foldable típusosztályokhoz hasonlóan itt is egy csomószor egyértelmű a definíció.
-- A sequenceA-t a legkényelmesebb erre hazsnálni.
-- A többi ez alapján definiálható, és <$> miatt nincs szükség függvény bemenetre.

data X1 a b = X1 Integer b (a b) deriving (Functor,Foldable)
data X2 a b = Nil | Val Integer | Sin b | Com (a b) deriving (Eq,Ord,Show,Functor,Foldable)

instance Traversable a => Traversable (X1 a) where
	sequenceA :: Applicative f => X1 a (f b) -> f (X1 a b)
	-- C x y z         ==>  C <$> x' <*> y' <*> z'       -- ahol C egy konstruktor
	-- x :: c          ==>  x' = pure x      :: f c      -- ahol c irreleváns típus
	-- y :: f b        ==>  y' = y           :: f b
	-- z :: a (f b)    ==>  z' = sequenceA z :: f (a b)  -- ahol a egy bejárható típus
	sequenceA = undefined

instance Traversable a => Traversable (X2 a) where
	sequenceA :: Applicative f => X2 a (f b) -> f (X2 a b)
	sequenceA = undefined

instance Traversable Maybe where
	sequenceA :: Applicative f => Maybe (f a) -> f (Maybe a)
	sequenceA = undefined

instance Traversable HTree where
	sequenceA :: Applicative f => HTree (f a) -> f (HTree a)
	sequenceA = undefined

instance Traversable STree where
	sequenceA :: Applicative f => STree (f a) -> f (STree a)
	sequenceA = undefined

-- A bejárható adatszerkezet minden értékét címkézzük fel egy számmal
-- A State kezdőállapota legyen az első címke, a többi címke ennek a léptetése.
label :: (Num n,Traversable t) => t a -> State n (t (n,a))
label = undefined

newtype Identity a = Identity {fromIdentity :: a} deriving (Eq, Ord, Show, Functor, Foldable)
newtype Const a b = Const {fromConst :: a} deriving (Eq, Ord, Show, Functor, Foldable)

instance Traversable Identity where
	sequenceA :: Applicative f => Identity (f a) -> f (Identity a)
	sequenceA = undefined

-- Adott Applicative (Const a)
instance (Monoid a) => Applicative (Const a) where
	pure :: b -> Const a b
	pure _ = Const mempty
	(<*>) :: Const a (b -> c) -> Const a b -> Const a c
	(<*>) (Const a) (Const b) = Const (a <> b)

instance Traversable (Const a) where
	sequenceA :: Applicative f => Const a (f b) -> f (Const a b)
	sequenceA = undefined

-- Alakítsunk át egy bejárható adatszerkezetet listává Foldable függvények nélkül.
-- Használjuk a Const típust.
toList :: (Traversable t) => t a -> [a]
toList = undefined

-- Definiáljuk a Foldable függvényeket Foldable függvények használata nélkül!
-- Tipp: Tudjuk-e a Const-ot használni itt?
fold :: (Monoid m,Traversable t) => t m -> m
fold = undefined

foldMap :: (Monoid m,Traversable t) => (a -> m) -> t a -> m
foldMap = undefined

foldl' :: (Traversable t) => (b -> a -> b) -> b -> t a -> b
foldl' = undefined

-- Bónusz: foldr (kicsit nehezebb)
foldr' :: (Traversable t) => (a -> b -> b) -> b -> t a -> b
foldr' = undefined

-- Írjuk ki az összes elemet, soronként egyet.
printLines :: (Traversable t,Show a) => t a -> IO ()
printLines = undefined

-- További Traversable feladatok
--------------------------------------------------------------------------------

data Pit a = PNil | PLeft a (Pit a) | PRight (Pit a) a deriving (Show, Functor, Foldable)
data RoseTree a = RoseNode a [RoseTree a] deriving (Show, Functor, Foldable)
data Sum f g a = Inl (f a) | Inr (g a) deriving (Show, Functor, Foldable)
data Product f g a = Product (f a) (g a) deriving (Show, Functor, Foldable)
newtype Compose f g a = Compose (f (g a)) deriving (Show, Functor, Foldable)

instance Traversable Pit where
	sequenceA :: Applicative t => Pit (t a) -> t (Pit a)
	sequenceA = undefined

instance Traversable RoseTree where
	sequenceA :: Applicative t => RoseTree (t a) -> t (RoseTree a)
	sequenceA = undefined

instance (Traversable f,Traversable g) => Traversable (Sum f g) where
	sequenceA :: Applicative t => Sum f g (t a) -> t (Sum f g a)
	sequenceA = undefined

instance (Traversable f,Traversable g) => Traversable (Product f g) where
	sequenceA :: Applicative t => Product f g (t a) -> t (Product f g a)
	sequenceA = undefined

instance (Traversable f,Traversable g) => Traversable (Compose f g) where
	sequenceA :: Applicative t => Compose f g (t a) -> t (Compose f g a)
	sequenceA = undefined
