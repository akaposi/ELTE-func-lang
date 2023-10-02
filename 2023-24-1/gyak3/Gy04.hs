{-# OPTIONS_GHC -Wno-tabs #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor #-}

import Prelude hiding (Applicative(..))

infixl 4 <*>, <*


data HTree a = HLeaf a | HNode (HTree a) (HTree a) deriving (Eq, Ord, Show, Functor)
data STree a = SLeaf | SNode (STree a) a (STree a) deriving (Eq, Ord, Show, Functor)

-- Functorok, megint
--------------------------------------------------------------------------------

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = undefined

apply :: Functor f => f (a -> b) -> a -> f b
apply = undefined

first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first = undefined

match :: (Functor f,Functor g) => (a -> b -> c) -> f a -> g b -> f (g c)
match = undefined

replace :: (Functor f) => a -> f b -> f a
replace = undefined

-- Maybe használat
--------------------------------------------------------------------------------

-- Ha a függvény bármire Nothing-ot ad, az eredmény legyen Nothing.
-- Egyébként a függvény alapján szűrjük a listát.
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe = undefined

-- Lift1: "Felemelünk" egy függvényt a Maybe típusra
liftM1 :: (a -> b) -> (Maybe a -> Maybe b)
liftM1 = undefined

-- Lift2: Ezúttal bináris függvény
liftM2 :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)
liftM2 = undefined

-- Ha a függvény bármire Nothing-ot ad, az eredmény legyen Nothing.
-- Egyébként a függvény alapján módosítjuk a listát.
mapMaybeTree :: (a -> Maybe b) -> HTree a -> Maybe (HTree b)
mapMaybeTree = undefined

-- Ha a függvény bármire Nothing-ot ad, az eredmény legyen Nothing.
-- Egyébként a függvény alapján zip-elünk.
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe = undefined

-- Ha a függvény bármire Nothing-ot ad, az eredmény legyen Nothing.
foldrMaybe :: (Foldable f) => (a -> b -> Maybe b) -> b -> f a -> Maybe b
foldrMaybe = undefined

-- Hasonló feladatok listákra
--------------------------------------------------------------------------------

-- Minden elemet lecserélünk egy vagy több elemmel.
bind :: (a -> [b]) -> [a] -> [b]
bind = undefined

-- Lift1: "Felemelünk" egy függvényt a Lista típusra
liftL1 :: (a -> b) -> ([a] -> [b])
liftL1 = undefined

-- Lift2: "Felemelünk" egy bináris függvényt a listára
-- Erre van néhány értelmezés...
liftL2 :: (a -> b -> c) -> ([a] -> [b] -> [c])
liftL2 = undefined

liftL2' :: (a -> b -> c) -> ([a] -> [b] -> [c])
liftL2' = undefined

-- Applicative
--------------------------------------------------------------------------------

-- Általános "lift" függvények
class Functor f => Applicative f where
	{-# MINIMAL pure, (liftA2 | (<*>)) #-}
	
	pure :: a -> f a

	liftA2 :: (a -> b -> c) -> (f a -> f b -> f c)
	liftA2 f a b = f <$> a <*> b

	(<*>) :: f (a -> b) -> f a -> f b
	(<*>) = liftA2 id

-- Applicative szabályok

-- f <$> a <*> b                  == liftA2 f a b
-- pure f  <*> pure x             == pure (f x)
-- pure id <*> a                  == a
-- ((pure (.) <*> a) <*> b) <*> c == a <*> (b <*> c)
-- u <*> pure y                   == pure ($ y) <*> u

-- <*> nem asszociatív.
-- <*> nem kommutatív.

-- Definiáljuk az "fmap" függvényt a fenti három segítségével.
fmap' :: (Applicative f) => (a -> b) -> f a -> f b
fmap' = undefined

instance Applicative Maybe where
	pure :: a -> Maybe a
	pure = undefined

	liftA2 :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)
	liftA2 = undefined

	(<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
	(<*>) = undefined

pair2 :: (Applicative f) => f a -> f b -> f (a,b)
pair2 = undefined

pair3 :: (Applicative f) => f a -> f b -> f c -> f (a,b,c)
pair3 = undefined

-- További operátorok
(<*) :: (Applicative f) => f a -> f b -> f a
(<*) = liftA2 const

(*>) :: (Applicative f) => f a -> f b -> f b
(*>) = liftA2 (flip const)

-- A Data.Functor-ban létezik <$ és $> is!

instance Applicative [] where
	pure :: a -> [a]
	pure = undefined

	-- Minden elempárra alkalmazzuk!
	-- Példa: liftA2 (*) [2,3,5] [7,11] == [14,22,21,33,35,55]
	-- Példa: liftA2 (++) ["A","B"] ["C","D","E","F"] == ["AC","AD","AE","AF","BC","BD","BE","BF"]
	liftA2 :: (a -> b -> c) -> [a] -> [b] -> [c]
	liftA2 = undefined

	(<*>) :: [a -> b] -> [a] -> [b]
	(<*>) = undefined

-- Mindegyik listából kiválasztunk egy elemet
-- Példa: combinations [[1,2,3],[4,5]] == [[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]]
combinations :: [[a]] -> [[a]]
combinations = undefined

instance Applicative HTree where
	pure :: a -> HTree a
	pure = HLeaf
	
	liftA2 :: (a -> b -> c) -> (HTree a -> HTree b -> HTree c)
	liftA2 = undefined

	(<*>) :: HTree (a -> b) -> HTree a -> HTree b
	(<*>) = undefined

data Fun a b = Fun (a -> b) deriving Functor
data Compose f g a = Compose (f (g a)) deriving (Eq,Show,Functor)
newtype ZipList a = ZipList [a] deriving (Eq,Show,Functor)
data NonEmpty a = Last a | Cons a (NonEmpty a) deriving (Eq,Show,Functor)

instance Applicative (Fun x) where
	pure :: a -> Fun x a
	pure = undefined

	liftA2 :: (a -> b -> c) -> (Fun x a -> Fun x b -> Fun x c)
	liftA2 = undefined

	(<*>) :: Fun x (a -> b) -> Fun x a -> Fun x b
	(<*>) = undefined

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
	pure :: a -> Compose f g a
	pure = undefined

	liftA2 :: (a -> b -> c) -> (Compose f g a -> Compose f g b -> Compose f g c)
	liftA2 = undefined

	(<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
	(<*>) = undefined

-- Bónusz
instance Applicative ZipList where
	-- Ezek adottak
	liftA2 :: (a -> b -> c) -> (ZipList a -> ZipList b -> ZipList c)
	liftA2 f (ZipList x) (ZipList y) = ZipList $ zipWith f x y

	(<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
	(<*>) (ZipList x) (ZipList y) = ZipList $ zipWith ($) x y

	-- pure?
	pure :: a -> ZipList a
	pure = undefined

instance Applicative NonEmpty where
	-- Bónusz: Működjön az == operátor az eredményen!
	pure :: a -> NonEmpty a
	pure = undefined

	liftA2 :: (a -> b -> c) -> (NonEmpty a -> NonEmpty b -> NonEmpty c)
	liftA2 = undefined

	(<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
	(<*>) = undefined

instance Applicative (Either x) where
	pure :: a -> Either x a
	pure = undefined

	liftA2 :: (a -> b -> c) -> (Either x a -> Either x b -> Either x c)
	liftA2 = undefined

	(<*>) :: Either x (a -> b) -> Either x a -> Either x b
	(<*>) = undefined
