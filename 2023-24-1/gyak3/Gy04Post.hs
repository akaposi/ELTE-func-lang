{-# OPTIONS_GHC -Wno-tabs #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor #-}

import Prelude hiding (Applicative(..))

infixl 4 <*>, <*


data HTree a = HLeaf a | HNode (HTree a) (HTree a) deriving (Eq, Ord, Show, Functor)
data STree a = SLeaf | SNode (STree a) a (STree a) deriving (Eq, Ord, Show, Functor)

-- Functorok, megint
--------------------------------------------------------------------------------

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip f = (fmap fst f,fmap snd f)

apply :: Functor f => f (a -> b) -> a -> f b
apply f a = ($ a) <$> f

first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first f (a, c) = (\x -> (x , c)) <$> f a

match :: (Functor f,Functor g) => (a -> b -> c) -> f a -> g b -> f (g c)
match f fa gb = (\x -> f x <$> gb) <$> fa

replace :: (Functor f) => a -> f b -> f a
replace a = fmap (const a)

-- Maybe használat
--------------------------------------------------------------------------------

-- Ha a függvény bármire Nothing-ot ad, az eredmény legyen Nothing.
-- Egyébként a függvény alapján szűrjük a listát.
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f [] = Just []
filterMaybe f (x:xs) = case f x of
	Nothing -> Nothing
	Just False -> filterMaybe f xs
	Just True -> case filterMaybe f xs of
		Just k -> Just (x:k)
		Nothing -> Nothing

-- Lift1: "Felemelünk" egy függvényt a Maybe típusra
liftM1 :: (a -> b) -> (Maybe a -> Maybe b)
liftM1 = fmap

-- Lift2: Ezúttal bináris függvény
liftM2 :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)
liftM2 f Nothing _ = Nothing
liftM2 f _ Nothing = Nothing
liftM2 f (Just a) (Just b) = Just (f a b)

-- Ha a függvény bármire Nothing-ot ad, az eredmény legyen Nothing.
-- Egyébként a függvény alapján módosítjuk a listát.
mapMaybeTree :: (a -> Maybe b) -> HTree a -> Maybe (HTree b)
mapMaybeTree f (HLeaf a) = HLeaf <$> f a
mapMaybeTree f (HNode a b) = liftM2 HNode (mapMaybeTree f a) (mapMaybeTree f b)

-- Ha a függvény bármire Nothing-ot ad, az eredmény legyen Nothing.
-- Egyébként a függvény alapján zip-elünk.
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe f [] _ = Just []
zipWithMaybe f _ [] = Just []
zipWithMaybe f (x:xs) (y:ys) = case f x y of
	Nothing -> Nothing
	Just z -> case zipWithMaybe f xs ys of
		Nothing -> Nothing
		Just zs -> Just (z:zs)

-- Ha a függvény bármire Nothing-ot ad, az eredmény legyen Nothing.
foldrMaybe :: (Foldable f) => (a -> b -> Maybe b) -> b -> f a -> Maybe b
foldrMaybe f z t = foldr helper (Just z) t where
	helper x Nothing = Nothing
	helper x (Just y) = f x y

-- Hasonló feladatok listákra
--------------------------------------------------------------------------------

-- Minden elemet lecserélünk egy vagy több elemmel.
bind :: (a -> [b]) -> [a] -> [b]
bind f = foldr ((++) . f) []

-- Lift1: "Felemelünk" egy függvényt a Lista típusra
liftL1 :: (a -> b) -> ([a] -> [b])
liftL1 = fmap

-- Lift2: "Felemelünk" egy bináris függvényt a listára
-- Erre van néhány értelmezés...
liftL2 :: (a -> b -> c) -> ([a] -> [b] -> [c])
liftL2 f [] _ = []
liftL2 f (x:xs) y = (f x <$> y) ++ (liftL2 f xs y)

liftL2' :: (a -> b -> c) -> ([a] -> [b] -> [c])
liftL2' = zipWith

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
fmap' f a = pure f <*> a

instance Applicative Maybe where
	pure :: a -> Maybe a
	pure = Just

	liftA2 :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)
	liftA2 = liftM2

	(<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
	(<*>) = liftM2 id

liftA3 :: (Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = pure f <*> a <*> b <*> c

pair2 :: (Applicative f) => f a -> f b -> f (a,b)
pair2 = liftA2 (,)

pair3 :: (Applicative f) => f a -> f b -> f c -> f (a,b,c)
pair3 = liftA3 (,,)

-- További operátorok
(<*) :: (Applicative f) => f a -> f b -> f a
(<*) = liftA2 const

(*>) :: (Applicative f) => f a -> f b -> f b
(*>) = liftA2 (flip const)

-- A Data.Functor-ban létezik <$ és $> is!

instance Applicative [] where
	pure :: a -> [a]
	pure a = [a]

	-- Minden elempárra alkalmazzuk!
	-- Példa: liftA2 (*) [2,3,5] [7,11] == [14,22,21,33,35,55]
	-- Példa: liftA2 (++) ["A","B"] ["C","D","E","F"] == ["AC","AD","AE","AF","BC","BD","BE","BF"]
	liftA2 :: (a -> b -> c) -> [a] -> [b] -> [c]
	liftA2 f [] _ = []
	liftA2 f (x:xs) y = (f x <$> y) <> (liftA2 f xs y)

	(<*>) :: [a -> b] -> [a] -> [b]
	(<*>) [] _ = []
	(<*>) (x:xs) y = (x <$> y) <> (xs <*> y)

-- Mindegyik listából kiválasztunk egy elemet
-- Példa: combinations [[1,2,3],[4,5]] == [[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]]
combinations :: [[a]] -> [[a]]
combinations [] = [[]]
combinations (x:xs) = liftA2 (:) x (combinations xs)

instance Applicative HTree where
	pure :: a -> HTree a
	pure = HLeaf
	
	liftA2 :: (a -> b -> c) -> (HTree a -> HTree b -> HTree c)
	liftA2 f (HNode a b) (HNode c d) = HNode (liftA2 f a c) (liftA2 f b d)
	liftA2 f (HLeaf a) (HLeaf c) = HLeaf (f a c)
	liftA2 f a (HNode c d) = HNode (liftA2 f a c) (liftA2 f a d)
	liftA2 f (HNode a b) c = HNode (liftA2 f a c) (liftA2 f b c)

	(<*>) :: HTree (a -> b) -> HTree a -> HTree b
	(<*>) (HNode a b) (HNode c d) = HNode (a <*> c) (b <*> d)
	(<*>) (HLeaf a) (HLeaf c) = HLeaf (a c)
	(<*>) a (HNode c d) = HNode (a <*> c) (a <*> d)
	(<*>) (HNode a b) c = HNode (a <*> c) (b <*> c)

data Fun a b = Fun (a -> b) deriving Functor
data Compose f g a = Compose (f (g a)) deriving (Eq,Show,Functor)
newtype ZipList a = ZipList [a] deriving (Eq,Show,Functor)
data NonEmpty a = Last a | Cons a (NonEmpty a) deriving (Eq,Show,Functor)

instance Applicative (Fun x) where
	pure :: a -> Fun x a
	pure a = Fun (const a)

	liftA2 :: (a -> b -> c) -> (Fun x a -> Fun x b -> Fun x c)
	liftA2 f (Fun a) (Fun b) = Fun (\x -> f (a x) (b x))

	(<*>) :: Fun x (a -> b) -> Fun x a -> Fun x b
	(<*>) (Fun a) (Fun b) = Fun (\x -> (a x) (b x))

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
	pure :: a -> Compose f g a
	pure = Compose . pure . pure

	liftA2 :: (a -> b -> c) -> (Compose f g a -> Compose f g b -> Compose f g c)
	liftA2 f (Compose a) (Compose b) = Compose (liftA2 (liftA2 f) a b)

-- Bónusz
instance Applicative ZipList where
	-- Ezek adottak
	liftA2 :: (a -> b -> c) -> (ZipList a -> ZipList b -> ZipList c)
	liftA2 f (ZipList x) (ZipList y) = ZipList $ zipWith f x y

	(<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
	(<*>) (ZipList x) (ZipList y) = ZipList $ zipWith ($) x y

	-- pure?
	pure :: a -> ZipList a
	pure = ZipList . repeat

instance Applicative NonEmpty where
	-- Bónusz: Működjön az == operátor az eredményen!
	pure :: a -> NonEmpty a
	pure = Last

	liftA2 :: (a -> b -> c) -> (NonEmpty a -> NonEmpty b -> NonEmpty c)
	liftA2 f (Cons a b) (Cons c d) = Cons (f a c) (liftA2 f b d)
	liftA2 f (Last a) (Last b) = Last (f a b)
	liftA2 f (Cons a b) (Last c) = Cons (f a c) (liftA2 f b (Last c))
	liftA2 f (Last a) (Cons c d) = Cons (f a c) (liftA2 f (Last a) d)

	(<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
	(<*>) (Cons a b) (Cons c d) = Cons (a c) (b <*> d)
	(<*>) (Last a) (Last b) = Last (a b)
	(<*>) (Cons a b) (Last c) = Cons (a c) (b <*> (Last c))
	(<*>) (Last a) (Cons c d) = Cons (a c) ((Last a) <*> d)

instance Applicative (Either x) where
	pure :: a -> Either x a
	pure = Right

	liftA2 :: (a -> b -> c) -> (Either x a -> Either x b -> Either x c)
	liftA2 f (Right a) (Right b) = Right (f a b)
	liftA2 f (Left a) _ = Left a
	liftA2 f _ (Left a) = Left a

	(<*>) :: Either x (a -> b) -> Either x a -> Either x b
	(<*>) (Right a) (Right b) = Right (a b)
	(<*>) (Left a) _ = Left a
	(<*>) _ (Left a) = Left a
