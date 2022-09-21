{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Ora1Gyakorlas where


-- Definiáljuk a következő függvényeket tetszőlegesen, de típushelyesen és totálisan
-- A megoldás ne vezessen végtelen rekurzióhoz vagy kivétel dobáshoz, illetve ne használjon unsafeCoerce és hasonló függvényeket
f14 :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
f14 = undefined

f15 :: Either a b -> (a -> c) -> (b -> c) -> c
f15 = undefined

f16 :: Either (a -> b) (a -> c) -> a -> Either b c
f16 = undefined

-- Az előző órárol visszahozott lista típus

data List a = Nil | Cons a (List a)
infixr 5 `Cons` -- kötési erősség definíció, nem kell tudni

instance Eq a => Eq (List a) where
    Nil         == Nil         = True
    (Cons a as) == (Cons b bs) = a == b && as == bs
    _           ==  _          = False

-- Definiáljuk a List a típusra egy Ord instance-ot amely az alábbi módon működik:
-- * Ha mindkét lista üres akkor egyenlőek
-- * Ha az egyik üres, de a másik nem, akkor a másik nagyobb
-- * Ha a listák fejeleme egyenlő, akkor a maradék lista alapján döntsük el, hogy melyik nagyobb
-- * Ha nem egyenlőek, akkor a nagyobb fejelemű lista a nagyobb
instance Ord a => Ord (List a) where
    (<=) = undefined


-- Definiáljunk egy olyan lista típust amely egy elem kettesével tárolja az elemeket. A konstruktorainak neve legyen PNill és PCons
data PairList a

-- Definiáljunk rá Eq instance-ot
instance Eq a => Eq (PairList a) where
    (==) = undefined


-- Definiáljunk egy egyirányú konverziót a két típus között. Fontos hogy a konverzió során ne veszítsünk elemet!
pairListToList :: PairList a -> List a
pairListToList = undefined

-- Definiáljunk egy fa adattípust. A fa minden elem vagy elemet tároló végpont (Leaf nevű konstruktor) vagy egy kétfelé irányuló elágazás
-- Az elágazás ne tartalmazzon a típusú elemet
data Tree a 

-- Definiáljunk rá Eq instance-ot
instance Eq a => Eq (Tree a) where
    (==) = undefined

-- Definiáljunk egy függvényt ami ennek a fának megadja az elemeit balról jobbra!
leftToRightTraversal :: Tree a -> [a]
leftToRightTraversal = undefined





--- | MEGOLDÁSOK | ---





f14' :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
f14' f g a = case f a of
    Just b -> g b
    _      -> Nothing

f14'' :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
f14'' f g a | Just b <- f a = g b
f14'' _ _ _                 = Nothing

-- Erre teljesen jó a (>=>) függvény a Control.Monad libraryből is.

f15' :: Either a b -> (a -> c) -> (b -> c) -> c
f15' (Left a)  f _ = f a
f15' (Right b) _ f = f b

f15'' :: Either a b -> (a -> c) -> (b -> c) -> c
f15'' et f g = either f g et

f16' :: Either (a -> b) (a -> c) -> a -> Either b c
f16' (Left f)  a = Left (f a)
f16' (Right g) a = Right (g a)

f16'' :: Either (a -> b) (a -> c) -> a -> Either b c
f16'' et a = either (Left . ($ a)) (Right . ($ a)) et
-- vagy either (\f -> Left $ f a) (\g -> Right $ g a) et

class Eq' a where
    eq :: a -> a -> Bool

infix 4 `eq`

class Eq' a => Ord' a where
    lte :: a -> a -> Bool

instance Eq' a => Eq' (List a) where
    Nil         `eq` Nil         = True
    (Cons a as) `eq` (Cons b bs) = a `eq` b && as `eq` bs
    _           `eq`  _          = False

instance Ord' a => Ord' (List a) where
    lte Nil         Nil         = True
    lte Nil         _           = True
    lte _           Nil         = False
    lte (Cons x xs) (Cons y ys) = lte x y && lte xs ys

data PairList' a = CNil' | CCons' a a (PairList' a)
-- vagy
data PairList'' a = CNil'' | CCons'' (a,a) (PairList'' a)

instance Eq a => Eq (PairList' a) where
    CNil'             == CNil'             = True
    (CCons' a1 a2 as) == (CCons' b1 b2 bs) = a1 == a2 && b1 == b2 && as == bs
    _                 == _                 = False

instance Eq a => Eq (PairList'' a) where
    CNil''         == CNil''         = True
    (CCons'' a as) == (CCons'' b bs) = a == b && as == bs
    _              == _              = False


pairListToList' :: PairList' a -> List a
pairListToList' CNil'             = Nil
pairListToList' (CCons' a1 a2 as) = a1 `Cons` a2 `Cons` pairListToList' as 

pairListToList'' :: PairList'' a -> List a
pairListToList'' CNil''                = Nil
pairListToList'' (CCons'' (a1, a2) as) = a1 `Cons` a2 `Cons` pairListToList'' as 

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) 

instance Eq a => Eq (Tree' a) where
    Leaf' a         == Leaf' b         = a == b
    (Node' tr1 tr2) == (Node' tr3 tr4) = tr1 == tr3 && tr2 == tr4
    _               == _               = False

leftToRightTraversal' :: Tree' a -> [a]
leftToRightTraversal' (Leaf' a) = [a]
leftToRightTraversal' (Node' left right) = leftToRightTraversal' left ++ leftToRightTraversal' right 