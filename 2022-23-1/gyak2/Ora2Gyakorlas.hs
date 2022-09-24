{-# OPTIONS_GHC -Wmissing-methods -Wincomplete-patterns #-}
module Ora2Gyakorlas where

data Pair a = Pair a a
data Triplet a = Triplet a a a
data OneOrTwo a = One a | Two a a
data SkipList a = Skip (SkipList a) | Cons a (SkipList a) | Nil

-- Eq instance szabályai:
-- * Ha a konstruktorok ugyanazok és a bennük lévő elemek egyenlőek akkor igaz
-- * Egyébként hamis
-- ==-t kell implementálni, nem eq-t!!!

-- Példa Eq instance:

data List a = Cons' a (List a) | Nil'

instance Eq a => Eq (List a) where
    Nil' == Nil' = True -- Megnézzük konstruktorok stimmelnek-e
    (Cons' x xs) == (Cons' y ys) = {- Megnézzük konstruktorok stimmelneke -} x == y && xs == ys {- Megnézzük belső elemek egyenlőek-e -}
    _ == _ = False -- Egyéb esetben hamis


-- Írjunk a fenti típusokra Eq instance-ot!

instance Eq a => Eq (Pair a) where

instance Eq a => Eq (Triplet a) where

instance Eq a => Eq (OneOrTwo a) where

instance Eq a => Eq (SkipList a) where


-- Fák

data Tree a = Leaf a | Branch (Tree a) a (Tree a) deriving Eq -- automatikus Eq instance, +/--on és vizsgán nem használható


-- Definiáljuk a preorderTraversal függvényt ami egy fa preorder bejárását adja meg!
preorderTraversal :: Tree a -> [a]
preorderTraversal = undefined

{- Tesztek:

preorderTraversal (Branch (Leaf 2) 3 (Leaf 4)) == [3,2,4]
preorderTraversal (Branch (Branch (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5)) == [4,2,1,3,5]

-}

-- Definiáljuk a nodeCount függvényt ami egy fa elemszámát megadja!
nodeCount :: Num b => Tree a -> b
nodeCount = undefined

{- Tesztek:

nodeCount (Branch (Leaf 2) 3 (Leaf 4)) == 3
nodeCount (Branch (Branch (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5)) == 5

-}

-- Foldable instance szabályai:
-- * A konstruktorokban nem lehet (a -> k) vagy (k -> a) típusú érték
-- * Más hajtogatható/rekurzív típusú paraméter esetén, foldr-t kell hívni
-- * Egyébként f-ek egymásba ágyazott hívása

-- Példák Foldable instance:
instance Foldable List where
    -- f :: a -> b -> b
    -- b :: b
    foldr f b Nil' = b
    foldr f b (Cons' a as) = f a {- f hívása -} $ foldr f b as {- Rekurzív típusú paraméter esetén foldr -}

-- Inorder foldable instance, ami órán volt a preorder foldable instance
instance Foldable Tree where
    -- f :: a -> b -> b
    -- b :: b
    foldr f b (Leaf a) = f a b -- f hívása
    foldr f b (Branch left a right) =  foldr f (f a {- f hívása -} $ foldr f b right) left {- Rekurzív típusú paraméter esetén foldr -}

-- Ami kívül van, az lesz az első
-- KÜLÖBÖZŐ BEJÁRÁSOK ESETÉN FOLDR DEFINÍCIÓK:
-- Inorder : foldr f b (Branch left mid right)   = foldr f (f mid $ foldr f b right) left -- Bal -> Középső -> Jobb
-- Preorder : foldr f b (Branch left mid right)  = f mid $ foldr f (foldr f b right) left -- Középső -> Bal -> Jobb
-- Postorder : foldr f b (Branch left mid right) = foldr f (foldr f (f mid b) right) left -- Bal -> Jobb -> Középső

-- Foldable-nél az INORDER szokás, avagy a konstruktor szerint balról jobbra járjuk be a paramétereket
-- Definiáljuk a fenti típusokra egy Foldable instance-ot! A bejárás konstruktor paraméterei szerint jobbról ballra legyen!

instance Foldable Pair where

instance Foldable Triplet where

instance Foldable OneOrTwo where

instance Foldable SkipList where


-- Definiáljuk a toList függvényt ami egy olyan típust melyre a foldr definiálva van, listává alakít!
toList :: Foldable f => f a -> [a]
toList = undefined


-----

-- MEGOLDÁSOK

-----



{-

instance Eq a => Eq (Pair a) where
    Pair a a' == Pair b b' = a == b && a' == b'

instance Eq a => Eq (Triplet a) where
    Triplet a a' a'' == Triplet b b' b'' = a == b && a' == b' && a'' == b''

instance Eq a => Eq (OneOrTwo a) where
    One a == One b = a == b
    Two a a' == Two b b' = a == b && a' == b'
    _ == _ = False

instance Eq a => Eq (SkipList a) where
    Nil == Nil = True
    Skip as == Skip bs = as == bs
    Cons a as == Cons b bs = a == b && as == bs

-}

preorderTraversal' :: Tree a -> [a]
preorderTraversal' (Leaf a) = [a]
preorderTraversal' (Branch left a right) = a : preorderTraversal' left ++ preorderTraversal' right

nodeCount' :: Num b => Tree a -> b
nodeCount' (Leaf _) = 1
nodeCount' (Branch left _ right) = nodeCount' left + 1 + nodeCount' right

{-
instance Foldable Pair where
    foldr f b (Pair a a') = f a (f a' b)

instance Foldable Triplet where
    foldr f b (Triplet a a' a'') = f a $ f a' $ f a'' b

instance Foldable OneOrTwo where
    foldr f b (One a) = f a b
    foldr f b (Two a a') = f a $ f a' b

instance Foldable SkipList where
    foldr f b Nil = b
    foldr f b (Skip as) = foldr f b as
    foldr f b (Cons a as) = f a $ foldr f b as
-}

toList' :: Foldable f => f a -> [a]
toList' = foldr (:) []