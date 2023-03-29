{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}
module Foldable_maybem where

-- Foldable feladatok

data List a = Nil | Cons a (List a)
    deriving (Show)

instance Foldable List where
    foldr :: (a -> b -> b) -> b -> List a -> b
    foldr = undefined


data FullTree a = Leaf a | Node a (FullTree a) (FullTree a)  -- Olyan fa, ahol a csúcsokba is vannak értékek!
    deriving (Show)

instance Foldable FullTree where
    foldr :: (a -> b -> b) -> b -> FullTree a -> b
    foldr = undefined


data PairRecurse a b = Pair a b (PairRecurse a b)
    deriving (Show)

instance Foldable (PairRecurse a) where        -- Ez az eset az, ahol muszáj az a-t lekötni!
    foldr :: (b -> c -> c) -> c -> (PairRecurse a) b  -> c    -- Figyeljük meg, hogy hogyan változtatta meg a foldr típusát!
    foldr = undefined



-- Maybe Monad feladatok

-- Mindenféleképpen próbáld meg a Monad Maybe instance-el (do notáció, vagy anélkül), ha nem megy, oldd meg
-- case-ekkel, és próbáld meg átírni rá!


-- Definiáld a sumWithMaybe függvényt, amely megpróbálja összeadni az elemeit egy Int listának.
-- Ha minden elemre Just-ot ad vissza a vizsgáló függvény, akkor az eredmény legyen Just \összeg\,
-- viszont ha bármely elemre Nothingot ad, a végeredmény legyen Nothing!
sumWithMaybe :: (Int -> Maybe Int) -> [Int] -> Maybe Int
sumWithMaybe = undefined


-- Definiáld a productWithMaybe függvényt, amely megpróbálja összeszorozni az elemeit egy Int listának.
-- Ha bármely elemre a vizsgáló függvény Nothing-ot ad, akkor legyen a végeredmény is Nothing!
-- Ha a vizsgáló függvény Just False-al tér vissza, akkor az adott elemmel szorozzunk.
-- Ha a vizsgálü függvény Just True-al tér vissza, akkor az adott elem kétszeresével szorozzunk!.
productWithMaybe :: (Int -> Maybe Bool) -> [Int] -> Maybe Int
productWithMaybe = undefined


-- Definiáld az isAscendingWithMaybe függvényt, amely el szeretné dönteni, hogy az adott lista szig.mon.növekvő-e,
-- és minden elemet megvizsgál.
-- Ha a vizsgálat bármely elemre Nothingot ad, akkor legyen a végeredmény is Nothing!
-- Ha minden elemre Just ()-ot ad, akkor legyen a végeredmény egy Maybe Bool, ahol a Bool
-- False, ha nem szig.mon.növekvő, viszont True, ha az!  -- Nehéz!!!

----- () -- Unit, olyan adattípus, aminek 1 lehetséges értéke van (előadáson szerepelt). Ez az egyetlen
-- lehetséges értéke (). Tehát ahogy a Bool lehet False és True, a () csak () lehet :)
isAscendingWithMaybe :: (Int -> Maybe ()) -> [Int] -> Maybe Bool 
isAscendingWithMaybe = undefined

isAscendingTest :: Int -> Maybe ()  -- Páros számokra "hibát" dob.
isAscendingTest x
    | even x = Nothing
    | otherwise = Just ()

-- Írj egy replicateWithMaybe függvényt, ami megpróbálja elvégezni a replicate függvényt
-- egy adott Int-szer egy bemenő "a" listán!
-- Minden elemét az a listának megvizsgáljuk, és az alábbi módon járunk el:
-- Ha a vizsgálat eredménye bármely elemre Nothing, legyen a végeredmény is Nothing!
-- Ha a vizsgálat eredménye Just False, akkor replikáljuk az adott elemet a bemenő Int paraméterszer.
-- Ha a vizsgálat eredménye Just True, akkor pedig dupla annyiszor replikáljuk! (Int * 2)
-- A végeredmény pedig legyen egy [[a]], melynek részlistái a replikált listaelemek.
-- Használd a beépített replicate függvényt!
replicateWithMaybe :: (a -> Maybe Bool) -> Int -> [a] -> Maybe [[a]]
replicateWithMaybe = undefined



-- Megoldások

{-
instance Foldable List where
    foldr :: (a -> b -> b) -> b -> List a -> b
    foldr f b Nil = b
    foldr f b (Cons a as) = f a (foldr f b as) 


instance Foldable FullTree where
    foldr :: (a -> b -> b) -> b -> FullTree a -> b
    foldr f b (Leaf a) = f a b
    foldr f b (Node a tr1 tr2) = let
        r' = foldr f b tr2
        l' = foldr f r' tr1
        all = f a l'
        in all
    -- let nélkül
    --foldr f b (Node a tr1 tr2) = f a (foldr f (foldr f b tr2) tr1)


instance Foldable (PairRecurse a) where        -- Ez az eset az, ahol muszáj az a-t lekötni!
    foldr :: (b -> c -> c) -> c -> (PairRecurse a) b  -> c    -- Figyeljük meg, hogy hogyan változtatta meg a foldr típusát!
    foldr f c (Pair a b ps) = f b (foldr f c ps) -- Az a-t szabadon hagyjuk, vele nem kezdünk semmit. 
-}




-- Definiáld a sumWithMaybe függvényt, amely megpróbálja összeadni az elemeit egy Int listának.
-- Ha minden elemre Just-ot ad vissza a vizsgáló függvény, akkor az eredmény legyen Just \összeg\,
-- viszont ha bármely elemre Nothingot ad, a végeredmény legyen Nothing!

-- sumWithMaybe :: (Int -> Maybe Int) -> [Int] -> Maybe Int
-- sumWithMaybe f [] = Just 0   -- Azért Just 0, mivel az összeadás egységeleme a 0.
-- sumWithMaybe f (x:xs) = do         -- Do notációval
--     x' <- f x
--     xs' <- sumWithMaybe f xs
--     return (x' + xs')

-- sumWithMaybe f (x:xs) =            -- Monád, de do notáció nélkül
--     f x >>= (\x' ->
--     sumWithMaybe f xs >>= (\xs' ->
--     return (x' + xs')))

-- sumWithMaybe f (x:xs) = case f x of   -- Casekkel, azaz Monad Maybe instance nélkül.
--     Nothing -> Nothing
--     Just x' -> case sumWithMaybe f xs of
--         Nothing -> Nothing
--         Just xs' -> Just (x' + xs')


-- Definiáld a productWithMaybe függvényt, amely megpróbálja összeszorozni az elemeit egy Int listának.
-- Ha bármely elemre a vizsgáló függvény Nothing-ot ad, akkor legyen a végeredmény is Nothing!
-- Ha a vizsgáló függvény Just False-al tér vissza, akkor az adott elemmel szorozzunk.
-- Ha a vizsgálü függvény Just True-al tér vissza, akkor az adott elem kétszeresével szorozzunk!.

-- productWithMaybe :: (Int -> Maybe Bool) -> [Int] -> Maybe Int
-- productWithMaybe f [] = Just 1  -- Azért Just 1, mert a szorzás egységeleme az 1.
-- productWithMaybe f (x:xs) = do         -- Do notációval
--     x' <- f x
--     xs' <- productWithMaybe f xs
--     return (if x' then 2 * x * xs' else x * xs')

-- productWithMaybe f (x:xs) =            -- Monád, de do notáció nélkül
--     f x >>= (\x' ->
--     productWithMaybe f xs >>= (\xs' ->
--     return (if x' then 2 * x * xs' else x * xs')))

-- productWithMaybe f (x:xs) = case f x of   -- Casekkel, azaz Monad Maybe instance nélkül.
--     Nothing -> Nothing
--     Just x' -> case productWithMaybe f xs of
--         Nothing -> Nothing
--         Just xs' -> Just (if x' then 2 * x * xs' else x * xs')


-- Definiáld az isAscendingWithMaybe függvényt, amely el szeretné dönteni, hogy az adott lista szig.mon.növekvő-e,
-- és minden elemet megvizsgál.
-- Ha a vizsgálat bármely elemre Nothingot ad, akkor legyen a végeredmény is Nothing!
-- Ha minden elemre Just ()-ot ad, akkor legyen a végeredmény egy Maybe Bool, ahol a Bool
-- False, ha nem szig.mon.növekvő, viszont True, ha az!

----- () -- Unit, olyan adattípus, aminek 1 lehetséges értéke van (előadáson szerepelt). Az egyetlen
-- lehetséges értéke (). Tehát ahogy a Bool lehet False és True, a () csak () lehet :)

-- isAscendingWithMaybe :: (Int -> Maybe ()) -> [Int] -> Maybe Bool
-- isAscendingWithMaybe f [] = Just True
-- isAscendingWithMaybe f [x] = do                    -- Do notációval
--     f x
--     return True
-- isAscendingWithMaybe f (x:(y:ys)) = do
--     f x                                            -- Itt nem tárolom el az értéket, mivel nem fontos számomra,
--     f y                                            -- csak a mellékhatás a fontos, tehát az, hogy ha Nothing van,
--     otherBool <- isAscendingWithMaybe f (y:ys)     -- eldobja
--     return (if otherBool then x < y else False)

-- isAscendingWithMaybe f [x] =                       -- Monad, de do notáció nélkül
--     f x >>
--     return True
-- isAscendingWithMaybe f (x:(y:ys)) =
--     f x >>
--     f y >>
--     isAscendingWithMaybe f (y:ys) >>= (\otherBool ->
--     return (if otherBool then x < y else False))

-- isAscendingWithMaybe f [x] = case f x of
--     Nothing -> Nothing
--     Just () -> Just True
-- isAscendingWithMaybe f (x:(y:ys)) = case f x of
--   Nothing -> Nothing
--   Just () -> case f y of
--     Nothing -> Nothing
--     Just () -> case isAscendingWithMaybe f (y:ys) of
--         Nothing -> Nothing
--         Just otherBool -> Just (if otherBool then x < y else False) 



-- Írj egy replicateWithMaybe függvényt, ami megpróbálja elvégezni a replicate függvényt
-- egy adott Int-szer egy bemenő "a" listán!
-- Minden elemét az a listának megvizsgáljuk, és az alábbi módon járunk el:
-- Ha a vizsgálat eredménye bármely elemre Nothing, legyen a végeredmény is Nothing!
-- Ha a vizsgálat eredménye Just False, akkor replikáljuk az adott elemet a bemenő Int paraméterszer.
-- Ha a vizsgálat eredménye Just True, akkor pedig dupla annyiszor replikáljuk! (Int * 2)
-- A végeredmény pedig legyen egy [[a]], melynek részlistái a replikált listaelemek.
-- Használd a beépített replicate függvényt!

-- replicateWithMaybe :: (a -> Maybe Bool) -> Int -> [a] -> Maybe [[a]]
-- replicateWithMaybe f x [] = Just [[]]
-- replicateWithMaybe f x (a:as) = do                       -- Do notációval
--     a' <- f a
--     as' <- replicateWithMaybe f x as
--     return (if a' then replicate (x*2) a : as' else replicate x a : as')

-- replicateWithMaybe f x (a:as) =                          -- Do notáció nélkül, de Monad
--     f a >>= (\a' ->
--     replicateWithMaybe f x as >>= (\as' ->
--     return (if a' then replicate (x*2) a : as' else replicate x a : as')))

-- replicateWithMaybe f x (a:as) = case f a of              -- Case-el
--     Nothing -> Nothing
--     Just a' -> case replicateWithMaybe f x as of
--         Nothing -> Nothing
--         Just as' -> Just (if a' then replicate (x*2) a : as' else replicate x a : as')