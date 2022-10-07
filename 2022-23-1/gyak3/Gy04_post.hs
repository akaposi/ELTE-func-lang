{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}
module Gy04 where

data SparseList a = Nil | Skip (SparseList a) | Cons a (SparseList a)
  deriving (Show)

exampleList :: SparseList Char
exampleList = Cons 's' (Cons 'a' (Skip (Cons 'j' (Skip (Cons 't' Nil)))))

exampleList' :: SparseList Int
exampleList' = Cons 1 (Skip (Cons 4 Nil))

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

exampleTree :: Tree String
exampleTree = Node
                (Node (Leaf "the") (Leaf "cake"))
                (Node (Leaf "is") (Node (Leaf "as") (Leaf "lie")))

{-
             ∙
            / \
           /   \
          /     \
         ∙       \
        / \       \
       /   \       \
   "the"   "cake"   ∙
                   / \
                  /   \
               "is"    ∙
                      / \
                     /   \
                   "a"   "lie"
-}

exampleTree' :: Tree Int
exampleTree' = Node (Node (Leaf 2) (Leaf 3)) (Leaf 5)

{-
        ∙
       / \
      ∙   5
     / \
    2   3
-}


-- Foldable
--------------------------------------------------------------------------------

-- class Foldable f where
--   foldr :: (a -> b -> b) -> b -> f a -> b
--   foldl
--   foldMap
--   ...

instance Foldable SparseList where
  foldr :: (a -> b -> b) -> b -> SparseList a -> b
  foldr f z Nil = z
  foldr f z (Skip ls) = foldr f z ls
  foldr f z (Cons a ls) = f a (foldr f z ls)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z (Leaf a) = f a z
  foldr f z (Node tr1 tr2) = let 
    r' = foldr f z tr2
    l' = foldr f r' tr1
    in l'
    


-- Maybe monád motiváló feladatok
--------------------------------------------------------------------------------

-- Írd meg a következő függvényt. A függvény úgy működik,
-- mint a lista "filter", viszont ha a kapott (a -> Maybe Bool)
-- függvény valamely elemre Nothing-ot ad, akkor Nothing legyen
-- a végeredmény, egyébként Just <szűrt lista>
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f [] = Just []
filterMaybe f (a:as) = case f a of
  Nothing -> Nothing
  Just a' -> case filterMaybe f as of
    Nothing -> Nothing
    Just ls -> if a'
      then Just (a:ls)
      else Just ls

lessThen3Even :: Int -> Maybe Bool
lessThen3Even x 
  | x < 3 = Nothing
  | even 2 = Just True
  | otherwise = Just False



-- Alkalmazz egy (a -> Maybe b) függvény egy Tree minden
-- levelére, ha bármelyik alkalmazás Nothing-ot ad,
-- legyen az eredmény Nothing!
mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree f (Leaf a) = case f a of
  Nothing -> Nothing
  Just b -> Just (Leaf b)
mapMaybeTree f (Node tr1 tr2) = case mapMaybeTree f tr1 of
  Nothing -> Nothing
  Just tr1' -> case mapMaybeTree f tr2 of
    Nothing -> Nothing
    Just tr2' -> Just (Node tr1' tr2')

testMapMaybe :: String -> Maybe String
testMapMaybe s = if length s < 2 
  then Nothing
  else Just (s ++ "!")

-- Definiáld újra az előbbi három feladatot Maybe monád instance használatával!

--(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--return :: a -> Maybe a
filterMaybe' :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe' f [] = Just []
-- filterMaybe' f (a:as) = 
--   f a >>= (\a' ->
--   filterMaybe' f as >>= (\ls ->
--   return (if a' then a:ls else ls)))
filterMaybe' f (a:as) = do
  a' <- f a
  ls <- filterMaybe' f as
  return (if a' then a:ls else ls)


mapMaybeTree' :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree' f (Leaf a) = do
  a' <- f a
  return (Leaf a')
mapMaybeTree' f (Node tr1 tr2) = do
  tr1' <- mapMaybeTree' f tr1
  tr2' <- mapMaybeTree' f tr2
  return (Node tr1' tr2')
  


-- Alkalmazzuk páronként a kapott (a -> b -> Maybe c) függvényt a bemenő listák
-- elemeire! Ha bármelyik függvényalkalmazás Nothing, akkor a kimenet legyen
-- Nothing, egyébként Just <lista zippelés eredménye>.

zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe f [] [] = Just []
zipWithMaybe f (a:as) (b:bs) = case f a b of
  Nothing -> Nothing
  Just c -> case zipWithMaybe f as bs of
    Nothing -> Nothing
    Just cs -> Just (c:cs)
-- zipWithMaybe f (a:as) [] = Just []
-- zipWithMaybe f [] (b:bs) = Just []
zipWithMaybe f _ _ = Just [] -- Ez a pattern jelen esetben ugyan az, mint a kettő sor felette, mivel a 
--                              beépített zip függvény csak addig megy, amíg valamelyik lista nem üres,
--                              a többit eldobja

-- Próbálj definiálni egy (a -> b -> Maybe c függvényt, amivel letesztelheted a zipWithMaybe-t!)



-- Do notáció nélkül
zipWithMaybe' :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe' f [] [] = Just []
zipWithMaybe' f (a:as) (b:bs) = 
  f a b >>= (\c ->                                    -- case f a b of, beépített "hibakezeléssel"
  zipWithMaybe' f as bs >>= (\cs ->                   -- case zipWithMaybe' f as bs of
  return (c:cs)))                                     -- Maybe monád miatt return = Just, visszacsomagoljuk
zipWithMaybe' f _ _ = Just []

-- Do notációval
zipWithMaybe'' :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe'' f [] [] = Just []
zipWithMaybe'' f (a:as) (b:bs) = do
  c <- f a b                                          -- f a b >>= (\c -> ...)
  cs <- zipWithMaybe' f as bs                         -- zipWithMaybe' f as bs >>= (\c -> ...)
  return (c:cs)                                       -- Változatban. Ne feledjétek, <- is "bind", és >>= is "bind"
zipWithMaybe'' f _ _ = Just []

-- Teszteld le az előző két függvényt a már elkészített tesztfüggvénnyel! 


-- IO monád
--------------------------------------------------------------------------------

-- getLine  :: IO String             -- beolvas
-- print    :: Show a => a -> IO ()  -- kinyomtat értéket
-- putStrLn :: String -> IO ()       -- String-et nyomtat ki

-- (>>=)  :: IO a -> (a -> IO b) -> IO b
-- return :: a -> IO a
-- fmap   :: (a -> b) -> IO a -> IO b


-- Írj egy függvényt, ami beolvas egy sort, majd visszaadja a sorban található
-- 'a' betűk számát!
io1 :: IO ()
io1 = undefined


-- Írj egy függvényt, ami beolvas egy sort, majd a sort kinyomtatja annyiszor,
-- ahány karakter van a sorban!
io2 :: IO ()
io2 = undefined


-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert. Ha a sorban 'x' van, akkor a program nyomtassa ki az
-- összes eddig beolvasott sort és térjen vissza.
io3 :: IO ()
io3 = undefined


-- A következőt ismételd végtelenül: olvass be egy sort, majd nyomtasd ki a
-- sorban a kisbetűk számát.  A Ctrl-c-c -vel lehet megszakítani a futtatást
-- ghci-ben.
io4 :: IO ()
io4 = undefined


-- Bónusz funktor feladatok múlt hétről
--------------------------------------------------------------------------------

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = undefined

apply :: Functor f => f (a -> b) -> a -> f b
apply = undefined

first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first = undefined

second :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
second = undefined

data Sum f g a = Inl (f a) | Inr (g a) deriving Show
data Product f g a = Product (f a) (g a) deriving Show
newtype Compose f g a = Compose (f (g a)) deriving Show

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap = undefined

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap = undefined

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap = undefined


-- bónusz bónusz: mire használható ez a függvény? Tipp: a megoldáshoz
-- rekurzió szükséges.
löb :: Functor f => f (f a -> a) -> f a
löb = undefined

-- bónusz bónusz 2:
newtype Fix f = Fix (f (Fix f))

fold :: Functor f => (f a -> a) -> Fix f -> a
fold = undefined
