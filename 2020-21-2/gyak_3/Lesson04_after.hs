{-# options_ghc -Wincomplete-patterns #-}
{-# language InstanceSigs #-}

module Lesson04 where

--

data SparseList a
  = Cons a (SparseList a)
  | Skip (SparseList a)
  | Nil deriving (Eq, Show)

instance Foldable SparseList where
  foldr :: (a -> b -> b) -> b -> SparseList a -> b
  foldr f acc (Cons a as) = f a (foldr f acc as)
    -- let
    --   restResult = (foldr f acc as)
    -- in
    --   f a restResult
  foldr f acc (Skip as) = foldr f acc as
  foldr f acc Nil = acc

tests1 :: [Bool]
tests1 =
  [ foldr (&&) True (Cons True (Skip (Cons False Nil))) == False
  , foldr (+) 0 (Cons 1 (Cons 4 (Skip (Cons 2 (Cons 8 (Skip Nil)))))) == 15
  , foldr (*) 1 (Skip (Cons 1 (Cons 4 (Cons 2 (Cons 8 (Skip Nil)))))) == 64
  , foldr (++) "" (Cons "lorem" (Skip (Skip (Cons "ipsum" (Cons "dolor" (Skip (Cons "sit" (Cons "amet" Nil)))))))) == "loremipsumdolorsitamet"
  ]

--

composeMaybe :: (b -> Maybe c) -> (a -> Maybe b) -> a -> Maybe c
composeMaybe f g a =
  let
    firstResult = g a
    secondResult = case firstResult of -- call f depending on firstResult
                     (Just b) -> f b
                     Nothing -> Nothing
  in
    secondResult

tests2 :: [Bool]
tests2 =
  [ composeMaybe (Just . (*5)) (Just . (+3)) 5 == Just 40
  , composeMaybe (Just . (+2)) (Just . (*4)) 6 == Just 26
  , composeMaybe (Just . (+2)) (const Nothing) 7 == Nothing
  , composeMaybe (\b -> if b then Just "ok" else Nothing) (Just . not) True == Nothing
  , composeMaybe (\b -> if b then Just "ok" else Nothing) (Just . not) False == Just "ok"
  ]

{- Applicative, Monad -}

data Result a = Ok a | Err String deriving (Eq, Show)

-- mapResult :: (a -> b) -> Result a -> Result b
-- mapResult = undefined

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap f (Ok v) = Ok (f v)
  fmap f (Err e) = Err e

applyResult :: Result (a -> b) -> Result a -> Result b
applyResult (Ok f) ra = fmap f ra
applyResult (Err e) ra = Err e

bindResult :: Result a -> (a -> Result b) -> Result b
bindResult (Ok v) f = f v
bindResult (Err e) f = Err e

instance Applicative Result where
  pure :: a -> Result a
  pure = Ok
  (<*>) = applyResult

instance Monad Result where
  -- return = Ok
  (>>=) = bindResult

-- makePairs [1,2,3,4,5,6] == [(1,2),(3,4),(5,6)]
makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs [_] = undefined
makePairs (a1:a2:as) = (a1,a2) : (makePairs as)

safeMakePairs :: [a] -> Result [(a, a)]
safeMakePairs [] = Ok []
safeMakePairs [_] = Err "odd number of elements"
safeMakePairs (a1:a2:as) = case (safeMakePairs as) of
                             Ok v -> Ok ((a1,a2) : v)
                             Err e -> Err e

-- test = safeMakePairs [1,2,3,4,5] >>= (return . (map snd))

test2 = do
  ps <- safeMakePairs [1,2,3,4,5]
  ss <- return (map fst ps)
  return (map (*2) ss)

-- Example from BE-AD:

composeMaybe' :: (b -> Maybe c) -> (a -> Maybe b) -> (a -> Maybe c)
composeMaybe' f g a = (g a) >>= f
-- composeMaybe' f g a = (return a) >>= g >>= f
-- composeMaybe' f g a = do
--   ga <- g a
--   f ga

-- Tasks:

smallEven :: Int -> Maybe Bool
smallEven x = if (x < 10) then Just (even x) else Nothing

-- 1. Írd meg a következő függvényt. A függvény úgy működik,
--    mint a lista "filter", viszont ha a kapott (a -> Maybe Bool)
--    függvény valamely elemre Nothing-ot ad, akkor Nothing legyen
--    a végeredmény, egyébként Just <szűrt lista>
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f [] = Just []
filterMaybe f (x:xs) = do
  b <- f x
  fxs <- filterMaybe f xs
  return (if b then x : fxs else fxs)

-- filterMaybe f (x:xs) = case (f x, filterMaybe f xs) of
--                          (Nothing, _) -> Nothing
--                          (_, Nothing) -> Nothing
--                          (Just True, Just fxs) -> Just (x : fxs)
--                          (Just False, Just fxs) -> Just fxs
--                          (_, _) -> Nothing

-- 2. Alkalmazz egy (a -> Maybe b) függvényt egy SparseList minden
-- elemére, ha bármelyik alkalmazás Nothing-ot ad,
-- legyen az eredmény Nothing!
mapMaybeSparseList :: (a -> Maybe b) -> SparseList a -> Maybe (SparseList b)
mapMaybeSparseList = undefined

-- 3. Alkalmazzuk páronként a kapott (a -> b -> Maybe c) függvényt a bemenő listák
-- elemeire! Ha bármelyik függvényalkalmazás Nothing, akkor a kimenet legyen
-- Nothing, egyébként Just <lista zippelés eredménye>.
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe = undefined
