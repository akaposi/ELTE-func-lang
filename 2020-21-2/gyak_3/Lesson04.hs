{-# options_ghc -Wincomplete-patterns #-}
{-# language InstanceSigs #-}

module Lesson04 where

--

data SparseList a
  = Cons a (SparseList a)
  | Skip (SparseList a)
  | Nil

instance Foldable SparseList where
  foldr :: (a -> b -> b) -> b -> SparseList a -> b
  foldr = undefined

tests1 :: [Bool]
tests1 =
  [ foldr (&&) True (Cons True (Skip (Cons False Nil))) == False
  , foldr (+) 0 (Cons 1 (Cons 4 (Skip (Cons 2 (Cons 8 (Skip Nil)))))) == 15
  , foldr (*) 1 (Skip (Cons 1 (Cons 4 (Cons 2 (Cons 8 (Skip Nil)))))) == 64
  , foldr (++) "" (Cons "lorem" (Skip (Skip (Cons "ipsum" (Cons "dolor" (Skip (Cons "sit" (Cons "amet" Nil)))))))) == "loremipsumdolorsitamet"
  ]

--

composeMaybe :: (b -> Maybe c) -> (a -> Maybe b) -> (a -> Maybe c)
composeMaybe = undefined

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

mapResult :: (a -> b) -> Result a -> Result b
mapResult = undefined

instance Functor Result where
  fmap = undefined

applyResult :: Result (a -> b) -> Result a -> Result b
applyResult = undefined

bindResult :: Result a -> (a -> Result b) -> Result b
bindResult = undefined

instance Applicative Result where
  pure = undefined
  (<*>) = undefined

instance Monad Result where
  return = undefined
  (>>=) = undefined

makePairs :: [a] -> [(a, a)]
makePairs = undefined

safeMakePairs :: [a] -> Result [(a, a)]
safeMakePairs = undefined

-- Example from BE-AD:

composeMaybe' :: (b -> Maybe c) -> (a -> Maybe b) -> (a -> Maybe c)
composeMaybe' = undefined

-- Tasks:

-- 1. Írd meg a következő függvényt. A függvény úgy működik,
--    mint a lista "filter", viszont ha a kapott (a -> Maybe Bool)
--    függvény valamely elemre Nothing-ot ad, akkor Nothing legyen
--    a végeredmény, egyébként Just <szűrt lista>
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe = undefined

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
