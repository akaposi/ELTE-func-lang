{-# options_ghc -Wincomplete-patterns #-}
{-# language InstanceSigs #-}

module Lesson04 where

--

data Tree a
  = Node (Tree a) a (Tree a)
  | Leaf
  deriving (Show, Eq)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f acc Leaf = acc
  foldr f acc (Node l v r) = foldr f (f v (foldr f acc r)) l
    -- let
    --   rightResult = foldr f acc r -- right folded
    --   combinedResult = f v rightResult -- right + value combined
    --   leftResult = foldr f combinedResult l -- left folded with combined result included
    -- in
    --   leftResult

tests1 :: [Bool]
tests1 =
  [ foldr (+) 0 (Node (Node Leaf 4 Leaf) 5 (Node (Node Leaf 2 Leaf) 7 Leaf)) == 18
  , foldr (*) 1 (Node (Node Leaf 4 Leaf) 5 (Node (Node Leaf 2 Leaf) 7 Leaf)) == 280
  , foldr (++) "" (Node (Node Leaf "lorem" (Node Leaf "ipsum" Leaf)) "dolor" (Node (Node Leaf "sit" Leaf) "amet" Leaf)) == "loremipsumdolorsitamet"
  ]

--

data Error = Error deriving (Show, Eq)

composeEither ::
  (b -> Either Error c) ->
  (a -> Either Error b) ->
  a -> Either Error c
composeEither f g a = case g a of
                        Left e -> Left e
                        Right v -> f v
  -- let
  --   innerResult = g a
  --   outerResult = case innerResult of -- call f on innerResult
  --                   Left e -> Left e
  --                   Right v -> f v
  -- in
  --   outerResult

tests2 :: [Bool]
tests2 =
  [ composeEither (Right . (*5)) (Right . (+3)) 5 == Right 40
  , composeEither (Right . (+2)) (Right . (*4)) 6 == Right 26
  , composeEither (Right . (+2)) (const (Left Error)) 7 == Left Error
  , composeEither (\b -> if b then Right "ok" else Left Error) (Right . not) True == Left Error
  , composeEither (\b -> if b then Right "ok" else Left Error) (Right . not) False == Right "ok"
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
applyResult (Err e) _ = Err e

bindResult :: Result a -> (a -> Result b) -> Result b
bindResult (Ok v) f = f v
bindResult (Err e) f = Err e

instance Applicative Result where
  pure :: a -> Result a
  pure = Ok

  (<*>) :: Result (a -> b) -> Result a -> Result b
  (<*>) = applyResult

instance Monad Result where
  -- return = Ok

  (>>=) :: Result a -> (a -> Result b) -> Result b
  (>>=) = bindResult

-- makePairs [1,2,3,4,5,6] = [(1,2), (3,4), (5,6)]

makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs [_] = undefined
makePairs (a1:a2:as) = (a1,a2) : (makePairs as) -- rekurzió

safeMakePairs :: [a] -> Result [(a, a)]
safeMakePairs [] = Ok []
safeMakePairs [_] = Err "odd number of elements"
safeMakePairs (a1:a2:as) = case (safeMakePairs as) of
                             Ok v -> Ok ((a1, a2) : v)
                             Err e -> Err e

test :: Result [Int]
-- test = safeMakePairs [1,2,3,5,4] >>= (\x -> return (fmap snd x))
test = do
  ps <- safeMakePairs [1,2,3,4,5,6,7]
  b <- return (fmap snd ps)
  return (fmap (*2) b)

-- Example from BE-AD:

composeEither' ::
  (b -> (Either Error) c) ->
  (a -> (Either Error) b) ->
  (a -> (Either Error) c)
composeEither' f g a = (g a) >>= f
-- composeMaybe' f g a = (return a) >>= g >>= f
-- composeMaybe' f g a = do
--   ga <- g a
--   f ga

-- Tasks:

smallEven :: Int -> Maybe Bool
smallEven x = if x < 10 then Just (even x) else Nothing

-- 1. Írd meg a következő függvényt. A függvény úgy működik,
--    mint a lista "filter", viszont ha a kapott (a -> Maybe Bool)
--    függvény valamely elemre Nothing-ot ad, akkor Nothing legyen
--    a végeredmény, egyébként Just <szűrt lista>
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f [] = Just []
filterMaybe f (x:xs) = do
  a <- f x
  rest <- filterMaybe f xs
  return (if a then (x:rest) else rest)

-- filterMaybe f (x:xs) = case ((f x), filterMaybe f xs) of
--                          (_, Nothing) -> Nothing
--                          (Nothing, _) -> Nothing
--                          (Just True, Just fxs) -> Just (x : fxs)
--                          (Just False, Just fxs) -> Just fxs

-- 2. Alkalmazz egy (a -> Maybe b) függvény egy Tree minden
-- csúcsára, ha bármelyik alkalmazás Nothing-ot ad,
-- legyen az eredmény Nothing!
mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree = undefined

-- 3. Alkalmazzuk páronként a kapott (a -> b -> Maybe c) függvényt a bemenő listák
-- elemeire! Ha bármelyik függvényalkalmazás Nothing, akkor a kimenet legyen
-- Nothing, egyébként Just <lista zippelés eredménye>.
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe = undefined
