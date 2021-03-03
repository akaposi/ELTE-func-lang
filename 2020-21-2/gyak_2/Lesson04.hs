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
  foldr = undefined

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
  (a -> Either Error c)
composeEither = undefined

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

composeEither' ::
  (b -> Either Error c) ->
  (a -> Either Error b) ->
  (a -> Either Error c)
composeEither' = undefined

-- Tasks:

-- 1. Írd meg a következő függvényt. A függvény úgy működik,
--    mint a lista "filter", viszont ha a kapott (a -> Maybe Bool)
--    függvény valamely elemre Nothing-ot ad, akkor Nothing legyen
--    a végeredmény, egyébként Just <szűrt lista>
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe = undefined

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
