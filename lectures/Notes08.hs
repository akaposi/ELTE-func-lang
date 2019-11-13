{-# language DeriveFunctor, DeriveFoldable #-}

import Control.Monad.State
import Control.Monad

-- Monád folytatás
--------------------------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Foldable, Show)


-- definiálj egy függvényt, ami kicsérli egy fa leveleiben tárolt értékeket
-- balról jobbra haladva egy megadott lista elemeire.
-- Használj State monádot!

-- pl: replace [10, 20, 30] (Node (Leaf 2) (Leaf 3)) == Node (Leaf 10) (Leaf 20)
--     replace [5] (Leaf 10) == Leaf 5
--     replace [5] (Node (Leaf 0) (Node (Leaf 0) (Leaf 0))) ==
--        (Node (Leaf 5) (Node (Leaf 0) (Leaf 0)))

replaceLeaves :: [a] -> Tree a -> Tree a
replaceLeaves = undefined where
  go :: Tree a -> State [a] (Tree a)
  go = undefined

-- ugyanezt jobbról balra is implementáld! Azaz jobbról balra haladva
-- illeszt a list elemeit a fába!
replaceLeaves' :: [a] -> Tree a -> Tree a
replaceLeaves' = undefined
