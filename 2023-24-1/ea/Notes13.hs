data BiList a = ConsSnoc a (BiList a) a | Nil
  deriving (Show)

instance Foldable BiList where
  foldMap f Nil = mempty
  foldMap f (ConsSnoc x xs x') =
    f x `mappend` (foldMap f xs) `mappend` f x'

b1 :: BiList Int
b1 = ConsSnoc 0 (ConsSnoc 1 Nil 2) 3

sumB1 :: Int
sumB1 = foldr (+) 0 b1

-- (xs ++ ys) ++ zs  LASSU   |xs| + (|xs|+|ys|)
-- xs ++ (ys ++ zs)  GYORS   |xs| + |ys|

type DiffList a = [a] -> [a]

-- dxs :: DiffList a , 

fromList :: [a] -> DiffList a
fromList xs = \ys -> ys ++ xs

toList :: DiffList a -> [a]
toList ds = ds []

(+++) :: DiffList a -> DiffList a -> DiffList a
dxs +++ dys = \ zs -> dys (dxs zs)
-- dxs zs = zs +++ xs
-- dys (dxs zs) = (zs +++ xs) +++ ys

-- (xs ++ ys) ++ zs = xs ++ (ys ++ zs)  NEM, ha xs,ys,zs valtozok
-- ([1,2,3] ++ [4,5,6]) ++ [7,8] = [1,2,3] ++ ([4,5,6] ++ [7,8])   IGEN

-- (xs +++ ys) +++ zs = xs +++ (ys +++ zs)  IGEN
-- (dxs +++ dys) +++ dzs = \ws -> dzs (dys (dxs ws))
-- xs +++ (ys +++ zs) = \ws -> (ys +++ zs) (xs ws) =
--   \ws -> zs (ys (xs ws))

-- ShowS Haskell standard library-ben

-- toList ((fromList xs +++ fromList ys) +++ fromList zs) =
-- fromList zs (fromList ys (fromList xs [])) =
-- (([] ++ xs) ++ ys) ++ zs = LASSU

pl :: [Int]
pl = toList ((fromList xs +++ fromList ys) +++ fromList zs)
  where
    xs = [1,2,3,5]
    ys = [3,4,9]
    zs = [1,2,3,4,5,6]

lassuLista :: Int -> [Int] -> [Int]
lassuLista n xs | n == 0    = xs
                | otherwise = lassuLista (n-1) xs ++ xs

gyorsLista :: Int -> [Int] -> [Int]
gyorsLista n xs | n == 0    = xs
                | otherwise = xs ++ gyorsLista (n-1) xs

-- teszteles:
-- length $ gyorsLista 10000 [1]
-- length $ lassuLista 10000 [1]

gyorsDiffLista :: Int -> DiffList Int -> DiffList Int
gyorsDiffLista n xs
  | n == 0    = xs
  | otherwise = xs +++ gyorsDiffLista (n-1) xs

lassuDiffLista :: Int -> DiffList Int -> DiffList Int
lassuDiffLista n xs
  | n == 0    = xs
  | otherwise = lassuDiffLista (n-1) xs +++ xs

-- most nincs kulonbseg a sebessegek kozott:
-- length $ toList $ lassuDiffLista 10000 (fromList [1])
-- length $ toList $ gyorsDiffLista 10000 (fromList [1])
-- (mindketto lassu)

-- javitsuk ki a fromList,+++-ot!

-- intuicio: dxs :: DiffList, az a fgv., ami egy lista ele rak "xs"-t

fromList' :: [a] -> DiffList a
fromList' xs = \ys -> xs ++ ys

(++++) :: DiffList a -> DiffList a -> DiffList a
dxs ++++ dys = \ zs -> dxs (dys zs)

-- i  :: Int helyett
-- i' :: (Int -> a) -> a  (CPS, continuation passing style)

gyorsDiffLista' :: Int -> DiffList Int -> DiffList Int
gyorsDiffLista' n xs
  | n == 0    = xs
  | otherwise = xs ++++ gyorsDiffLista' (n-1) xs

lassuDiffLista' :: Int -> DiffList Int -> DiffList Int
lassuDiffLista' n xs
  | n == 0    = xs
  | otherwise = lassuDiffLista' (n-1) xs ++++ xs

-- most nincs kulonbseg a sebessegek kozott:
-- length $ toList $ lassuDiffLista' 10000 (fromList' [1])
-- length $ toList $ gyorsDiffLista' 10000 (fromList' [1])
-- (mindketto gyors)

{-
Yoneda lemma -- kategoriaelmelet
  specialis esetei:
  [a] ≅ (f :: [a] -> [a]), es valamilyen feltetel f-re
           xs `isPrefixOf` f xs 

  b ≅ forall a. (b -> a) -> a

  F :: * -> * , functor
  F a ≅ y a -> F
  F a ≅ forall b . (a -> b) -> F b
  [a] ≅ forall b . (a -> b) -> [b]

  y b a ≅ y a -> y b
  y a a ≅ y a -> y a
  [a] ≅/ y a a ≅ y a -> y a ≅? [a] -> [a]
  
  F I ≅ yI -> F

  yI I = yI -> yI

  Cayley tétel

Pierce: Basic category theory for computer sciencists
- Functor
- Monad
- Category, Yoneda
-}
