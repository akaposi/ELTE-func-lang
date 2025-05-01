{-# language ViewPatterns #-}

-- Divianszky Peter eloadasa

import Data.List (transpose, nub, sort)
import Data.Bits (testBit, setBit, clearBit, shiftL, shiftR)
import Data.Ratio (numerator, denominator)
import Data.Complex (Complex ((:+)), realPart)
import qualified Data.IntMap as Map (IntMap, size, fromList, toList, keys, lookup, compose, elems)
import qualified Data.Vector.Mutable as V (MVector, new, read, write)
import Control.Monad (forM, forM_)
import Control.Monad.ST (ST, runST)
import System.Environment (getArgs)


------------------------------------

class Root f where
  mkRoot :: Rational -> f

instance RealFloat d => Root (Complex d) where
  mkRoot r = exp (2*pi* i * realToFrac r)
   where
    i = 0 :+ 1

------------------------------------

data F
  = MkRoot Rational    --  0 <= r  ,  r < 1
  | Zero
  deriving (Eq)

instance Show F where
  show (MkRoot r) = "{" ++ show (numerator r) ++ "/" ++ show (denominator r) ++ "}"
  show Zero = "0"

instance Root F where
  mkRoot = MkRoot . fraction
   where
    fraction r = r - fromIntegral (floor r)

instance Num F where

  a * 0 = 0
  0 * a = 0
  MkRoot a * MkRoot b = mkRoot (a + b)

  negate a = MkRoot (1/2) * a

  fromInteger 0 = Zero
  fromInteger 1 = mkRoot 0

  0 + a = a
  a + 0 = a

  abs    = undefined
  signum = undefined

------------------------------------- for testing

newtype Matrix f = MkMatrix [[f]]
  deriving Eq

instance Show f => Show (Matrix f) where
  show (MkMatrix fss)
    = unlines [unwords [show f | f <- fs] | fs <- fss]

matrixFun :: Num f => Matrix f -> [f] -> [f]
matrixFun (MkMatrix m) v
  = [row <.> v | row <- m]
 where
  as <.> bs = sum (zipWith (*) as bs)

funMatrix :: Num f => Int -> ([f] -> [f]) -> Matrix f
funMatrix k f
  = MkMatrix $ transpose [f (unitVec i) | i <- [0 .. k-1]]
 where
  unitVec i = [if j == i then 1 else 0 | j <- [0 .. k-1]]


------------------------------------- for testing

ftMatrix :: Root f => Int -> Matrix f
ftMatrix k
  = MkMatrix [ [ mkRoot (row*col / 2^k)
               | col <- [0..2^k-1]
               ]
             | row <- [0..2^k-1]
             ]


ftInvMatrix :: Root f => Int -> Matrix f
ftInvMatrix k
  = MkMatrix [ [ mkRoot (-row*col / 2^k)
               | col <- [0..2^k-1]
               ]
             | row <- [0..2^k-1]
             ]


------------------------------------

type Permutation = Map.IntMap Int

permFun :: Permutation -> Int -> Int
permFun m x = maybe x id (Map.lookup x m)

funPerm :: [Int] -> (Int -> Int) -> Permutation
funPerm dom f = Map.fromList [(x, f x) | x <- dom]

permDomain :: Permutation -> [Int]
permDomain = Map.keys

permImage :: Permutation -> [Int]
permImage = Map.elems

permSize :: Permutation -> Int
permSize = Map.size

shiftPerm :: Int -> Permutation -> Permutation
shiftPerm i p = Map.fromList [(i+a, i+b) | (a, b) <- Map.toList p]

compose :: Permutation -> Permutation -> Permutation
compose m n = Map.compose m n <> n <> m 

idPerm :: Permutation
idPerm = funPerm [] id


------------------------------------

data Mat f
  = Id
  | Mul  (Mat f) (Mat f)
  | Plus (Mat f) (Mat f)
  | Embed f
  | AddSub
  | PermMat Permutation
  deriving Show

instance Num (Mat f) where
  (*) = Mul
  (+) = Plus
  negate      = undefined
  abs         = undefined
  signum      = undefined
  fromInteger = undefined

-------------------------------------

rows    = fst . size
columns = snd . size

size Id = (1, 1)
size (Mul a b) = (rows a, columns b)
size (Plus (size -> (r, c)) (size -> (r', c'))) = (r + r', c + c')
size (Embed _) = (1, 1)
size AddSub = (2, 2)
size (PermMat (permSize -> i)) = (i, i)

------------------------------------- for testing

matFun :: Num f => Mat f -> [f] -> [f]
matFun Id [x] = [x]
matFun (Embed x) [y] = [x * y]
matFun AddSub [x, y] = [x + y, x - y]
matFun (Mul a b) xs = matFun a (matFun b xs)
matFun (Plus a b) (splitAt (columns a) -> (xs, ys)) = matFun a xs ++ matFun b ys
matFun (PermMat p) xs = [xs !! permFun p i | i <- [0 .. permSize p - 1]]

matToMatrix :: Num f => Mat f -> Matrix f
matToMatrix m = funMatrix (columns m) (matFun m)


------------------------------------- Cooley–Tukey algorithm

double a = a + a

bitSwapMat k = PermMat (funPerm [0..2^(k+2)-1] (bitSwap k (k+1)))
 where
  bitSwap i j n
    | n >= 0, n < 2^(k+2) = setBit' i (testBit n j) (setBit' j (testBit n i) n)
    | otherwise = n

setBit' i b n = if b then setBit n i else clearBit n i

{-
split 0 = double Id
split k = bitSwapMat (k-1) * double (split (k-1))
-}

split k = PermMat (funPerm [0..2^(k+1)-1] f)
 where
  f n = setBit' 0 (testBit n k) (shiftL (clearBit n k) 1)

merge k = PermMat (funPerm [0..2^(k+1)-1] f)
 where
  f n = setBit' k (testBit n 0) (shiftR n 1)

{-
addSub 0 = AddSub
addSub k = bitSwapMat (k-1) * double (addSub (k-1)) * bitSwapMat (k-1)
-}

addSub k = split k * f k * merge k
 where
  f 0 = AddSub
  f k = double (f (k-1))

scaleMat k = foldl (+) Id [Embed (mkRoot (i / 2^(k+1))) | i <- [1..2^k-1]]

fftMat 0 = Id
fftMat k = addSub (k-1) * (fftMat (k-1) + scaleMat (k-1) * fftMat (k-1)) * split (k-1)


scaleInvMat k = foldl (+) Id [Embed (mkRoot (- i / 2^(k+1))) | i <- [1..2^k-1]]

fftInvMat 0 = Id
fftInvMat k
   = addSub (k-1) * (fftInvMat (k-1) + scaleInvMat (k-1) * fftInvMat (k-1)) * split (k-1)

------------------------------------

-- sparse matrices
data Sparse f
  = SEmbed f Int
  | SAddSub Int Int
  | SPermutation Permutation
  deriving Show

mapIndex :: (Int -> Int) -> Sparse f -> Sparse f
mapIndex p (SEmbed s i)  = SEmbed s (p i)
mapIndex p (SAddSub i j) = SAddSub (p i) (p j)


------------------------------------ not used for c code generation

-- [a] -> [a]
type VecTrans a = forall s . V.MVector s a -> ST s ()

vecTransFun :: VecTrans f -> [f] -> [f]
vecTransFun vt fs = runST $ do
  v <- V.new (length fs)
  forM_ (zip [0..] fs) $ \(i, f) -> V.write v i f
  vt v
  forM [0..length fs-1] $ \i -> V.read v i

sparseVecTrans :: Num f => Sparse f -> VecTrans f
sparseVecTrans (SEmbed f i) v = do
  a <- V.read v i
  V.write v i (f * a)
sparseVecTrans (SAddSub i j) v = do
  a <- V.read v i
  b <- V.read v j
  V.write v i (a + b)
  V.write v j (a - b)
sparseVecTrans (SPermutation p) v = do
  fs <- forM (permImage p) $ \i -> V.read v i
  forM_ (zip (permDomain p) fs) $ \(i, f) -> V.write v i f

sparseFun :: Num f => Sparse f -> [f] -> [f]
sparseFun s = vecTransFun (sparseVecTrans s)

sparseToMatrix :: Num f => Int -> Sparse f -> Matrix f
sparseToMatrix k s = funMatrix k (sparseFun s)

------------------------------------

type Sparses f = [Sparse f]

sparsesFun :: Num f => Sparses f -> [f] -> [f]
sparsesFun ss = vecTransFun (\v -> forM_ ss (\s -> sparseVecTrans s v))

sparsesToMatrix :: Num f => Int -> Sparses f -> Matrix f
sparsesToMatrix k ss = funMatrix k (sparsesFun ss)


matToSparses :: Num f => Mat f -> Sparses f
matToSparses m = f 0 m []
 where
  f i Id = id
  f i (Mul  a b) = f i b . f i a
  f i (Plus a b) = f i a . f (i + columns a) b
  f i (Embed s) = (SEmbed s i:)
  f i AddSub = (SAddSub i (i+1):)
  f i (PermMat p) = (SPermutation (shiftPerm i p):)

fftSparses k = matToSparses (fftMat k)

fftInvSparses k = matToSparses (fftInvMat k)


------------------------------------

optimize :: Sparses f -> (Permutation, Sparses f)
optimize = f idPerm
 where
  f p (SPermutation q: ss) = f (compose p q) ss
  f p (s: ss) = (q, mapIndex (permFun p) s: r)
   where (q, r) = f p ss
  f p [] = (p, [])

includePerm :: (Permutation, Sparses f) -> Sparses f
includePerm (p, ss) = ss ++ [SPermutation p]


optimized_fftSparses k = optimize (fftSparses k)

optimized_fftInvSparses k = optimize (fftInvSparses k)

fftFun :: (Root f, Num f) => Int -> [f] -> [f]
fftFun k = sparsesFun $ includePerm $ optimized_fftSparses k

fftInvFun :: (Root f, Num f) => Int -> [f] -> [f]
fftInvFun k = sparsesFun $ includePerm $ optimized_fftInvSparses k


------------------------------------ c code generation

var n = "v" ++ show n

fC :: F -> String
fC (MkRoot r)
  | r == 1/4  = "I"
  | otherwise = show re ++ " + " ++ show im ++ " * I"
 where
  re :+ im = mkRoot r

sparseC :: Sparse F -> String
sparseC (SEmbed a n)  = var n ++ " *= " ++ fC a ++ ";"
sparseC (SAddSub a b)
  = "tmp = " ++ var a ++ " - " ++ var b ++ "; "
  ++ var a ++ " += " ++ var b ++ "; "
  ++ var b ++ " = tmp;"

fftC k = putStrLn $ unlines
  $ "#include <stdio.h>"
  : "#include <complex.h>"
  : ""
  : "int main() {"
  : "  double complex tmp;"
  : "  double x, y;"
  : ""
  : ["  scanf(\"%lf %lf\", &x, &y); double complex v" ++ show j ++ " = x + y * I;"
        | j <- [0..2^k-1]] ++ ""

  : map (("  " ++) . sparseC) is ++ ""

  : ["  printf(\"%.8f %.8f\\n\", creal(v" ++ show j ++ "), cimag(v" ++ show j ++ "));"
       | j <- map p [0..2^k-1]] ++ ""
  : "  return 0;"
  : "}"
  : []
 where
  (permFun -> p, is) = optimized_fftSparses k


------------------------------------ Strassen multiplication

base = 1000

coeffs :: Num f => Integer -> [f]
coeffs 0 = []
coeffs i = fromIntegral (mod i base): coeffs (div i base)

fromCoeffs :: RealFrac f => [f] -> Integer
fromCoeffs cs = foldr (\c x -> round c + base * x) 0 cs

extend k cs = take (2^k) (cs ++ repeat 0)

log2 :: Int -> Int
log2 0 = 0
log2 1 = 0
log2 n = 1 + log2 ((n-1) `div` 2 + 1)

ffts = [(fftFun k . extend k, map (/fromIntegral (2^k)) . fftInvFun k) | k <- [0..]]
  :: [([Complex Double] -> [Complex Double], [Complex Double] -> [Complex Double])]

mul :: Integer -> Integer -> Integer
mul a b = fromCoeffs $ map realPart fc
 where
  fa = coeffs a
  fb = coeffs b

  (fft, fftInv) = ffts !! log2 (length fa + length fb)

  fc = fftInv $ zipWith (*) (fft fa) (fft fb)

test a b = mul a b == a * b

------------------------------------

main = do
  args <- getArgs
  case args of
    -- fft c code generation
    ["fftC", k] -> fftC (read k)

    -- Strassen multiplication
    ["mul", a, b] -> do
      print $ mul (read a) (read b)

    -- Strassen multiplication after warmup
    ["mul2x", a, b] -> do
      print $ mul (read a) (read b)
      print $ mul (read a) (read b)

    -- test Strassen multiplication
    ["test", a, b] -> do
      print $ test (read a) (read b)
