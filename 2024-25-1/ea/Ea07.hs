-- Diviánszky Péter előadása

{-
I.   Lens use cases
II.  How Lens works
-}

{-# options_ghc -fdefer-type-errors #-}
{-# language NoMonomorphismRestriction #-}

import Data.Char
import Data.Complex
import Data.Function
import Data.List
import qualified Data.Vector as Vector
import qualified Data.Set as Set
import qualified Data.Map as Map

-- from lens package
import Control.Lens      
import Data.Bits.Lens
import Data.Vector.Lens
import Data.Set.Lens
import Data.Complex.Lens
import Data.List.Lens
import Data.Map.Lens
import System.FilePath.Lens
import Numeric.Lens

{-

Types of optics discussed here:

           Iso
          /   \
      Lens     Prism
     /    \   /
  Getter  Setter

-}

--------------------------------------- Iso

-- Iso' a b  ~  (a -> b, b -> a)

-- view :: Iso' a b -> a -> b
-- (^.) :: a -> Iso' a b -> b
-- (^.) = flip view

-- adding, multiplying :: Double -> Iso' Double Double

e1  = 34 ^. adding 10
e2  = 2  ^. multiplying 2

-- from :: Iso' a b -> Iso' b a

e3  = 34 ^. from (adding 10)
e4  = 2  ^. from (multiplying 2)
e5  = 2  ^. from (exponentiating 2)

-- (.) :: Iso' a b -> Iso' b c -> Iso' a c

fahrenheit = multiplying (9/5) . adding 32

e6  = 0 ^. fahrenheit
e7  = 0 ^. from fahrenheit

-- over :: Iso' a b -> (b -> b) -> a -> a
-- (&) = flip ($)

e8  = 0 & over fahrenheit (+1)

-- (%~) = over

e9  = 0 & fahrenheit %~ (+1)

e10 = 3 & negated %~ (+1)

-- enum    :: Enum a => Iso' Int a

e11 = 'a' & from enum %~ (+1)

-- curried  :: Iso' ((a, b) -> c) (a -> b -> c)
-- flipped  :: Iso' (a -> b -> c) (b -> a -> c)

-- swapped  :: Iso' (a, b) (b, a)
-- swapped  :: Iso' (Either a b) (Either b a)

-- vector   :: Iso' [a] (Vector a)

e12 = [1,2,3] ^. vector

-- reversed :: Iso' [a] [a]
-- reversed :: Iso' (Vector a) (Vector a)

e13 = "live" & reversed %~ ('d':)

-- mapping :: Iso' a b -> Iso [a] [b]

e14 = "Hello" ^. mapping (from enum)
e15 = ["Hello", "World"] ^. mapping (mapping (from enum))

-- mapping :: Iso' a b -> Iso' (Vector a) (Vector b)
-- mapping :: Iso' a b -> Iso' (r -> a) (r -> b)


--------------------------------------- Lens

-- Lens' a b  ~  (a -> b, b -> a -> a)

-- view :: Lens' a b -> a -> b
-- (^.) :: a -> Lens' a b -> b

-- directory, filename, basename, extension
--    :: Lens' FilePath FilePath

e16 = "a/b/c.txt" ^. directory
e17 = "a/b/c.txt" ^. filename
e18 = "a/b/c.txt" ^. basename
e19 = "a/b/c.txt" ^. extension

-- set :: Lens' a b -> b -> a -> a
-- (.~) = set

e20 = "a/b/c.txt" & extension .~ ".hs"

-- (%~) :: Lens' a b -> (b -> b) -> a -> a

e21 = "a/b/c.txt" & basename %~ (++ "1")

-- (.) :: Lens' c b -> Lens' a b -> Lens' a c

e22 = "a/b/c.txt" ^. directory . directory

-- _realPart, _imagPart
--    :: Lens' (Complex Double) Double

e23 = (3 :+ 4) ^. _realPart
e24 = 3 :+ 4 & _realPart .~ 8
e25 = 3 :+ 4 & _realPart %~ (* 8)

-- _conjugate :: Iso' (Complex Double) (Complex Double)

e26 = (3 :+ 4) ^. _conjugate

-- (.) :: Iso' c b -> Lens' a b -> Lens' a c
-- (.) :: Lens' c b -> Iso' a b -> Lens' a c

e27 = 2 :+ 3 & _conjugate . _imagPart %~ (+1)

-- _polar :: Iso' (Complex Double) (Double, Double)

e28 = (1 :+ 1) ^. _polar

-- _1 :: Lens' (a, b) a
-- _2 :: Lens' (a, b) b

e29 = (1 :+ 1) & _polar . _1 %~ (*2)

-- bitAt  :: Int -> Lens' Int Bool
-- byteAt :: Int -> Lens' Int Word8

e30 = (16     :: Int) ^. bitAt 4
e31 = (0xff01 :: Int) ^. byteAt 0

e32 = 16     & bitAt 4 .~ False
e33 = 0xff01 & byteAt 0 .~ 2
e34 = 0xff01 & byteAt 0 . bitAt 0 %~ not

-- contains :: a -> Lens' (Set a) Bool

e35 = Set.fromList [1,2,3,5] ^. contains 3
e36 = Set.fromList [1,2,3,5] & contains 4 .~ True
e37 = Set.fromList [1,2,3,5] & contains 3 %~ not

-- at :: a -> Lens' (Map a b) (Maybe b)

e38 = Map.fromList [(0, "hello"), (1, "world")] ^. at 1
e39 = Map.empty & at 1 .~ Just "world"

-- choosing  :: Lens' a b -> Lens' c d
--           -> Lens' (Either a c) (Either b d)
-- alongside :: Lens' a b -> Lens' c d
--           -> Lens' (a, c) (b, d)

-- sliced :: Int -> Int -> Lens' (Vector a) (Vector a)

e40 = Vector.fromList [1..10] ^. sliced 2 5


------------------------------------- Setter

-- Setter' a b  ~  b -> a -> a

-- (.~) :: Setter' a b -> b -> a
-- (%~) :: Setter' a b -> b -> a -> a

-- mapped :: Setter' (Vector a) a

e41 = Vector.fromList [1..10] & sliced 2 5 . mapped .~ 0


------------------------------------- Getter

-- Getter a b  ~  a -> b

-- (^.) :: a -> Getter a b -> b


------------------------------------- Prism

-- Prism' a b  ~  (a -> Maybe b, b -> a)

-- (^?) :: a -> Prims' a b -> Maybe b

-- base :: Int -> Prism' String Int

e42 = "100" ^? base 16

-- re :: Prims' a b -> Getter b a

e43 = 1767707668033969 ^. re (base 36)

-- integral :: Integral a => Prism' Integer a

e44 = (2^100) ^? integral   :: Maybe Int

-- _head, _last :: Prism' [a] a
-- _tail. _init :: Prism' [a] [a]

-- (.~) :: Prism' a b -> b -> a

e45 = [0,1,2] ^? _head
e46 = [0,1] & _head .~ 2
e47 = [1,2] & _tail .~ [3,4,5]

-- ix :: Int -> Prism' [a] a

e48 = [0..10] ^? ix 4
e49 = [0..5] & ix 4 .~ 2
e50 = [0..5] & ix 14 .~ 2

-- ix :: a -> Prism' (Map a b) b

e51 = Map.fromList [(2, "Earth")] & ix 2 %~ ("New " ++)
e52 = Map.fromList [("John", (True, 3))
                  , ("Sally", (False, 1))]
              & ix "John" . _2 %~ (+1)

e53 = "preview" ^? prefixed "pre"
e54 = "hello" ^. re (suffixed ".o")

-- _Left  :: Prism' (Either a b) a
-- _Right :: Prism' (Either a b) b

-- _Just    :: Prism' (Maybe a) a
-- _Nothing :: Prism' (Maybe a) ()

-- _Show :: (Read a, Show a) => Prism' String a

e55 = "[1,2,3]" ^? _Show  :: Maybe [Int]
e56 = "[1,2,3 " ^? _Show  :: Maybe [Int]

-- below :: Prism' a b -> Prism' [a] [b]

e57 = [Right "foo", Right "blah", Right "woot"] ^? below _Right
e58 = [Right "foo", Right "blah", Left  "woot"] ^? below _Right



------------------------------------- polymorphism

e59 = ('a', True) & _2 %~ show


-- _1   :: Lens' (a, b) a
-- (%~) :: Lens' a b -> (b -> b) -> a -> a
--         --->
-- _1   :: Lens (a, b) (a', b) a a'
-- (%~) :: Lens a a' b b' -> (b -> b') -> a -> a'

-- Lens' a b  ~  (a -> b, b -> a -> a)
--            ~  (a -> b, a -> (b -> a))
--            ~  a -> (b, b -> a)

-- Lens a a' b b'   ~  a -> (b, b' -> a')
-- Lens' a b = Lens a a b b


------------------------------------- composition

-- Lens a a' b b'
--   ~  a -> (b, b' -> a')
--   ~  forall f. Functor f => (b -> f b') -> a -> f a'


iso1 :: (a -> (b, b' -> a'))
   -> forall f. Functor f => (b -> f b') -> a -> f a'
iso1 f g a = fmap h (g b)
 where
  (b, h) = f a

iso2 :: (forall f. Functor f => (b -> f b') -> a -> f a')
   -> a -> (b, b' -> a')
iso2 f a =
  ( getConst (f Const a)
  , \b' -> runIdentity (f (Identity . const b') a)
  )



{-
type Lens s t a b =
  forall f. Functor f =>
    (a -> f b) -> s -> f t

type Setter s t a b =
  forall f. Settable f =>
    (a -> f b) -> s -> f t

type Getter s a =
  forall f. (Contravariant f, Functor f) =>
    (a -> f a) -> s -> f s

type Iso s t a b =
  forall p f. (Profunctor p, Functor f) =>
    p a (f b) -> p s (f t)

type Prism s t a b =
  forall p f. (Choice p, Applicative f) =>
    p a (f b) -> p s (f t)
-}



