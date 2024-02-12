{-

Funkcionalis nyelvek = Halado Haskell

Kaposi Ambrus, eloadas

Haskell = Clean = Miranda: "tiszta" funkcionalis erosen statikus tipusos nyelv, first-class mellekhatasokkal

tiszta = tipus oszinte, nem oszinte: error, exception, vegtelen ciklus
  - OCaml, ML, F#: nem tisztak
erosen tipusos: minden termnek/programnak/erteknek van tipusa:
- Python
- Java, C#, Scala, Rust
nem erosen tipusos: C, C++
nem statikus: Python, Java-ban nincs kikenyszeritve

first-class mellekhatas, pl. az IO muveleteket lehet listakba pakolni stb.

Haskell = legjobb imperativ programozasi nyelv

f :: Int -> Int
f = f     ___
         /   \
         v    \
kotes = (λ -> ■ + 1) = (λ alma → alma + 1) = (λ korte → korte + 1)
 "kotott valtozo neve nem szamit"

int f(int y) { return (y + 1); }

_
∫ (λ x → 1/x) d :: R -> R -> (R -> R) -> R
_

1
∫ 1/x dx
0

lim (1/(1+x))
x↦∞

let :: a -> (a -> b) -> b
let :: Int -> (Int -> Int) -> Int
let x = 3 in x + 2

for (i = 1 to 3) {
 ...itt van i...
}

David Turner - Miranda nyelv alkotoja = Elementary strong functional programming

  pl. fuggo tipusos nyelvek: Agda, Coq, Idris, Lean, F*

Ismétlés, parametrikus ADTk (, , Either, ->), case, let kötések, Eq, Show, Ord

egyenlosegi erveles
-}

data Egtaj = Kelet | Nyugat | Eszak | Del

fordul :: Egtaj -> Egtaj
fordul Kelet = Eszak
fordul _ = error "ilyen nincs!"

-- Peano
data Nat = Zero | Suc Nat
-- 3 = Suc (Suc (Suc Zero))

add :: Nat -> Nat -> Nat
add Zero    b = b
add (Suc a) b = Suc (add a b)

three :: Nat
three = Suc (Suc (Suc Zero))

class MyNum a where
  toIntegr :: a -> Integer

-- (1,2) :: (Int,Int)
-- λx.2  :: Int -> Int
-- () :: ()
-- [] :: List a
-- :   ::

newtype Csomag a = Csomag a

instance (MyNum a) => Show (Csomag a) where
  show (Csomag x) = show (toIntegr x)

instance MyNum Nat where
  toIntegr Zero = 0
  toIntegr (Suc n) = 1 + toIntegr n

{-
instance Show Nat where
  show :: Nat -> String
  show Zero = "0"
  show (Suc n) = "1+" ++ show n -}

newtype Osszeadassal a = Osszeadassal a
  deriving (Show)

instance Semigroup (Osszeadassal Integer) where
  Osszeadassal x <> Osszeadassal y = Osszeadassal (x + y)
instance Monoid (Osszeadassal Integer) where
  mempty = Osszeadassal 0

newtype Szorzassal a = Szorzassal a
  deriving (Show)

instance Semigroup (Szorzassal Integer) where
  Szorzassal x <> Szorzassal y = Szorzassal (x * y)
instance Monoid (Szorzassal Integer) where
  mempty = Szorzassal 1

-- mempty `mappend` x = x


-- grupoid = magma > felcsoport > monoid > csoport > Abel-csoport > ...
