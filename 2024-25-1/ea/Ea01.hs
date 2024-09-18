import Prelude hiding (Either, Left, Right)

-- funkcionalis nyelvek = halado Haskell
-- Kaposi Ambrus, 16.00--17.30, ea+gy, github.com/akaposi/ELTE-func-lang/
-- https://github.com/akaposi/ELTE-func-lang/tree/master/2024-25-1

{-
- tiszta funkcionalis erosen statikus tipusos nyelv, first-class mellekhatasokkal
  - tiszta: "mellekhatas-mentes", x :: Int
    - ha ez valami ertelmes, akkor egy Int
    - nem csinal kozben mast, pl. nem ir a merevlemezre
    - lehet, hogy vegtelen ciklus
    - lehet, hogy exceptiont ad
    - pl. Agda, Coq <- strong functional programming
    - Java, Python, C#, C++, Rust, JavaScript, PHP <- mellekhatasok nem latszanak a tipusban
  - funkcionalis: "ez nem jelent semmit"
    - ML(amerikai), OCaml(francia,amerikai), F#, Scala, Closure, Lisp, Erlang, Elixir
      - mind piszkosabbak a Haskellnel
    - Haskell: lusta (Miranda(angol), Clean(holland))
      (\x.x+x) (1+2) ~~> (\x.x+x) 3 ~~> 3+3 ~~> 6   szigoru(call by value, ertek szerinti parameteratadas)
      (\x.x+x) (1+2) ~~> (1+2)+(1+2) ~~> 3+(1+2) ~~> 3+3 ~~> 6  (call by name, nev szerinti parameteratadas)
      (\x.10) (1+2) ~~> (\x.10) 3 ~~> 10 (ertek szerinti)
      (\x.10) (1+2) ~~> 10 (nev szerinti)
      (\x.x+x) (1+2) ~~> let n:=1+2 in n+n ~~> let n:=3 in n+n ~~> let n:=3 in 3+n ~~> let n:=3 in 3+3 ~~> 6
      (\x.10) (1+2) ~~> let n:=1+2 in 10 ~~> 10
  - statikus tipusos nyelv
    - dinamikusan: Python, JavaScript, PHP, Ruby, Erlang
    - gyengen tipusos: C, C++
  map :: (a -> b) -> [a] -> [b]
  [IO ()]
-}

-- algebrai adattipusok

data Egtaj = Kelet | Nyugat | Eszak | Del
  deriving (Show)

fordul :: Egtaj -> Egtaj
fordul Kelet = Del
fordul Nyugat = Eszak
fordul _ = Kelet

data Nat = Zero | Suc Nat -- rekurziv tipus

instance Show Nat where
  show :: Nat -> String
  show n = show (conv n)
    where
      conv :: Nat -> Int
      conv Zero = 0
      conv (Suc n) = 1 + conv n
      
nulla, egy, ketto, harom :: Nat
nulla = Zero
egy = Suc nulla
ketto = Suc egy
harom = Suc ketto

-- plus
plus :: Nat -> Nat -> Nat
plus x Zero = x
plus x (Suc y) = Suc (plus x y)

data HomPair a = HomPair a a
  deriving (Show)

pl :: HomPair Int
pl = HomPair 3 4

b1,b2,b3,b4 :: HomPair Bool
b1 = HomPair False False
b2 = HomPair False True
b3 = HomPair True False
b4 = HomPair True True

-- data a × b = (,) a b
-- data (,) a b = (,) a b

-- a + b = Left a | Left b
data Either a b = Left a | Right b
  deriving (Show)

c1,c2,c3,c4 :: Either Bool Bool
c1 = Left False
c2 = Left True
c3 = Right False
c4 = Right True

-- () :: () unit -- nullaris szorzat tipus
-- ((),a) = a

-- ures tipus, nullaris osszeg tipus
data Empty
d1 :: a -> Either Empty a
d1 x = Right x

ilyennincs1, ilyennincs2, ilyennincs3 :: Either Empty Empty
ilyennincs1 = undefined
ilyennincs2 = Left undefined
ilyennincs3 = Right undefined

f :: Either Empty Empty -> Bool
f (Left _) = True
f (Right _) = False

-- kovetkezo ora 7 perccel rovidebb

-- ->), case, let kötések, Eq, Ord


-- {-# options_ghc -fwarn-incomplete-patterns #-}

