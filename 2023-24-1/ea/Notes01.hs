{-
Funkcionális nyelvek = Halado Haskell

Kaposi Ambrus
Programozasi Nyelvek es Forditoprogramok

https://github.com/akaposi/ELTE-func-lang/tree/master/2023-24-1

16.00-17.30

Haskell: tiszta funkcionalis erosen statikus tipusos nyelv
-- Python dinamikusan tipusos
-- C gyengen tipusos
-- Haskellben a tipusban megjelenik, ha van mellekhatas
-- first class mellekhatas
-}
pl :: [Int -> Int]
pl = [(\x->x), (+1), (*2), (\_->3)]

-- pl' :: [Tipus]
-- pl' = [Int, Bool, String]
{-
elso funkcionalis nyelv: Lisp
elso imperativ nyelv: Fortran, asm
elso leirasa annak, hogy mi az, hogy algoritmus: lambda kalkulus (Church, Turing teljes)

kotesek: lim(1/(x+2))  = lim(1/(z+2)),    d(x²+1)          2            100
         x↦∞             z↦∞              -------          ∫ 1/i di      Σ(n)
                                            dx             0            n=0

         lim :: (Real -> Real) -> Real
         lim (\ x -> 1/(x+2))

(\ x -> x + 2)   = (\ y -> y + 2) = (\ kisnyuszi -> kisnyuszi + 2)

Moses Schönfinkel kombinátor logikája: S,K kombinátor

Mathematica (Stephen Wolfram), Maple, Octave
-}

k :: a -> b -> a
k a b = a                      -- kβ

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g a = f a (g a)            -- sβ
-- f :: a -> b -> c
-- g :: a -> b
-- a :: a
-- g a :: b

-- a -> b -> c = a -> (b -> c)
-- curry-zes (Haskell Curry)

-- <C>  C i(C x) { return x; } 
i :: forall c . c -> c
-- i x = x
i = s k k
-- s :: (a -> b -> a) -> (a -> b) -> a -> a
-- k   :: a -> b -> a
-- s k :: (a -> b) -> a -> a
-- s k :: (a -> a -> a) -> a -> a
-- k   :: a -> a -> a
-- s k k :: a -> a

-- egyenlosegi erveles:
-- be akarom bizonyitani, hogy i u = u minden u-ra

-- ( miert f(x) es nem x|f )

-- i u =(def) s k k u =(sβ) k u (k u) =(kβ) u
--            s f g a =     f a (g a)
--                          k a b     =     a
 
-- Fothi Akos

---------------------------------------------

-- kov.ora: Conor McBride tipusok termek munkasosztaly

-- data, osztalyok
