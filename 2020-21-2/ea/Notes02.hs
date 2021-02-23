
--------------------------------------------------------------------------------

-- ADT-k
-- lawful osztályok
-- (Semigroup, Monoid) : röviden
-- Functor

--------------------------------------------------------------------------------

-- Algebrai adattípusok.
-- Miért "algebrai"?
--   Öszeadás, szorzás, hatványozás : meg lehet adni, hogy bizonyos ADT-knek hány darab lehetséges értéke van


-- 1  (standard: ()  "unit" típus, értéke: ())
data One = One     -- egy darab konstruktor: One

one :: One
one = One

-- 0
data Zero          -- nulla darab konstruktor

-- instance Show Zero where
--   show x = seq x undefined

-- (az egyetlen lehetséges érték a végtelen loop)
zero :: Zero
zero = zero   -- rekurzív definíció


-- összeadás
-- standard típus: data Either a b = Left a | Right
data Plus a b = Inl a | Inr b

-- Notáció: |Zero| = 0
--          |One|  = 1

-- |Plus a b| = |a| + |b|

p1 :: Plus One One
p1 = Inl One

p2 :: Plus One One
p2 = Inr One

-- |Plus Bool Bool| = 4

-- szorzás: standard (a, b)
data Mul a b = Pair a b

-- |Mul Bool (Plus One Bool)| = 6
m1 :: Mul Bool (Plus One Bool)
m1 = Pair True (Inr False)

-- azonosság
-- mit értünk két addattípus "egyenlőségén"?
--    (a ~ b) : ha van (f :: a -> b) aminek van egy inverz függvénye (hasonló: halmazok bijekciója)

-- a + 0 = a
-- Plus a Zero ~ a

to :: Plus a Zero -> a
to (Inl a) = a

from :: a -> Plus a Zero
from a = Inl a


-- a + b = b + a
-- Plus a b ~ Plus b a
plusComm :: Plus a b -> Plus b a
plusComm (Inl a) = Inr a
plusComm (Inr b) = Inl b


-- a * 1 = a
mulId :: Mul a One -> a
mulId (Pair a One) = a

mulId' :: a -> Mul a One
mulId' a = Pair a One

-- házi feladat: függvények: (a + b) + c = a + (b + c), (a * b) * c = a * (b * c)

-- disztributivitás:
-- a * (b + c) ~ (a * b) + (a * c)
distrib :: Mul a (Plus b c) -> Plus (Mul a b) (Mul a c)
distrib (Pair a (Inl b)) = Inl (Pair a b)
distrib (Pair a (Inr c)) = Inr (Pair a c)

distrib' :: Plus (Mul a b) (Mul a c) -> Mul a (Plus b c)
distrib' (Inl (Pair a b)) = Pair a (Inl b)
distrib' (Inr (Pair a c)) = Pair a (Inr c)

-- típus ekvivalencia : refaktorálás eszköze (bármilyen programot át lehet írni egy típusról egy ekvivalens típusra)
--    lehet kényelmi/hatékonysági különbség

-- gyakran használjuk a köv. függvényt:

getA :: Plus (Mul a b) (Mul a c) -> a
getA (Inl (Pair a b)) = a
getA (Inr (Pair a c)) = a

getA' :: Mul a (Plus b c) -> a
getA' (Pair a _) = a
-- ekkor getA' hatékonyabb

-- hatványozás: |Exp a b| = |a|^|b|
type Exp a b = (b -> a)

-- négyzet:   Exp a Bool     a^2
--   |Exp a Bool| = |a|^2 = |a|*|a|

-- Exp a Bool = (Bool -> a)

squareEq :: Exp a Bool -> Mul a a
squareEq f = Pair (f True) (f False)

squareEq' :: Mul a a -> Exp a Bool
squareEq' (Pair a a') = \b -> if b then a else a'

-- általánosan:   a^b = (a * a * a * a * .... a)   (b-darab "a" szorzata)

-- Mi helyzet: a^0     (Zero -> a)
--  a^0 = 1

expZero :: Exp a Zero -> One
expZero f = One

expZero' :: One -> Exp a Zero
expZero' One = undefined           -- ez a definíció is totális (minden lehetséges bemetre véges idő alatt értékkel tér vissza)
                                   --                            itt: nincses lehetséges bemenet!


expZero'' :: One -> (Zero -> a)
expZero'' One = undefined


-- kérdés: Zero típus mire használható?
--         Haskell-ben:     valamilyen generikus függvényeket exportáló library

-- data Widget a b = Con1 a | Con2 b | MyWidget .... | MyWidget2 ....
--    library felhasználójának nincs szüksége a Con1 által nyújtott funkcióra?
--    type Widget' b = Widget Zero b            (Con1 konstruktor már nem lehetséges, nem kell mintát illeszteni a Con1-re sehol)

-- házi feladat: Exp Zero a?
--   0^a        (a -> Zero)

--   Házi feladat:
--   Ha |a| =  0 akkor |a -> Zero| = 1
--      |a| /= 0 akkor |a -> Zero| = 0

-- Osztályok
--------------------------------------------------------------------------------

-- Eq, Show, Ord   (egyszerű, kényelmi feature)

-- Mikor érdemes új osztályt létrehozni?

-- Példa arra, hogy mikor *nem* érdemes osztályt csinálni:

class Default a where        -- olyan "a" típusok osztálya, ahol van egy "default" érték
  def :: a

-- Mi alapján választok?

-- instance Default Bool where
--   def = True

-- instance Default Bool where
--   def = False

-- instance Default Int where
--   def = ...

-- class Coerce a b where        -- típuskonverziók osztálya, overloaded coerce függvény (lásd pl C++ implicit konverziók)
--   coerce :: a -> b


-- Haskell gyakorlat:
--  ideális: osztály megfelel valamilyen algebrai struktúrának / fogalomnak
--           ha van valamilyen algebrai azonosság, ami leírja, hogy mi a helyes instance

class Eq a where
  (==) :: a -> a -> Bool
  -- (==) adjon meg egy ekvivalencia relációt:
  --  ∀ x y. x == y → y == x
  --  ∀ x. x == x
  --  ∀ x y z. x == y -> y == z -> x == z

  -- (IEEE floating point megsérti ezt (NaN miatt))
  -- Eq Double (szintén "illegális" Eq instance)


-- standard osztályok:

-- class Semigroup a where        -- félcsoport:
--   (<>) :: a -> a -> a
--   -- spec: (<>) legyen asszociatív

-- class Semigroup a => Monoid a where   -- monoid
--   mempty :: a
--   -- spec: mempty legyen a (<>) az egységeleme
--   -- ∀ x. x <> mempty = x
--   -- ∀ x. mempty <> x = x

-- (kérdés: GHC használja-e az azonosságokat fordításkor?)
--   alapból nem, programozó írhat ún. REWRITE pragma (házi: REWRITE pragma GHC)

-- pl:
-- instance Semigroup [a] where
--   (<>) = (++)

-- instance Monoid [a] where
--   mempty = []

-- Házi feladat: Prelude/egyéb doc: milyen Semigroup és Monoid instance-ok vannak definiálva


-- Functor
--------------------------------------------------------------------------------

-- Félév során (hierarchia 3 osztállyal):
--   Functor => Applicative => Monad
--   Összesen 4 darab metódus
--   (3 osztály + 4 metódus : félév jelentős része)

-- (kérdés: Selective hol van a hierarchiában? Válasz: nézd meg a cikkben ami a Selective funktor-ról szól)


-- Functor-ban olyan adatstruktúrák vannak, amelyekre lehet "map" függvényt alkalmazni

-- class Functor f where             -- "f" nem konkrét típus, hanem egy 1-paraméteres típus
--    fmap :: (a -> b) -> f a -> f b   -- (a -> b) függvénnyel map-elünk (f a) fölött, és kapunk (f b)-t
--    -- spec:
--    -- ∀ x. fmap id x = x  -- id függvénnyel map-elés nem csinál semmit (kizárja: lista map, ami megfordítja az eredményt)
--    -- ∀ f g x. fmap f (fmap g x) = fmap (f . g) x  -- "optimalizációs" szabály: két map-elés összevonható (kevesebb bejárás)


-- instance Functor [] where
--   fmap :: (a -> b) -> [] a -> [] b
--   fmap :: (a -> b) -> [a] -> [b]            -- "[a]" cukorka az "[] a" kifejezésre
--   fmap = map

-- instance Functor Maybe where
--   -- fmap :: (a -> b) -> Maybe a -> Maybe b
--   fmap f Nothing = Nothing
--   fmap f (Just a) = Just (f b)

-- a^2 = a*a
-- Square a ~ (Bool -> a)
data Square a = Square a a

instance Functor Square where
  fmap f (Square a1 a2) = Square (f a1) (f a2)

-- Általánosan: mi Functor?
--   ha (Functor f), akkor (f a) olyan adatstruktúra, amiből ki lehet nyerni "a" típusú értékeket

data Fun a b = Fun (a -> b)    -- (Fun a) 1-paraméteres típus

instance Functor (Fun c) where       -- ha lerögzítjük a függvény bemenő típusát, akkor az Functor-e?
  -- fmap :: (a -> b) -> Fun c a -> Fun c b
  fmap f (Fun g) = Fun (f . g)       -- házi feladat: megnézni, hogy miért helyes ez a definíció.
                                     -- függvény-ek fmap-ja = kompozíció
