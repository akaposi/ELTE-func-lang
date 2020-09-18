
{-# language KindSignatures #-} -- language extension (lásd lejjebb)

-- Típusosztályok: bevezetés, semigroup, monoid, functor
--------------------------------------------------------------------------------

import Prelude
  hiding (Semigroup(..), Monoid(..), Functor(..), Applicative(..),
          Monad(..))

{-
Mai anyag: konkrét típusosztályok (Semigroup, Monoid, Functor). A félév során 3
osztály (Functor, Applicative, Monad) képezi az anyag jelentős részét. Ezekben
összesen 4 darab metódus található, viszont az osztályok kellően absztraktak
ahhoz, hogy jelentős időt el lehessen tölteni az egyes instance-okkal és a
használatukkal.
-}

{-
Néhány szót az osztályok idiomatikus használatáról. Először nézzünk egy
példát az osztályok "csúnya" használatára:
-}

class Default a where
  defaultVal :: a

{-
Ennek az osztálynak annyi a célja, hogy egy "default" értéket rendeljen típusokhoz.
A probléma az, hogy a Default instance-ok lényegében tetszőlegesek a legtöbb esetben.
-}

instance Default Bool where
  defaultVal = True -- False

instance Default Int where
  defaultVal = 0 -- 1

{-
A Haskell-ben a konvenció az, hogy az osztályokat nem ad-hoc túlterhelésre használjuk,
hanem elsősorban akkor, ha van valamilyen algebrai struktúra és specifikáció, amivel
az instance-okat megszoríthatjuk "jól viselkedő" esetekre.

Egyszerű példa az Eq (egyenlőségviszgálat). Itt azt szeretnénk, hogy az "(==) ::
a -> a -> Bool" metódus ekvivalencia-relációt döntsön el. Azaz (==) legyen
reflexív, szimmetrikus és tranzitív. Ez nem mindig teljesül; például a Double
(lebegőpontos típus) egyenlőség-vizsgálata nem reflexív, a NaN értékek miatt.
Pl ((0/0 :: Double) == (0/0 :: Double) == False). Viszont általános szabályként érdemes
jól viselkedő Eq-ra törekedni.

Ebből az is látszik, hogy a metódusok tulajdonságai *konvenciók*, amelyek
dokumentációként szerepelhetnek, viszont a GHC nem képes a specifikációkat
ellenőrizni. A GHC engedi, hogy helytelen instance-t írjunk, pl

  instance Eq Bool where
    (==) _ _ = True

Léteznek viszont nyelvek, ahol a fordító képes tetszőleges specifikációt
ellenőrizni, pl Agda, Coq, Idris.  Bizonyos MSc-s tárgyakon (nyelvek
típusrendszere, formális szemantika) Agda/Coq nyelvek ilyen képességét
használjuk formális specifikációra és bizonyításra.
-}

{-
Emlékeztető: operátor precedencia deklarálása
bármilyen függvény-definíció lehet operátor
-}
(**) :: Int -> Int -> Int
(**) x y = x + 3 * y

{-
Ugyanabban a fájlban (modulban), ahol a definíció szerepel, szerepelhet
fixitás deklaráció. A szám a kötési erősség, az "infixl" balra zárójelez, az
"infixr" jobbra, az "infix" pedig semerre, azaz ekkor mindig zárójelezni kell
kézzel a láncolt használatot.
-}
infixl 6 **


{- Semigroup = "félcsoport", azaz egy típus egy asszociatív bináris művelettel -}
class Semigroup a where
  infixr 5 <>              -- a fixitást megadhatjuk az osztály-deklarációban is,
  (<>) :: a -> a -> a      -- és általában konvenció itt megadni.

  -- (<>) legyen asszociatív:  ∀ x y z. (x <> y) <> z = x <> (y <> z)

instance Semigroup [a] where
  (<>) = (++) -- asszociativitás OK


{- Az Int típusra két természetes Semigroup instance is lehetséges:

  instance Semigroup Int where
    (<>) = (+)

  instance Semigroup Int where
    (<>) = (*)

Ilyen esetben a Haskell gyakorlat az, hogy nem választunk a két instance között,
hanem két newtype wrapper-t használunk az Int típuson, és a két wrapper-re különböző
instance-ot adunk meg.

A newtype használata hasonló a "data" deklárcióhoz. Pl

  newtype Foo = Foo Int

Ekkor létrejön a "Foo" mint új típus, és a "Foo :: Int -> Foo" mint új konstruktor.
"data"-val a következőt írtuk volna:

  data Foo = Foo Int

A különbség a "data" és "newtype" között:
  - newtype-nál pontosan egy konstruktort adhatunk meg pontosan egy mezővel.
    pl. "newtype Foo = Foo Int Int" nem valid, és "newtype Foo = Foo | Bar" sem.
  - A newtype-nak nincsen futásidejű költsége, kizárólag típusellenőrzés közben
    releváns.

Tehát a "newtype" felfogható a típusszinoníma ("type Foo = ...") erősen
típusozott variánsának, ahol a newtype egy új típus, aminek az értékei nem
keverhetők szabadon a wrap-elt típus értékeivel.
-}

-- mindkét newtype az Int-et wrap-eli, a különbség csak a Semigroup instance
-- a kettő között.
newtype Sum = Sum Int
newtype Prod = Prod Int

instance Semigroup Sum where
  Sum x <> Sum y = Sum (x + y)

instance Semigroup Prod where
  Prod x <> Prod y = Prod (x * y)

-- példák
sum1 :: Sum
sum1 = Sum 10 <> Sum 10    -- Sum 20

prod1 :: Prod
prod1 = Prod 10 <> Prod 10  -- Prod 100

sumFun :: Sum -> Int
sumFun (Sum n) = n + 100

prodFun :: Prod -> Bool
prodFun (Prod n) = n > 0

{-
Példa megszorított Semigroup instance-ra.
-}
instance Semigroup a => Semigroup (Maybe a) where
  Nothing <> _       = Nothing
  _       <> Nothing = Nothing
  Just x  <> Just y  = Just (x <> y)
  -- (asszociatív ez is)

-- példa:
-- Just "foobar" <> Just "!!!" == Just "foobar!!!"
-- Just (Sum 10) <> Just (Sum 20) == Just (Sum 30)


--------------------------------------------------------------------------------

{-
A Monoid osztály kiegészíti a Semigroup-ot egy egységelemmel a <> művelethez.
A "Semigroup a =>" megszorítás azt jelenti, hogy a Semigroup superclass-ja a
Monoidnak, azaz csak olyan "a"-ra lehet Monoid instance-ot írni, amire már
van Semigroup instance.
-}
class Semigroup a => Monoid a where
  mempty :: a
  -- tulajdonság:
  -- ∀ x. mempty <> x == x
  -- ∀ x. x <> mempty == x

instance Monoid [a] where
  mempty = []

instance Monoid Sum where
  mempty = Sum 0

instance Monoid Prod where
  mempty = Prod 1


-- házi feladat:
-- instance Monoid (a -> b)     (lehet hozzá tetszőleges constraint-et kitalálni!)
-- instance Monoid (a, b)
-- instance Monoid (a -> a)


-- Functor
--------------------------------------------------------------------------------

{-
A Functor osztály a lista "map" műveletet általánosítja.

  map :: (a -> b) -> [a] -> [b]

Itt a lista helyett szerepelhet bármilyen *típuskonstruktor*, amire
van Functor instance; lásd később, hogy mit értünk típuskonstruktor alatt.

Egyelőre annyit jegyezzünk meg, hogy a "Functor f"-ben szereplő "f" egy
olyan típus, amit alkalmazhatunk egy darab típusparaméterre. Erre egyszerű példa
a Maybe típuskonstruktor, ami alkamazható (Maybe a) módon tetszőleges típusra.
-}

class Functor f where
  fmap :: (a -> b) -> f a -> f b

  -- tulajdonságok:
  --   1. ∀ x.     fmap id x == x
  --   2. ∀ f g x. fmap (f . g) x == fmap f (fmap g x)

{-
A 1. tulajdonság azt fejezi ki, hogy a map-elés csakis az "a" típusú tárolt
értékeket változtathatja meg, az "f" szerekezét nem. Például Maybe esetén
a következő fmap nem helyes:

  fmap f (Just _) = Nothing

Mivel ekkor (fmap id (Just x) /= Just x).

A 2. tulajdonság egyfajta optimalizációt fejez ki, hogy két fmap helyett
lehet egyet használni, és akkor nincs szükség létrehozni egy közbülső "f"
struktúrát. Listák esetén a két alábbi definíció egyenlő:

  map (+10) $ map (+20) [0..10]
  map ((+10) . (+20)) [0..10]

A második verzió hatékonyabb, mivel eggyel kevesebb listát allokál.

A GHC-ben a standard lista típusra ez az átalakítás általában megtörténik, ha
optimalizációval fordítunk (azaz -O vagy -O2 opcióval).

Megjegyzés: bizonyos esetben *minden* közbülső lista eltűnik GHC optimalizáció
során, pl

  \n -> sum $ map (+10) $ map (+20) [0..n]

függvényből egy ciklus lesz, ami csak számokat növel és szummáz. De ez az átalakítás
már nem következik a 2. funktor törvényből, hanem extra mechanizmus szükséges hozzá,
amit nem tárgyalunk itt.

-}

instance Functor [] where
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap = map

{-
A típuskonstruktorokról: ha megadunk egy konkrét típust paraméterként, akkor
egy konkrét típust kapunk:

  Maybe       : konstruktor
  Maybe Int   : konkrét típus

A lista típus valójában a [] nevű konstruktor, és a [a] szintaxis csak cukorka.

  []          : konstruktor
  [Int]       : konkrét típus   (szintaktikus cukorka)
  [] Int      : konkrét típus   (prefix alkalmazása []-nak)
-}

list1 :: [] Int
list1 = [0..10]

maybe1 :: Maybe Int
maybe1 = Just 100

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing  = Nothing
  fmap f (Just a) = Just (f a)

{-

A "Functor f" instance-ban az "f" kizárólag 1 paraméteres konstruktor lehet, egyéként
hibát kapunk.

  instance Functor Int where -- hiba

Ez lényegében egy típushiba, viszont *típus szintű típushiba*. A típusok szintjén
is van típusozás! A paraméteres és konkrét típusok meg vannak különöztetve.

- A típusok típusait úgy is szoktuk hívni, hogy "kind".
- A konkrét típusok kind-ját úgy hívjuk, hogy * (csillag).
- Bármely két a,b kind-ra létezik az (a -> b) kind.

ghci-ben egy típus típusát le lehet kérdezni a következő módon:

  > :k <típus>
  típus :: kind

Pl:

  > :k Int
  Int :: *

Példák kind-okra:

  Maybe :: * -> *
  []    :: * -> *
  Maybe Int :: *
  Either :: * -> * -> *
  Either Int :: * -> *      -- parciális applikáció típusokra is!
  Either Int Int :: *

Tetszőleges kind-ú típust data/newtype segítségével lehet létre hozni.

Ha a következőt a fájl elejére tesszük: {-# language KindSignatures #-}, akkor
bekapcsoljuk a lehetőséget, hogy data/newtype/class deklarációban kind-okat annotáljunk.

Pl a Functor deklarációt írhatjuk így:

  class Functor (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b

Az Either-t pedig így:

  data Either (a :: *) (b :: *) = Left a | Right b

Ha valahol olyan kind-ot használunk, ami nem *, akkor jó ötlet ezt annotációval jelezni.

Példa (* -> *) használatára data definícióban:

  data MyData (f :: * -> *) = MyData (f Int) (f Bool)

  myData :: MyData Maybe
  myData = MyData (Just 10) (Just True)

  myData2 :: MyData []
  myData2 = MyData [10] [True, False]

-}


{-
Functor instance függvény típusra, fixált input típussal. Itt azt érdemes tudni, hogy
a függvény nyíl is csak szintaktikus cukor! Azaz létezik a ((->) :: * -> * -> *) típus
konstruktor, ami megadja a függvény típust.

Ha parciálisan alkalmazzuk a (->)-at, akkor kapunk egy (* -> *) kind-ú kifejezést, amire
lehet Functor instance-ot írni.
-}

instance Functor ((->) c) where   -- függvény c-ből
  -- fmap :: (a -> b) -> (c -> a) -> (c -> b)
  -- fmap :: (a -> b) -> ((->) c a) -> ((->) c b)
  -- fmap :: (a -> b) -> (((->) c) a) -> (((->) c) b)
  fmap f g = \c -> f (g c)
  -- fmap = (.)

{-
Ez példa arra, hogy nem csak véges adatstruktúra lehet funktor, hanem minden olyan
típus, ami "tárol" "a"-típusú értékeket. Egy "a -> b" függvény minden "a" értékre
tárol egy "b" értéket.
-}

{-
Példa: a Pair funktor ekvivalens a ((->) Bool) funktorral.
Minden a-ra, (Bool -> a) ekvivalens az (Pair a) típussal.
-}
data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)
-- Pair a ~ (Bool -> a)

{-
Milyen olyan típus van, ami f :: * -> *, viszont nincsen Functor instance rá?
-}

-- Például: "a"-t nem tárolja Bar, hanem inputként várja. Nincs Functor Bar!
data Bar a = Bar (a -> Bool)

-- Kitekintés: kontravariáns funktor osztály (fordított map)
class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

instance Contravariant Bar where
  contramap f (Bar g) = Bar (g . f)
