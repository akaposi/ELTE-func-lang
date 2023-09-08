
### Bevezetés
#### Mi az, hogy "tisztán funkcionális" programozás?

A "funkcionális" és "tisztán funkcionális" jelzővel gyakran illetik a Haskell nyelvet. Néha azt lehet olvasni, hogy a tiszta funkcionális programozásban nincsenek mellékhatások. Ez nem igaz; a Haskell-ben például a következő dolgokat boldogan el tudjuk végezni:

- Tetszőleges input-ouput műveletek, fájlrendszer-műveletek, hálózati műveletek.
- Kivételek dobása, akár aszinkron módon is.
- Memória destruktív írása.
- Többszálú/konkurrens programozás.

Azaz Haskell-ben lényegében ugyanazok a mellékhatások elérhetők, mint bármelyik produkciós programozási nyelvben.

A lényeges különbség az, hogy Haskell-ben a legtöbb mellékhatás *first-class*, és *típusozott*.

- *First-class* alatt az értendő, hogy a mellékhatás létrehozása és végrehajtása elkülönül. A létrehozás fázisában a mellékhatás ugyanolyan adat, mint bármi más, és függvénynek átadható, függvénnyel kiszámolható, kombinálható, és tetszőleges adatstruktúrában tárolható. A mellékhatás végrehajtása pedig precízen kontrollálható. Például létrehozhatunk egy IO műveleteket tartalmazó listát, majd az összes műveletet sorba fűzhetjük egy nagy műveletté, majd végül átadjuk végrehajtásra a GHC runtime system-nek. Míg a jól ismert programozási nyelvekben tudunk kivételt dobni, Haskell-ben akár *kivétel-dobások listáját* is létre tudjuk hozni.

- *Típusozott* mellékhatás alatt az értendő, hogy egy program típusából kiderül, hogy milyen mellékhatást végezhet el. Illetve: a mellékhatás-mentes kód típusából kiderül, hogy mellékhatás-mentes. Ez utóbbi az igazán lényeges előny és a fő motiváció.

Viszont: az nem igaz, hogy a Haskell *minden* potenciálisan érdekes hatást lekövet.

Például: a Haskell egyáltalán nem kezeli az ún. parciálitás hatást: ha egy Haskell függvény bizonyos inputra végtelen ciklusba kerülhet, az nem látszik meg a függvény típusában. [Agda](https://agda.readthedocs.io/), [Coq](https://coq.inria.fr/) és [Idris](https://www.idris-lang.org/) nyelvekben a totális függvények típusából kiderül, hogy soha nem kerülnek végtelen ciklusba.

Továbbá, a Haskell alapból nem tekinti hatásnak a memória-allokációt, azaz a típusok nem mondanak arról semmit, hogy mennyi stack/heap allokációk hajthat végre egy függvény, vagy hogy pontosan mikor szabadíthatók fel memóriaterületek. A [Rust](https://www.rust-lang.org/en-US/) nyelvben az allokált memória élettartama például megjelenik típusszinten.


## Haskell: algebrai adattípusok

Új típusok létrehozására alapvető az ADT (algebraic data type) deklaráció. Az "algebrai" jelentésére később visszatérünk; a lényeg, hogy kis számú primitív típusból indulunk ki, és kis számú művelet segítségével új típusokat hozunk létre a meglévőkből. A következőkben négy típusképzési módszert tárgyalunk: tuple-képzést, unió-képzést, paraméterezést és rekurzív típusdefiniálást. Mind a négy része az általános ADT deklaráció sémájának.

#### Tuple-képzés

Véges számú meglévő típusnak képzhetjük tuple-jét:

```haskell
data IntAndBool = MkIntAndBool Int Bool
  deriving Show
```
A `deriving Show`-t később tárgyaljuk, egyelőre annyit érdemes tudni, hogy ez kódot generál, ami `String`-re konvertálja az `IntAndBool` típus elemeit, és ezáltal GHCi-ben a típus elemeit meg tudjuk jeleníteni.

A `MkIntAndBool` egy *konstruktor*, aminek két argumentuma van, egy `Int` és egy `Bool`. GHCi-ben lekérdezhetjük a típusát:

```haskell
> :t MkIntBool
MkIntAndBool :: Int -> Bool -> IntAndBool
```
Habár ez egy függvénytípus, a konstruktorok nem végeznek semmilyen számítást az argumentumokkal, csak eltárolják őket. A GHCi egyszerűen kinyomtatja a begépelt értéket:

```haskell
> MkIntAndBool 0 False
MkIntAndBool 0 False
```
A `MkIntBool` értékeit *mintaillesztéssel* tudjuk feldolgozni:
```haskell
getInt :: IntAndBool -> Int
getInt (MkIntAndBool n _) = n
```
A fenti függvény a konstruktor első mezőjét visszadja. `_`-al jelölhetjük azokat a mezőket, amelyeket nem használunk fel az egyenlőség jobb oldalán.

Illeszthetünk mintát literálokra is:
```haskell
foo :: IntAndBool -> Int
foo (MkIntAndBool 10 _) = 20
foo _                   = 0
```
A fenti függvényben két minta van. Az illesztés szemantikája a következő: felülről lefelé haladunk, és az első illeszkedő minta esetén visszaadjuk az egyenlőség jobb oldalán található kifejezést. Tehát ebben az esetben, ha az első mező `10`, visszaadunk `20`-at, egyébként pedig `0`-t.

Alternatív illesztési szintaxis a következő:
```haskell
foo :: IntAndBool -> Int
foo x = case x of
  MkIntAndBool 10 _ -> 20
  _ -> 0
```
Definiálhatunk akárhány mezőt tartalmazó konstruktort:
```haskell
data ThreeInts = ThreeInts Int Int Int
```
Itt a `ThreeInts` azonosító kétszer szerepel. Az első a *típus* nevét deklarálja, a második pedig a *konstruktor* nevét. Mivel az egyik típus, a másik pedig egy függvény típusú érték, ezért mindig egyértelmű, hogy melyiket értjük, és a gyakorlatban sokszor definiálnak így típust. A szintaxisra annyi a kikötés, hogy mind a típusok, mind a konstruktorok nagybetűvel kezdődnek.

Mezők nélküli konstruktor szintén lehetséges:
```haskell
data Nullary = NullaryCon
```
Itt `NullaryCon` lényegében egy konstans érték `Nullary` típussal.

#### Unió-képzés

Lehetőség van egy adattípushoz több különböző konstruktort adni. Emlékezzünk, hogy lehetséges mezők nélküli konstruktort megadni. Ha több ilyen konstruktor adunk egy típushoz, akkor lényegében reprodukáljuk a más nyelvekből ismerős "enum"-okat.
```haskell
data Direction = South | North | West | East
```
Ilyen a jól ismert `Bool` is:
```haskell
data Bool = True | False
```
A függőleges vonalakkal *választást* deklarálunk több lehetséges konstruktor között. A mintaillesztás során a lehetséges konstruktorokon tudunk esetszétválasztást csinálni:
```haskell
negate :: Bool -> Bool
negate True = False
negate False = True

turnRight :: Direction -> Direction
turnRight dir = case dir of
  South -> West
  West  -> North
  North -> East
  East  -> South
```
Tudunk azonban mezőket adni bármelyik konstruktorhoz, így kombinálva a tuple- és unióképzést:
```haskell
data MyData = TwoInts Int Int | OneBool Bool
```
A mintaillesztés értelemszerűen történik:
```haskell
myfun :: MyData -> Int
myfun (TwoInts n m) = n + m
myfun (OneBool b)   = if b then 0 else 1
```
Mi történik, ha kihagyunk egy esetet?
```haskell
missingCase :: MyData -> Bool
missingCase (OneBool b) = b
```
GHCi-ben a következő történik:
```
> missingCase (TwoInts 10 10)
*** Exception: <interactive>:6:1-27: Non-exhaustive patterns in function missingCase
```
Azaz kapunk egy csúnya kivételt. Az ilyen függvényeket "parciális"-nak hívjuk, és a használatuk kerülendő. Ha egy `.hs` fájl tetejére a következőt mellékeljük
```haskell
{-# options_ghc -fwarn-incomplete-patterns #-}
```
akkor a GHC figyelmeztetni fog minden hiányos mintaillesztésre. Használhatjuk `{-# options_ghc -Wall #-}`-t továbbá, ha minden figyelmeztetést látni szeretnénk.

#### Paraméteres típusok

Elég unalmas feladat tuple-öket definiálni minden lehetséges konkrét kombinációra. Szerencsére használhatunk *paraméteres* típusokat. Ez nagyjából megfelel a Java, C# és C++ generikus adattípusainak. A pár típust például egyszerre definiáljuk az összes típusra a következőképp:
```haskell
data Pair a b = Pair a b
```
Az `a` és `b` típusparaméterek helyére bármilyen konrét típust helyettesíthetünk. Például `Pair Int Int` egy helyes típus, aminek a  konstruktora `Pair :: Int -> Int -> Pair Int Int`. Néhány példa:
```haskell
> :t Pair True False
Pair True False :: Pair Bool Bool
> :t Pair True "foo"
Pair True "foo" :: Pair Bool String
```
Kérdezzük meg `Pair` típusát:
```haskell
> :t Pair
Pair :: a -> b -> Pair a b
```
Azaz: `Pair` polimorf típusú, hiszen bármilyen `a`-ra és `b`-re működik.

Hasonlóképpen létrehozhatujk a paraméteres unió típust általánosan:
```haskell
data Either a b = Left a | Right b
```
Itt a GHCi-ben a következő érdekességet tapasztaljuk:
```haskell
> :t Left True
Left True :: Either Bool b
```
Mivel csak a `Left` konstrukor mezője derül ki a kifejezésből, a `Right` mezőjének típusa egy tetszőleges `b` típus. Az mondjuk, hogy `Left True` egy *polimorf típusú érték*, tehát nem csak polimorf függvények lehetségesek, hanem értékek is.

#### Rekurzív típusok

Ahhoz, hogy igazán érdekes és hasznos adatstruktúrákat tudjunk definiálni, szükség van rekurzív típusokra. A szintaxis egyszerű: hivatkozhatunk a konstruktorok mezőinél arra a típusra, amit éppen definiálunk. A jól ismert egyszeresen láncolt lista a következőképpen adható meg:
```haskell
data List a = Nil | Cons a (List a)
  deriving Show
```
Egy lista tartalmazhat bármilyen elemet, tehát paraméteres. Lehet egy lista üres, ez a `Nil` konstruktor, vagy pedig nemüres, ez pedig a `Cons`. Ez utóbbi tartalmaz egy `a` értéket, és a lista hátralevő részét, azaz a "farkát".

Az így definiált lista típus pontosan ugyanúgy viseledik, mint a standard Haskell lista típus. A különbség pusztán az, hogy a standard listára van szintaktikus cukor (pl. `[0, 1, 2, 3]`), míg a saját listánkra nincsen, azaz csak a deklarált konstruktorokat használhatjuk (és rájuk illeszthetünk mintát). Példa:
```haskell
list1 :: List Bool
list1 = Cons True (Cons False Nil)

list2 :: List a
list2 = Nil

list3 :: List Int
list3 = Cons 100 (Cons 200 (Cons 300 Nil))
```
Általában rekurzív típuson rekurzív függvények szükségesek. A `map'` definíciója a következő (a standard `map`-al való névütközést elkerüljük a `'`-al):
```haskell
map' :: (a -> b) -> List a -> List b
map' f Nil         = Nil
map' f (Cons a as) = Cons (f a) (map' f as)
```
Példa:
```haskell
> map' (+10) list3
Cons 110 (Cons 210 (Cons 310 Nil))
```
#### Mitől "algebrai" az ADT?

A válasz: a tuple- és unió-képzésre nagyon egyszerű algebrai képlet van, ami megadja, hogy az új típusnak hány lehetséges értéke van a komponens típusoktól függően.

A tuple-képzésnél összeszorozzuk a mezők lehetséges értékeinek a számát, és így megkapjuk a tuple lehetséges értékeinek a számát. Például: `Pair Bool Bool`-nak négy, azaz 2\*2 lehetséges értéke van, `Pair Int Int`-nek pedig 2^64 * 2^64 lehetséges értéke van 64 bites rendszeren. A formula működik mező nélküli konstruktorra is: nulla darab szám szorzata a matematikában 1, tehát például `data NoFields = NoFields` lehetséges értékeinek a száma szintén 1 (a `NoFields` konstans).

Az unió-képzésnél összeadjuk a lehetséges értékek számát, azaz `Either Bool (Either Bool Bool)` lehetséges értékeinek száma 2 + 2 + 2 = 6. Itt is működik az üres eset, megadhatjuk ugyanis a konstruktor nélküli típust Haskell-ben, aminek 0 darab értéke van. A matematikában pedig 0 darab szám összege 0.
```haskell
data Empty  -- nincs konstruktor
```
Szintén van képlet a függvényekre is: `a -> b` típus lehetséges értékeinek a száma `|b|^|a|`-val egyenlő, ahol a `^` a hatványozás, a két argumentum pedig az input és output típusok értékeinek száma. A "hatványhalmaz" kifejezés ebből a képletből származik, hiszen `a` hatványhalmazának a mérete `2^|a|`, és ebből az is látszik, hogy a hatványhalmaz reprezentálható `f :: a -> Bool` függvényekkel is, ahol `{x | x ∈ A ∧ f x = True}` a részhalmaz kódolása.

---
## Típusosztályok (bevezetés)

#### Háttér

A típusosztályok (type class) az automatikus kódgenerálás egy körülhatárolt és megszorított kivitelezése. Mit értünk automatikus kódgeneráláson? Az alap probléma a következő: a scope-ban van valahány definíció valamilyen típussal, és szeretnénk létrehozni egy értéket egy bizonyos cél típussal. Pl. a következő van scope-ban:

```haskell
-- kihagyjuk itt a definíciókat
data A a
data B
f :: B -> String
g :: (a -> String) -> A a -> String
```
A cél pedig a következő:
```haskell
h :: A B -> String
```
Egy kézenfekvő megoldás `h ab = g f ab` itt. Ezt megkaphatjuk pl. valamilyen keresőalgoritmus segítségével, vagy brute-force próbálgatással. A gyakorlati programozás jelentős része hasonló probléma: valamely adott definíciók segítségével implementáljunk adott típusú (és specifikációjú) programot.

A gyakorlatban jelenleg nem reális, hogy a programokat kiadjuk az automatikus keresésnek implementációjára, a következő két okból kifolyólag:

- Nem tudjuk vagy nem akarjuk formálisan specifikálni az implementációt. Megfelelő specifikáció nélkül az automatikus keresés haszontalan programokat fog generálni; az elégséges specifikáció azonban költséges és szakértelmet igényel.
- Szinte minden nem-triviális esetben túl nagy a keresési tér.

A típusosztályok a fenti két problémát kezelik azzal, hogy *megszorítják* a keresést. Visszatérve a példánkhoz:

```haskell
-- kihagyjuk itt a definíciókat
data A a
data B
f :: B -> String
g :: forall a. (a -> String) -> A a -> String

class Show a where
  show :: a -> String

instance Show B where
  show = f

instance Show a => Show (A a) where
  show = g show

h :: A B -> String
h = show
```

A `class Show a` deklarációval jelezzük, hogy szeretnénk a `show`-t "túlterhelni" úgy, hogy különböző 'a' típusokon különböző implementációt szeretnénk meghívni egyes `show`-alkalmazásokkor.

Az `instance Show B`-nél az implementáció felhasználja a létező `f :: B -> String` függvényt. Érdekesebb az `instance Show a => Show (A a)` eset: itt *feltételes* implementációt adunk meg, azaz definiáljuk a `show :: A a -> String` függvényt minden olyan esetben, amikor van `Show a` instance. A `h = show`-nél a GHC a típusokat követkve beszúrja a `show` helyére a megfelelő `instance`-okban megadott implementációt.

A keresés megszorítása tehát a következő:

- Előre megadjuk a `class` deklarációban, hogy milyen típusú függvények implementációját akarjuk automatikusan generálni.
- A keresés kizárólag az `instance`-okban megadott kódot látja; egyéb definíciókat nem használhat fel.
- Az egyes `instance`-ok fejei (pl. a `Show B` feje a `B` típus) nem fedhetik át egymást. Azaz, ha a `show :: t -> String` függvényt valamilyen `t`-re szeretnénk meghívni, akkor mindig pontosan egy `Show t`-re illeszkedő instance létezik.

Példa átfedő (overlapping) instance-ra:

```haskell
class Foo a where
  foo :: a -> a

instance Foo (Int, b) where
  foo = id

instance Foo (a, b) where
  foo = id
```
Itt ha pl. `foo :: (Int, Int) -> (Int, Int)` típusú alkalmazást szeretnénk, akkor mindkét instance alkalmazható lenne, és ezt a szituációt alapból a GHC nem engedélyezi, azaz mindig egyértelmű kell hogy legyen az illeszkedő instance.

A type class-ok nagy előnye, hogy a feltételes instance-ok és a paraméteres típusok segítségével végtelen sok különböző típusra meg tudunk adni kódgenerálási sémát, véges számú instance-al. Például ha általánosan definiáljuk a pár (tuple) típust, akkor ennek segítségével végtelen sok egymásba ágyazott pár típust leírhatunk, és mindegyikre megadhatjuk pl. az egyenlőség-vizsgálatot:

```haskell
class Eq a where
  (==) :: a -> a -> Bool

instance (Eq a, Eq b) => Eq (Pair a b) where
  Pair a b == Pair a' b' = a == a' && b == b'
```

Most helyes pl. a `Pair (Pair True False) (0 :: Int) == Pair (Pair False True) (0 :: Int)` kifejezés, ha létezik `Eq Int` és `Eq Bool` instance.

#### Alapvető class-ok, class constraint-ek.

A [`Prelude` dokumentációban](https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html) megtalálhatjuk az `Eq` (egyenlőség-vizsgálat), `Ord` (rendezési műveletek), `Enum` (felsorloható típusok), `Bounded` (alsó és felső korlátos típusok) leírását. A `ghci`-ben `:i` paranccsal kérhetünk információt osztályokról. Pl. `:i Show` felsorolja az osztálymetódusokat és a scope-ban levő instance-okat.

Az [`Ord` deklarációjánál](https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html#t:Ord) láthatunk egy új jelenséget, mégpedig az `Eq a =>` feltételt a `class` deklarációban:

```haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
```
Azt mondjuk erre, hogy az `Eq` az `Ord` "superclass"-ja. Ez azt eredményezi, hogy csak akkor definiálhatunk `Ord a` instance-ot, ha létezik már `Eq a` instance (bármely `a`-ra).

Ha megkérdezzük pl. a `max` típusát, a következőt kapjuk:
```haskell
> : t max
max :: Ord a => a -> a -> a
```
Az `Ord a =>` azt jelenti, hogy a függvény vár egy extra `Ord a` instance-ot argumentumként, amit mindig az automatikus keresés ad meg, azaz kézzel nem tudunk instance-ot átadni a függvénynek. A `max True False` kifejezés a GHC belső reprezentációjában a `max`-ot két extra argumentumra alkalmazzuk: a `Bool` típusra, és egy instance-ra, ami a belső reprezentációban egyszerűen egy `data`-definiált típusként jelenik meg, amelynek a mezői tartamazzák az összes class metódust.

Definiálhatunk tetszőleges függvényeket, amelyeknek instance argumentumai is vannak, pl:

```haskell
lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup a [] = Nothing
lookup a ((a', b):abs) = if a == a' then Just b else lookup a abs
```
A fenti függvény az `a == a'` kifejezésnél hivatkozik a rejtett argumentumként kapott `Eq a` instance `(==)` metódusára.
