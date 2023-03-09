
## Kind rendszer

Korábban láttunk paraméteres típusokat, mint például a `Maybe`. Fontos, hogy a
paraméteres típusokat helyes módon használjuk. Például `x :: Maybe`
típusannotációnak nincs értelme, mivel a `Maybe` önmagában nem egy típus, és
alkalmazni kell egy paraméterre ahhoz, hogy típust kapjunk. Ghci-ben, ha `x ::
Maybe; x = undefined`-et ütünk be, a következő hibát kapjuk:

```haskell
    • Expecting one more argument to ‘Maybe’
      Expected a type, but ‘Maybe’ has kind ‘* -> *’
    • In the type signature: x :: Maybe
```

A "kind" alatt a típusszintű kifejezések típusait értjük:

- A típusok kind-ja `*`. Például `Int :: *` vagy `Bool :: *`.
- Az egy paraméteres típusok kind-ja `* -> *`, pl. `Maybe :: * -> *`.
- A két paraméteres típusok kind-ja `* -> * -> *`, pl. `Either :: * -> * -> *`.

A kind-okat lekérdezhetjük ghci-ben is, a `:k <típuskifejezés>` paranccsal:

```haskell
    > :k Bool
	Bool :: *
	> :k Maybe
	Maybe :: * -> *
	> :k Maybe Bool
	Maybe Bool :: *
```

Tehát a kind-ok általánosan a következők:

- `*` a típusok kind-ja.
- Ha `k1` és `k2` kind-ok, akkor `k1 -> k2` szintén egy kind.

A `k1 -> k2` kind-ra érdemes úgy gondolni, mint típusszintű függvénytípusra, ami
több szempontból hasnolít az értékszintű függvénytípusra. Például a parciális
applikáció szintén működik:

```haskell
    > :k Either
	Either :: * -> * -> *
	> :k Either Int
	Either Int :: * -> *
	> :k Either Int Int
	Either Int Int :: *
```

Tehát, ha `Either`-t csak egy paraméterre alkalmazzuk, egy `* -> *` kind-ú
kifejezést kapunk, amit egy további paraméterre lehet alkalmazni.

Az egy megszorítás viszont, hogy nincsenek általános függvény-definíciók a
típusszinten. A paraméteres típusok létrehozási módja mindig `data` vagy
`newtype`.

```haskell
    > data Foo a b c = Foo a b c
	> :k Foo
	Foo :: * -> * -> * -> *
```

A magasabbrendű típuskonstruktorok is működnek, tehát egy típus paramétere maga is lehet paraméteres:

```haskell
    data Pair f g a = Pair (f a) (g a)    -- Pair :: (* -> *) -> (* -> *) -> * -> *

	p1 :: Pair Maybe Maybe Int
	p1 = Pair Nothing (Just 100)

	p2 :: Pair (Either Int) Maybe Bool    -- Either Int :: * -> *
	p2 = Pair (Left 100) (Just True)

	p3 :: Pair (Either Int) Maybe Bool
	p3 = pair (Right True) (Just False)
```

A `Pair` definíciójában a GHC kikövetkezteti az `f` és `g` paraméterek kind-ját:
mivel mindkettőt alkalmazzuk `a`-ra, ezért tudjuk, hogy az `f` és `g` is `* ->
*` kell hogy legyenek, azaz egy paraméteres típusok.

Az érthetőség kedvéért a kind-okat explicit módon is lehet jelölni. Ehhez a `{-# language KindSignatures #-}`
opciót kell egy forrásfájl elején mellékelni.

```haskell
    data Pair (f :: * -> *) (g :: * -> *) (a :: *) = Pair (f a) (g a)
```

Érdemes tudni, hogy a beépített pár, lista és függvény típusok mind rendelkeznek
kind-al, és lehet őket prefix formában alkalmazni és parciálisan applikálni.

- A lista típust prefix formában egyszerűen `[]`-vel írjuk, tehát `[] :: * -> *` és
  `[] Int :: *`. A `[a]` jelölés szintaktikus cukor a `[] a`-ra.
- Az összes tuple típus elérhető prefix formában: `(,) :: * -> * -> *`, `(,,) :: * -> * -> * -> *`,
  `(,,,) :: * -> * -> * -> * -> -`, és így tovább, és mindegyiket lehet parciálisan applikálni.
  Pl. `(,) Int :: * -> *`, ami a pár típuskonstruktor alkalmazása az első mezőnek megfelelő típusra.
  Az `(a, b)` forma szintaktikus cukor az `(,) a b`-re.
- A függvény típus `(->) :: * -> * -> *` prefix formában alkalmazható. Az `a -> b` szintaktikus cukor
  a `(->) a b` formára.

## Functor

A `map` függvény jól ismert:

```haskell
    map :: (a -> b) -> [a] -> [b]
```

A standard `Functor` osztály segítségével ezt a függvényt általánosabb formában
tudjuk használni, különböző adattípusokon. Az osztály definíciója:

```haskell
    class Functor f where
      fmap :: (a -> b) -> f a -> f b
```

Vegyük észre, hogy az `f` osztályparaméter egy 1-paraméteres típus. A `{-# language KindSignatures #-}`
bekapcsolásával a következőt is írhatnánk:

```haskell
    class Functor (f :: * -> *) where
      fmap :: (a -> b) -> f a -> f b
```

A `Functor` instance-jait tehát `f :: * -> *` típuskonstruktorokra tudjuk megadni. Például a
listákra a `fmap`-et a létező `map`-el definiálhatjuk. Az instance-ban
használjuk a lista típus prefix formáját (emlékezzünk, hogy `[] :: * -> *`).

```haskell
    instance Functor [] where
	  fmap :: (a -> b) -> [a] -> [b]
	  fmap = map
	  -- Másképpen jelölve: fmap :: (a -> b) -> [] a -> [] b
	  -- tehát "f" helyettesül "[]"-re
```

A `Functor` instance megadható bármilyen `* -> *` típusra, ami a paraméter
típusú értéket valamilyen formában "tárolja". Az `fmap` egy függvényt alkalmaz
az összes ilyen tárolt értékre.

```haskell
    instance Functor Maybe where
	  fmap f Nothing = Nothing
	  fmap f (Just a) = Just (f a)

   data Tree a = Leaf a | Node (Tree a) (Tree a)

   instance Functor Tree where
     fmap f (Leaf a)   = Leaf (f a)
	 fmap f (Node l r) = Node (fmap f l) (fmap f r)
```
Ha egy típusnak több mint egy paramétere van, akkor is megadhatunk rá `Functor` instance-t:
a típust *parciálisan alkalmazzuk* úgy, hogy csak egy paraméter hiányozzon. Például, ha
az `Either` első paraméterét lerögzítjük, akkor `* -> *` típust kapunk.

```haskell
    instance Functor (Either c) where
	  fmap :: (a -> b) -> Either c a -> Either c b
	  fmap f (Left c)  = Left c
	  fmap f (Right a) = Right (f a)
```
Hasonlóképpen lerögzíthetjük a pár típus első paraméterét, és így a második mező felett tudunk map-elni:

```haskell
    instance Functor ((,) c) where
	  fmap :: (a -> b) -> (c, a) -> (c, b)
	  fmap f (c, a) = (c, f a)
```
Általánosan: egy N paraméteres típus utolsó paramétere fölött tudunk map-elni `fmap`-el.

### A függvény funktor

A függvény típus egy fontos példa a funktorokra. Ha rögzítjuk a függvény típus
bemenő típusát, akkor tudunk instance-ot megadni:

```haskell
    instance Functor ((->) c) where
	   fmap :: (a -> b) -> (c -> a) -> (c -> b)
	   fmap = (.)
```
Az `fmap` definíciója egyszerűen a függvény-kompozíció. Erre úgy érdemes gondolni,
hogy a `c -> a` típusú érték eltárol annyi darab `a` típusú értéket, ahány lehetséges `c`
érték létezik. Tehát pl. a `Bool -> a` típus ekvivalens a következő funktorral:

```haskell
  data Twice a = Twice a a
  instance Functor Twice where
    fmap f (Twice x y) = Twice (f x) (f y)
```

Ha az `fmap f`-et egy `g` függvényre alkalmazzuk, akkor annak szintén az lesz a
hatása, hogy a "tárolt" értékekre egy további `f` függvényt alkalmazunk.

### Funktor törvények

A Haskell-ben a legtöbb típusosztálynál vannak ún. *törvények*, amelyek olyan
konvenciók, amelyeknek minden instance-nak meg kéne felelni. Maga a Haskell nem
tartatja be ezeket a konvenciókat, viszont library-k gyakran feltételezik
ezeket, ezért nem jó ötlet megsérteni őket.

A `Functor` esetén két törvény van:

1. Minden `x`-re `fmap id x = x`.
2. Minden `f`, `g`, `x`-re `fmap f (fmap g x) = fmap (f . g) x`.

Az egyenlőséget (`=`) itt informálisan értjük; egyelőre elég annyit tudnunk,
hogy az egyenlő programok mindig ugyanúgy viselkednek a visszaadott értékek
szempontjából, viszont nem feltétlenül ugyanolyan hatékonyak.

Az első törvény azt jelenti, hogy az `fmap` nem módosíthatja az adott típus
*struktúráját*, kizárólag a tárolt értékekhez nyúlhat. Pl. illegális egy olyan
lista `fmap` definíció, ami megfordítja a listát.

A második törvény egy *hatékonysági* tulajdonság: ha kétszer egymás után járunk
be egy struktúrát `fmap`-el, akkor helyette mindig elég csak egyszer bejárnunk.
Ezzel megspóroljuk a köztes struktúra létrehozását.

### Milyen típusok nem funktorok?

Általánosan, ha egy paraméteres típus utolsó paramétere mindig adatmezőként vagy
függvény kimeneteként szerepel a `data` definícióban, akkor tudunk `Functor`
instance-ot megadni.

Ha a paraméter típus függvény *bemeneteként* szerepel, akkor nem létezik
helyes `Functor` instance. Például:

```haskell
    data Predicate a = Predicate (a -> Bool)

    instance Functor Predicate where
	    fmap :: (a -> b) -> Predicate a -> Predicate b
		fmap f (Predicate g) = Predicate $ \b -> _
```

Itt a kapott `f :: a -> b` függvény egyszerűen a rossz irányba képez: a lyuk
kitöltésekor nem tudjuk a `g :: a -> Bool` függvényt semmilyen módon
felhasználni, mivel nincs `a` értékünk. Ugyan meg tudunk adni valamilyen
konstans `Bool`-t adó függvényt itt, az nem fog megfelelni a funktor
törvényeknek.

### Deriving Functor

Ha bekapcsoljuk a `{-# language DeriveFunctor #-}` opciót, akkor lényegében
bármilyen olyan `data`-ra vagy `newtype`-ra automatikusan `Functor` instance-t
kaphatunk, amire létezik legális instance.

```haskell
    {-# language DeriveFunctor #-}

	data Foo a b c = C1 a | C2 b | C2 c (Foo a b c) (Foo a b c)
	  deriving Functor
```
