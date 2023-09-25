## Hajtogatás

Hasonlóan, mint ahogyan A `Functor` általánosítja a `map` műveletet, a `Foldable` típuosztály általánosítja a `foldr` műveletet.
```hs
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```
A függvény az első paraméterül kapott kétparaméteres függvényt **jobbról** balra alkalmazza:
```hs
foldr f b [x1, x2, x3, ... , xn] == f x1 (f x2 (f x3 (... (f xn b))))
```

Ez a művelet tetszőleges adatszerkezetre alkalmazható ahol a hajtogatandó típusú paraméterek nem függvényben szerepelnek - ezért alapult a típuosztály. Példa implementáció egy fára:
```hs
data Tree a = Leaf a | Node (Tree a) a (Tree a)

foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree f b (Leaf a) = f a b
foldrTree f b (Node tr1 a tr2) = foldrTree f (f a (foldrTree f b tr2)) tr1
```

`foldr` íráskor a paraméterek ugyanolyan sorrendben szerepelnek zárójelezési szempontból, mint amilyen sorrendbe a paraméterek megjelennek a konstruktorban.


```hs
--                  |      |
--                  |      |
--                  |      |
--                  V      V
foldrTree f b (Leaf a) = f a b

--                  1   2 3                   ( 2 (              3 ))  1
--                  |   | |                     |                |     |
--                  |   | |                     |                |     |
--                  |   | |                     |                |     |
--                  V   V V                     V                V     V
foldrTree f b (Node tr1 a tr2) = foldrTree f (f a (foldrTree f b tr2)) tr1
```

## A Foldable instance automatikus generálása

A `Foldable` instance írás a fenti szabály miatt algoritmizálható, ezért a GHC-be van beépítve automatikus ezköz, amivel lehet `deriving` segítségével `Foldable` instanceokat írni. Ehhez régebbi haskell verziókon a `DeriveFoldable` nyelvi kiegészítőt be kell kapcsolni, hasonlóan mint a `Functor`-nál.
```hs
{-# LANGUAGE DeriveFoldable #-}

data List a = Nil | Cons a (List a)
    deriving (Eq, Show, Foldable)
```

Az automatikus instance írás csak azokban az esetekben működik, amikor az 'a' típusparaméter nem szerepel függvényben vagy nem hajtogatható típusban. Ezekre a típusokra manuálisan se lehetne `Foldable` instance-ot írni.

## A Foldable instance-al járó műveletek

Rengeteg művelet jár egy `Foldable` instance-al ajándékba, ezek a `:i Foldable` parancsal megtekinthetőek:
```hs
class Foldable t where
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap' :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
  {-# MINIMAL foldMap | foldr #-}
    -- Defined in ‘Data.Foldable’
```

A fenti műveletek mind olyasféle műveletek amelyek csak végigiterálnak a lista elemein, tehát mind implementálhatóak a `foldr` segítségével. Például a length függvény nem egy "lista" hosszát adja vissza, hanem végighajtogat az összes elemen, és minden elemnél az akkumulátor paramétert növeli 1-el, ezért lesz a függvény (és a hajtogatási műveletek nagyrésze) `O(n)` azaz lineáris műveletigényű.
```hs
length' :: Foldable t => t a -> Int
length' = foldr (const (+1)) 0
```

A műveleteket azért lehet mégis felülírni mert sok más tároló a fenti műveletekre van optimalizálva (Finger Tree, Min/Max Heap), így az automatikus lineáris műveleteket le lehet cserélni konstans vagy logaritmikus műveletekre.

## A foldr alternatívája

A `Foldable` MINIMAL pragmája meg engedi a `foldMap` függvény megírását a `foldr` helyett, amelnyek a szignatúrája:
```hs
foldMap :: Monoid m => (a -> m) -> t a -> m
```

A függvény a tároló minden elemére alkalmazza az első paraméterül kapott függvényt, és utána a félcsoport `<>` nevű asszociatív műveletével kombinálja őket:
```hs
foldMap f [x1, x2, x3, ..., xn] == f x1 <> f x2 <> f x3 <> ... <> f xn <> mempty
```

Példa képpen ha a (ℝ, +) páros által képzett félcsoport esetén megírható a `sum` függvény:
```hs
data Sum a = Sum { runSum :: a }

instance Num a => Semigroup (Sum a)
 (Sum a) <> (Sum b) = Sum (a + b)
 
instance Num a => Monoid (Sum a)
 mempty = Sum 0

sum :: (Foldable t, Num a) => t a -> a
sum = runSum . foldMap Sum
```

Mivel a `foldMap` alternatívája a `foldr` függvénynek ezért a kettőt meg kell tudni írni a másik a segítségével. A `foldMap`-ot megírni a `foldr` segítségével kifejezetten egyszerű:
```hs
foldMap f = foldr (\a b -> f a <> b) mempty
```

Ahhoz, hogy a `foldr`-t megírjuk, a (A -> A, .) félcsoportot kell használni (azaz az endomorfizmusok és a függvénykompozíció által képzett monoidot):
```hs
data Endo a = Endo { runEndo :: a -> a }

instance Semigroup (Endo a) where
 (Endo f) <> (Endo g) = Endo (f . g)

instance Monoid (Endo a)
 mempty = Endo id
 
foldr f b t = runEndo (foldMap (Endo . f) t) b
```

