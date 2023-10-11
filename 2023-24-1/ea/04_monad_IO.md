## Monád intuíció

A Funktor azt engedte meg nekünk, hogy egy **tiszta** függvényt alkalmazzunk egy adatstruktúrában eltárolt elemekre, ez volt az fmap függvény:
```hs
fmap :: Functor f => (a -> b) -> f a -> f b
```

Azt viszont nem engedte meg, hogy a struktúrát a függvény segítségével megváltoztassuk (ez volt a struktúra prezerválási tulajdonsága a Funktornak). Ha azt szeretnénk, hogy a kifejezésekre alkalmazott függvény valami struktúraváltoztatást alkalmazzunk, akkor valami olyan függvény kéne, ami `b` helyett `f b`-be tér vissza:

```hs
??? :: ??? f => (a -> f b) -> f a -> f b
```

Ha egy ilyen típusú függvényt az `fmap`-nak adnánk át paraméterül, akkor egy `f (f b)` típusú kifejezést kapnánk
```hs
f :: a -> f b
x :: f a
fmap f x :: Functor f => f (f b)
```
Ha lenne egy olyan függvényünk ami ilyen `f (f b)` kifejezéseket össze tudna vonni az is egy ezzel ekvivalens megoldás lenne:
```
??? :: ??? f => f (f a) -> f a
```

Az ezeket a műveleteket összefogó típusosztály lesz a Monád:
```hs
class Functor m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
```

A második említett függvény definiálható a `>>=` (ejtsd: bind) művelet segítségével:
```hs
join :: Monad m => m (m a) -> m a
join m = m >>= \x -> x
```

A monádoknak vannak törvényeik:
```
return a >>= k == k a
m >>= return == m
m >>= (\x -> k x >>= h) == (m >>= k) >>= h
```

Egy monád instance-al sok művelet jár együtt, de ami kifejezetten érdekel minket az a `>>` függvény, ami ugyanazt csinálja mint a `>>=` csak ignorálja a paramétert:
```hs
(>>) :: Monad m => m a -> m b -> m b
m1 >> m2 = m1 >>= const m2
```

## Monád példák

Rengeteg olyan típus ami `Functor` is volt az `Monad` is lesz, például a lista esetén a műveletek így néznek ki:
```hs
instance Monad [] where
    return x = [x]
    xs >>= f = concatMap f xs
```

Instanceok írása elég egyszerű már ismert típusokra (`Maybe`, `Either`, `NonEmpty` stb). Habár az instance írás kisebb típusokra kifejezetten egyszerű, komplexebb típusokra nem ennyire triviális és habár a `Functor` instance írása algoritmikus a `Monad` instance írása nem az.

Mivel a Haskell tiszta nyelv, ezért mellékhatásos függvényeket nem tudtunk eddig írni. Viszont a magasabbrendű polimorfizmus és a monádok segítségével típusszinten jelezhetjük, hogy egy kifejezés mellékhatásokat vonz magával. Ha konkrét I/O műveletekre gondolunk ez lesz az `IO` monád (aminek a definíciója mágikus). Ha egy kifejezés `IO` monádba van csomagolva, az azt jelenti az érték eléréséhez egy nem tiszta számítást kéne elvégezni (fájlból olvasás, random szám generálása stb). Mivel ezek a műveletek nem tiszták ezért egy `IO` monádban lévő kifejezés nem juthat ki a monádon kívülre:

```hs
-- Ilyen függvény  nem lehet!
leaveIO :: IO a -> a
leaveIO = ???
```

Az `IO` monád fel van szerelve pár hasznos művelettel amivel lehet a standard inputtal és outputtal kommunikálni. Ezek az alábbiak:
```hs
putStr, putStrLn :: String -> IO ()
getLine :: IO String
print :: Show a => a -> IO ()
readLn :: Read a => IO a
```
 A `putStr` és `putStrLn` kiírnak egy kifejezést stdout-ra és a `putStrLn` új sort is ki ír. Az `IO`-ban tárolt kifejezést unitnak hívják - pontosan egy értéke van, ezért akkor szokás használni a számítás eredménye irreleváns. A `getLine` művelet beolvas egy sort a stdin-ről. A másik két művelet csak a `read` és `show` műveletek kombinációja az első három művelettel.
 
## Do-notáció

A monádműveletek sokszoros alkalmazása elég ronda és olvashatatlan kódhoz vezet. Péládul ha egy függvényt szeretnék írni, ami beolvas két számot a felhasználótol majd kiírja az összegüket, az így nézne ki:
```hs
f :: IO ()
f = putStr "Adj egy számot: " >> readLn >>= \l1 -> putStr "Adj mégegy számot: " >> readLn >>= \l1 -> putStrLn ("Az összegük: " ++ show (ln1 + ln2))
```

Ennek a problémának a megoldására fel lett találva az ún. `do-notáció`, amivel kicsit imperatívabb többsoros szintaxissal lehet monadikus műveleteket definiálni. A szintaxis mindig egy `do` kulcsszóval kezdődik, utána a monadikus műveleteket egyesével lefordítjuk sorokká.

A `x >>= \y ->` kifejezések esetén a lambda paramétert bal oldalra írjuk és egy visszafele mutató nyilat írunk az x-ből kezdve: `y <- x`
Az `x >>` kifejezések esetén leírjuk a monadikus műveletet magában.

A felső függvény átírása így nézne ki:
```hs
f :: IO ()
f = do
    putStr "Adj egy számot: "
    l1 <- readLn
    putStr "Adj mégegy számot: "
    l2 <- readLn
    putStrLn ("Az összegük" ++ show (ln1 + ln2))
```
