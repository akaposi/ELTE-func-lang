

A feladatsor megoldására 2 óra áll rendelkezésre. A vizsgán tetszőleges segédeszköz használható.

Adott az alábbi típus:

```haskell
data Either' a b = Left' a | Right' b | Both a b
  deriving (Eq, Show)
```

Írjunk `Functor` példányt az `Either'` típushoz! __(1 pont)__

```haskell
instance Functor (Either' a)
```

Írjunk `Foldable` példányt az `Either'` típushoz! __(1 pont)__

```haskell
instance Foldable (Either' a)
```

Írjunk `Traversable` példányt az `Either'` típushoz! __(1 pont)__

```haskell
instance Traversable (Either' a)
```

Írjunk egy függvényt, ami az `Either'` konstruktorai szerint három listára bont egy listát.
__(1 pont)__

```haskell
partition :: [Either' a b] -> ([a], [b], [(a, b)])
```

Példa a működésre:
```haskell
partition [Left' True, Right' (0::Int), Both False 10,Both False 1] == ([True],[0],[(False,10),(False,1)])
partition [] == ([], [], [])
```

Írjunk egy függvényt, ami két listát kombinál elemenként egy függvénnyel. Az eredmény lista hossza a hosszabb input lista hossza legyen. Ha elfogyik valamelyik input lista, akkor a `Left'` vagy `Right'` konstruktorral kell feldolgozni a fennmaradó listát. __(2 pont)__

```haskell
zipWith' :: (Either' a b -> c) -> [a] -> [b] -> [c]
```

Példák:
```haskell
zipWith' (\x -> case x of Left' x -> x; Right' y -> 0; Both x y -> x + y) [1, 2, 3] [] == [1,2,3]
zipWith' (\x -> case x of Left' x -> x; Right' y -> 0; Both x y -> x + y) [1, 2, 3] [10, 20, 30] == [11,22,33]
zipWith' (\x -> case x of Left' x -> x; Right' y -> 0; Both x y -> x + y) [1] [10, 20, 30] == [11,0,0]
```

Alkalmazzunk egy `a -> Maybe b` függvényt minden `a` típusú értéken az inputban. Ha bármelyik függvényhívás eredménye `Nothing` legyen a végeredmény `Nothing`, egyébként a végeredmény `Just`-ban az output lista, ahol a függvényt minden `a`-ra alkalmaztuk. __(2 pont)__

```haskell
mapMaybeLeft :: (a -> Maybe b) -> [Either' a c] -> Maybe [Either' b c]
```

Példák:

```haskell
mapMaybeLeft (\_ -> Just True) [Left' ()] == Just [Left' True]
mapMaybeLeft (\x -> if x == 0 then Nothing else Just x) [Right' 10, Both 0 10] == Nothing
mapMaybeLeft (\x -> if x == 0 then Nothing else Just x) [Right' 10, Both 1 10] == Just [Right' 10,Both 1 10]
```

Vegyük a következő típust:

```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show)
```

Írjunk `Functor`, `Foldable` és `Traversable` instance-ot `Tree`-hez. __(2 pont)__

Irjunk egy függvényt, ami minden levélben az attól balra levő levelekben levő
`Int`-ek összegét adja vissza. __(2 pont)__

```haskell
treeSums :: Tree Int -> Tree Int
```

Példák:
```haskell
treeSums (Leaf 10) == Leaf 0
treeSums (Node (Leaf 10) (Leaf 10)) == Node (Leaf 0) (Leaf 10)
treeSums (Node (Leaf 10) (Node (Leaf 10) (Leaf 10))) == Node (Leaf 0) (Node (Leaf 10) (Leaf 20))
treeSums (Node (Node (Leaf 1) (Leaf 100)) (Leaf 0)) == Node (Node (Leaf 0) (Leaf 1)) (Leaf 101)
```

# `While` nyelv (8 pont)

A feladatunk, hogy kiegészítsük a `While` nyelvet Stringek és `Int` kifejezések log-olásával.

## Absztrakt szintaxis

Vegyünk fel egy `LogStr :: String -> Statement` és `LogInt :: Expr -> Statement`
konstruktort! __(1 pont)__

## Parser

A következő feladatunk string literálok olvasása. Néhány példa:

A string kifejezések konkrét szintaxisára néhány példa:

```haskell
"asd"
" asd   "
```

A string literálokat idézőjel határolja el, és a belsejükben *bármilyen*
nem-idézőjel karakter lehet. Az egyszerűség kedvéért nem folgalkozunk escape
karakterekkel.

Definiáljunk egy `pStringLit` nevű, string literálokat felismerő parsert (`Parser
String`) a fenti konkrét szintaxisnak megfelelően! __(2 pont)__

Ügyeljünk a whitespace karakterek helyes olvasására: a string literál belsejében
minden space karakter az olvasott `String`-ben meg kell hogy jelenjen.

Egészítsük ki az utasításokat felismerő `Parser`-t, hogy a logolást is felismerje! __(1 pont)__

A `LogStr` és a `LogInt` kulcsszavaknak számítanak.

A `LogStr` szintaxisa "LogStr" kulcsszó után egy string literál. Példa:

```haskell
if (x == 10) then LogStr "kutya" else LogStr "macska" end
```

A `LogInt` szintaxisa "LogInt" kulcsszó után egy kifejezés. Példa:

```haskell
x := 10; LogInt (10 + x); LogInt x
```

## Interpretáció

Egészítsük ki az interpretert a `LogStr` és `LogInt` kiértékelésével!

A működés a következő: minden `Log` művelet a programállapotban egy `String`-et
fűz egy `[String]` elejére.

Változtassuk meg a `type Eval =` definícióban az állapotot `(Env, [String])`
típusra. A futás során tehát két komponense lesz az állapotnak, a korrábi `Env`
és az új `[String]`. Szükség szerint módosítsuk a többi definíciót az
interpreterben, hogy azok típushelyesek legyenek. A kiértékelés kezdésekor
legyen üres lista a `[String]` értéke. __(2 pont)__

Adjuk meg a `LogStr` és `LogInt` kiértékelését az `evalSt`-ben. __(2 pont)__

A `LogStr` esetén fűzzük a `String`-et a log lista elejére. A `LogInt` esetén
értékeljük ki a paramétert `Int` típusú értékre, és annak a `String`
reprezentációját fűzzük a log elejére.  Használjuk a `show :: Int -> String`
függvényt ehhez.


# Ponthatárok

  - __2__: 10-11
  - __3__: 12-14
  - __4__: 15-16
  - __5__: 17-
