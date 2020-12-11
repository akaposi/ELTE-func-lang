
A feladatsor megoldására 2 óra áll rendelkezésre. A vizsgán tetszőleges segédeszköz használható.

# `RoseTree` típus (11 pont)

Adott az alábbi tetszőlegesen sok irányba elágazó fákat reprezentáló adattípus:

```haskell
data RoseTree a
  = Branch a [RoseTree a]
  deriving (Eq, Ord, Show)
```

Egy lehetséges értéke:

```haskell
ex1 :: RoseTree Int
ex1 = Branch 2 $
      [ Branch 3 $
          [ Branch 11 []
          ]
      , Branch 5 $ []
      , Branch 7 $
          [ Branch 13 []
          ]
      ]
```

__Megjegyzés__: Másoljátok be ezt a példát is a kódotokba!

## `Functor` és `Foldable`

Írjunk `Functor` példányt a `RoseTree` típushoz! __(1 pont)__

```haskell
instance Functor ...
```

Írjunk `Foldable` példányt a `RoseTree` típushoz! __(1 pont)__

```haskell
instance Foldable ...
```

Definiáljuk azt a függvényt, amely megszámolja hogy hány elem van egy `RoseTree`-ben! __(1 pont)__

```haskell
countElems :: RoseTree a -> Int
```

Definiáljuk azt a függvényt, amely megkeresi a maximális elemet egy `RoseTree`-ben! __(1 pont)__

```haskell
maxElem :: Ord a => RoseTree a -> a
```

## `Traversable`

Írjunk `Traversable` példányt a `RoseTree` típushoz! __(2 pont)__

```haskell
instance Traversable ...
```

Definiáljuk azt a függvényt, amely megszámozza egy `RoseTree` elemeit! A bejárás sorrendje legyen preorder, azaz először az elemet látogassuk meg, majd balról jobbra a részfákat. __(2 pont)__

```haskell
numberElems :: RoseTree a -> RoseTree (a, Int)
```

__Segítség__: Használjuk `State` monádot és a `forM` vagy `mapM` függvényt!

Definiáljuk azt a függvényt, amely biztonságosan indexel egy listába! __(1 pont)__

```haskell
safeIndex :: [a] -> Int -> Maybe a
```

Definiáljuk azt a függvényt, amely egy `RoseTree`-ben lévő indexet lecserél egy adott lista annyiadik elemére! Az indexelés nem feltétlenül helyes, ezért `Maybe`-vel térjünk vissza! Ha akár egyszer is invalid lenne az index, akkor `Nothing`-gal térjünk vissza! __(2 pont)__

```haskell
transformWithList :: [a] -> RoseTree Int -> Maybe (RoseTree a)
```

__Segítség__: Használjuk az előzőleg definiált `safeIndex` függvényt!


# `While` nyelv (9 pont)

A feladatunk, hogy kiegészítsük a `while` nyelvet a `string` típussal. Ehhez módosítanunk kell a szintaxist, a parsert és az interpretert is. A nyelvet összességében string literálokkal és a rajtuk értelmezett konkatenáció művelettel fogjuk kiegészíteni.

A feladatokat tetszőlegesen oldhatjátok meg, azonban a következő függvények legyenek scope-ban:

```haskell
evalParser :: Parser a -> Maybe a
expr :: Parser Expr
evalExpr :: Expr -> Eval RTVal
evalEval :: Eval a -> Map Var RTVal -> Either String RTVal
```

## Absztrakt szintaxis

Első feladatunk, hogy a nyelv absztrakt szintaxisát kiegészítsük string literálokkal és a konkatenációval.

Vegyünk fel egy `StrLit :: String -> Exp` nevű és típusú adatkonstruktort az eddigi literálok mellé! __(1 pont)__

Vegyünk fel egy `Append :: Exp -> Exp -> Exp` nevű és típusú adatkonstruktort
az eddigi kifejezések mellé! Ez fogja reprezentálni a `++` műveletet. __(1 pont)__

## Parser

A következő feladatunk, hogy konkrét szintaxist is társítunk az újonnan megadott nyelvi elemekhez.

A string kifejezések konkrét szintaxisára néhány példa:

```haskell
"asd"
" asd   "
"asd"++"qwe"
"asd" ++ "qwe"
```

Az egyszerűség kedvéért a string-ek __kizárólag__ alfanumerimus karaktereket és
a szóköz karaktert tartalmazhatják (tehát nincs multiline string, nincs tab,
nincs escape-elés, stb). A string-eket mindig egy idézőjel kezdi és zárja le. A
konkatenációt a "++" operátorral fogjuk jelölni, szintaktikusan a Haskelles
"++"-hoz hasonlóan viselkedik.

Definiáljunk egy `pStringLit` nevű, string literálokat felismerő parsert
(`Parser String`) a fenti konkrét szintaxisnak megfelelően! Ügyeljünk arra, hogy
az idézőjelek közötti szóközöket helyesen ismerje fel a parser! __(2 pont)__

Az `++` operátor kössön erősebben, mint a `<`, és gyengébben, mint a `+`, és
asszociáljon jobbra! Tehát pl. a `"foo" ++ "bar" ++ "baz"` zárójelezése legyen
`"foo" ++ ("bar" ++ "baz")`.

Egészítsük ki a kifejezéseket felismerő `Parser`-t (`pExp`), hogy a string
kifejezéseket (literálok, összefűzés) is felismerje! __(2 pont)__

## Interpretáció
Definiáljuk újra a `Val` típust úgy, hogy `String` literálokat is tartalmazhasson!
__(1 pont)__

Egészítsük ki a kifejezéseket értelmező függvényt (`evalExp`), hogy string
literálokat és a hozzájuk tartozó konkatenációt is tudja értelmezni! __(2
pont)__


# Ponthatárok

  - __2__: 10-11
  - __3__: 12-14
  - __4__: 15-16
  - __5__: 17-
