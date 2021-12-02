
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
