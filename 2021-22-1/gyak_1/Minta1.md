
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
