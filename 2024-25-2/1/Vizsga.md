# Funkcionális Nyelvek Vizsga (2024.06.17. 14:00 - 17:30)

A feladatsor megoldására 2.5 óra áll rendelkezésre. Külső segítség és kollaboráció nem megengedett (együttműködés más vizsgázóval, külső személlyel vagy mesterséges intelligenciával : ChatGPT, BingAI stb). A megoldást akárhányszor be lehet küldeni, az utolsó megoldás számít. Részpontszám kapható minden feladatra. A leírás végén megtaláljátok a teljes Haskell fájlt, amit ki kell egészíteni és feltölteni.

Akinek meg van a 28 pontja a gyakorlati számonkérésekből, annak a +1 jegyet automatikusan beszámoljuk, ha a vizsga legalább az eléséges határt eléri.


# Ponthatárok

  - __2__: 15 - 17
  - __3__: 18 - 21
  - __4__: 22 - 25
  - __5__: 26 - 30

## Datás feladatok (10 pont)

Adott az alábbi típus:
```hs
data VeryUnbalancedTree a 
    | Node (Maybe (VeryUnbalancedTree a)) a (Maybe (VeryUnbalancedTree a))
    deriving (Show, Eq)
```

Írjunk a fenti típusra (algoritmikusan generálható) `Functor`, `Foldable` és `Traversable` instance-ot! __(2 = 0.5 + 0.5 + 1 pont)__

`deriving` semmilyen formában nem használható.

A tesztek során használt példányok:
```hs
t1 :: VeryUnbalancedTree Int
t1 = Node Nothing 1 $ Just $ Node (Just $ Node Nothing 2 Nothing) 3 Nothing

t2 :: VeryUnbalancedTree Char
t2 = Node (Just $ Node (Just $ Leaf 'h') 'e' (Just $ Leaf 'l')) 'l' (Just $ Node (Just $ Leaf 'l') 'o' (Just $ Node (Just $ Node Nothing 'w' Nothing) 'o' (Just $ Node (Just $ Leaf 'r') 'l' (Just $ Leaf 'd'))))

t3 :: VeryUnbalancedTree Bool
t3 = Node Nothing False (Just $ fmap not $ t3)
```

A `t1` kirajzolva:
```hs
     1
    / \
   X   2
      / \
     3   X
```

Tesztek a működésre:
```hs
sum t1 == 6
not $ and t3
fmap (+1) t1 == Node Nothing 2 (Just (Node (Just (Node Nothing 3 Nothing)) 4 Nothing))
(3 <$ t2) == Node (Just (Node (Just (Node Nothing 3 Nothing)) 3 (Just (Node Nothing 3 Nothing)))) 3 (Just (Node (Just (Node Nothing 3 Nothing)) 3 (Just (Node (Just (Node Nothing 3 Nothing)) 3 (Just (Node (Just (Node Nothing 3 Nothing)) 3 (Just (Node Nothing 3 Nothing))))))))
traverse (\a -> if a > 4 then Just (a ^ 2) else Nothing) t1 == Nothing
traverse (\a -> if a < 4 then Just (a ^ 2) else Nothing) t1 == Just (Node Nothing 1 (Just (Node (Just (Node Nothing 4 Nothing)) 9 Nothing)))
```

Definiálj egy függvényt, amely egy bináris fából egy VUTba (Very Unbalanced Tree-be) konvertál. __(1 pont)__

```hs
data BinTree a = BNode (BinTree a) a (BinTree a) | BLeaf a deriving (Eq, Show)

toVUT :: BinTree a -> VeryUnbalancedTree a
toVUT = undefined
```

Tesztek a működésre:
```hs
toVUT (BNode (BLeaf 1) 2 (BLeaf 3)) == Node (Just (Node Nothing 1 Nothing)) 2 (Just (Node Nothing 3 Nothing))
toVUT (BLeaf "almafa") == Node Nothing "almafa" Nothing
```

Definiálj egy függvényt, amely egy VUT-ból bináris fát csinál. Ha a VUT-nak pontosan egy ága hiányzik, akkor nem lehet belőle fát csinálni! __(2 pont)__

```hs
fromVUT :: VeryUnbalancedTree a -> Maybe (BinTree a)
fromVUT = undefined
```

Tesztek a működésre:
```hs
fromVUT t1 == Nothing
fromVUT t2 == Just (BNode (BNode (BLeaf 'h') 'e' (BLeaf 'l')) 'l' (BNode (BLeaf 'o') ' ' (BNode (BLeaf 'w') 'o' (BNode (BLeaf 'r') 'l' (BLeaf 'd')))))
```

Definiálj egy függvényt, ami megviszgálja, hogy az adott mélységen a VUTban létezik e `a` típusú elem! A mélységet 0-tól kezdve vizsgáljuk! __(2 pont)__
```hs
hasNodeAtDepth :: (Num i, Eq i) => i -> VeryUnbalancedTree b -> Bool
```

Tesztek a működésre:
```hs
hasNodeAtDepth 2 t1
not $ hasNodeAtDepth 3 t1
hasNodeAtDepth 100 t3
not $ hasNodeAtDepth 5 t2
hasNodeAtDepth 1000000 t3
```

Definiálj egy függvényt, amely kiegyensúlyoz egy VUTot! Egy VUTot akkor hívunk kiegyensúlyozottnak, ha minden ágon pontosan kettő vagy pontosan 0 ága van és minden ugyanazon a szinten lévő részfa ugyanolyan mély. Az újonnan generált ágakat a `mempty :: Monoid a => a` kifejezéssel töltsük ki. __(3 pont)__

A `t1` fa kibalanceolt formában (A `mempty`-k csillaggal jelölve):
```hs
     _1_
   _/   \_
  *       3
 / \     / \
*   *   2   *
```


```hs
fullify :: Monoid a => VeryUnbalancedTree a -> VeryUnbalancedTree a
fullify = undefined
```

Tesztek a működésre:
```hs
(getSum <$> fullify (Sum <$> t1)) == Node (Just (Node (Just (Node Nothing 0 Nothing)) 0 (Just (Node Nothing 0 Nothing)))) 1 (Just (Node (Just (Node Nothing 2 Nothing)) 3 (Just (Node Nothing 0 Nothing))))
let (Node _ a _) = (getAll <$> fullify (All <$> t3)) in let (Node _ b _) = t3 in a == b
```
## Monád transzformeres feladatok (10 pont)

A feladatok során használt monád transzformer felépíthető típusosztályok használatával vagy a teljes monád stack előre definiálásával.

Modellezzünk egy kávéfőzőt monádok segítségével! Azt szeretnénk ha a monád az alábbiakra lenne képes:
- Legyen egy `[String]` típusú írási környezete (milyen kávékat adott ki a kávégép)
- Legyen egy `Int` típusú olvasási állapotváltozási környezete (hány cukor menyjen a kávéba)
- Legyen egy `TooMuchSugarError` típusú hibakezelési környezet

A `TooMuchSugarError` típusnak egy konstruktora legyen ugyanezen néven nulla paraméterrel. A típusra automatikusan deriveoljunk `Eq` és `Show` instance-ot.

__(3 pont)__

Az alábbi műveletet használjuk a tesztek során.
```hs
runCoffee coffeeMonad initialState = runExcept $ runWriterT $ runStateT coffeeMonad initialState
```

Definiáljuk az `addSugar` függvényt, amely megnöveli a cukor számát 1-el. Ha így a cukor száma nagyobb mint öt, akkor dobjunk `TooMuchSugarError`-t!
__(3 pont)__

Tesztek:
```hs
runCoffee addSugar 4 == Right (((),5),[])
runCoffee addSugar 5 == Left TooMuchSugarError
runCoffee (addSugar `catchError` (\_ -> pure ())) 5 == Right (((),5),[])
```

Definiáljuk az `orderCofee` függvényt, amely egy adott nevű kávét, adott számú cukorral csinál. A kávé nevét írjuk ki az írási környezetbe, a cukor számát növeljük meg az adott értékre.
__(4 pont)__

Tesztek:
```hs
runCoffee (orderCoffee "Pumpkin Latte" 3) 4 == Left TooMuchSugarError
runCoffee (orderCoffee "Pumpkin Latte" 3) 0 == Right (((),3),["Pumpkin Latte"])
runCoffee (orderCoffee "Pumpkin Latte" 3 >> orderCoffee "Mocha" 3) 0 == Left TooMuchSugarError
runCoffee (orderCoffee "Pumpkin Latte" 3 >> put 0 >> orderCoffee "Mocha" 3) 0 == Right (((),3),["Pumpkin Latte","Mocha"])
```


## Parser Interpreter feladatok (10 pont)

A feladat a `while` nyelv kiegészítése mintaillesztéssel.

Másold be az alábbi deklarációt a kódba:
```hs
data Pattern = ByValue Val | Wildcard | Named String
  deriving (Eq, Show)
```
Add a szintaxishoz a `PatternMatch :: Exp -> [(Pattern, [Statement])] -> Statement` konstruktort! __(1 pont)__

### Parser

A szintaxis a következő:

- A mintaillesztés a `match` kulccszóval kezdődik. A kulcsszó után space-el
  elválasztva egy kifejezés lesz, amire majd mintaillesztünk. A kifejezés után
  egy `:` legyen.
- Ezután `|`-al elválasztva legyenek a minták és az azokhoz asszociált programok:
  - Először a mintát, utána egy jobbra nyilat (`->`), majd egy programot parse-olunk.
  - A minta lehet:
    - egy Int vagy Bool literál kifejezés (`1`, `2`, `3`, `true`, `false`, stb.) 
    - egy alsóvonal a wildcardnak (`_`)
    - tetszőleges változonév
- Az állítás az `end` kulcsszóval legyen bezárva.
- A `match` kulcsszót adjuk hozzá a kulcsszavak listájához.

__(3 pont)__
Példák a működésre:
```hs
runParser program "match 1: 1 -> x := 0; end;" == Right ([PatternMatch (IntLit 1) [(ByValue (VInt 1),[Assign "x" (IntLit 0)])]],"")
runParser program "match x: 2 -> x := 0; y := 1; | _ -> x := 1; end;" == Right ([PatternMatch (Var "x") [(ByValue (VInt 2),[Assign "x" (IntLit 0),Assign "y" (IntLit 1)]),(Wildcard,[Assign "x" (IntLit 1)])]],"")
runParser statement "match := 1" == Left "statement: no statement matched"
runParser program "match x: x -> x := x; end;" == Right ([PatternMatch (Var "x") [(Named "x",[Assign "x" (Var "x")])]],"")
runParser program "match y + x: true -> match true: false -> x := 1; | _ -> x := 2; end; | 2 -> x := 3; | x -> x := 4; end;" == Right ([PatternMatch (Var "y" :+ Var "x") [(ByValue (VBool True),[PatternMatch (BoolLit True) [(ByValue (VBool False),[Assign "x" (IntLit 1)]),(Wildcard,[Assign "x" (IntLit 2)])]]),(ByValue (VInt 2),[Assign "x" (IntLit 3)]),(Named "x",[Assign "x" (IntLit 4)])]],"")
```

### Interpreter

Egészítsd ki az interpretert az új konstrukciók kiértékelésével! __(4 pont)__

A működés a következő:

- Értékeljük ki először az illesztendő kifejezést.
- A kiértékelés az első illeszkedő mintához tartozó programmal folytatódik.
- Illeszkedés vizsgálata:
  - Ha a minta `ByValue` és az érték típusa nem egyezik az illesztendő érték típusúval, akkor
    a minta nem illeszkedik, és **nem dobunk típushibát**. Ha a típusok egyeznek és az értékek is
	egyenlők, akkor a minta illeszkedik.
  - `Wildcard` minta minden értékre illeszkedik.
  - `Named` minta is illeszkedik minden értékre, viszont ekkor a környezetbe lokálisan felvesszük a név-érték párost.
- Ha az érték egyik mintára sem illeszkedik, akkor az állítás nem csinál semmit.

Példák a működésre:
```hs
run "x := 1; match x: 1 -> x := 2; end;" == [("x",VInt 2)]
run "x := 1; match x: true -> x := 3; | y -> x := y + 1; end;" == [("x",VInt 2)]
run "x := 1; match x: _ -> x := 2; | _ -> x := 3; end;" == [("x",VInt 2)]
run "x := 1; y := 0; while not (x == 5) do match (x + 1): 3 -> y := 10000; | 5 -> y := 2; x := 5; | 2 -> x := 4; end; end;" == [("x",VInt 5),("y",VInt 2)]
```
