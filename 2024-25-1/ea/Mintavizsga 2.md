# Funkcionális Nyelvek Vizsga (2024.06.17. 14:00 - 17:30)

A feladatsor megoldására 2.5 óra áll rendelkezésre. Külső segítség és kollaboráció nem megengedett. A megoldást akárhányszor be lehet küldeni, az utolsó megoldás számít. Részpontszám kapható minden feladatra. A leírás végén megtaláljátok a teljes Haskell fájlt, amit ki kell egészíteni és feltölteni.

Akinek meg van a 28 pontja a gyakorlati számonkérésekből, annak a +1 jegyet automatikusan beszámoljuk, ha a vizsga legalább az eléséges határt eléri.


# Ponthatárok

  - __2__: 15 - 17
  - __3__: 18 - 21
  - __4__: 22 - 25
  - __5__: 26 - 30

## Datás feladatok (10 pont)

Adott az alábbi típus:
```hs
data Stream a = Append a (Stream a) | Cycle [a]
    deriving (Eq, Show)

infixr 5 `Append`
```

Írjunk a fenti típusra (algoritmikusan generálható) `Functor`, `Foldable` és `Traversable` instance-ot! __(2 = 0.5 + 0.5 + 1 pont)__

`deriving` semmilyen formában nem használható.

A tesztek során használt példányok:
```hs
s1 :: Stream Int
s1 = 1 `Append` 2 `Append` (Cycle [3])

s2 :: Stream Int
s2 = Cycle [1..10]

s3 :: Stream Bool
s3 = True `Append` (not <$> s3)
```

Tesztek a működésre:
```hs
sum s1 == 6
not $ and s3
fmap (+1) s1 == (2 `Append` 3 `Append` (Cycle [4]))
(3 <$ s2) == Cycle (replicate 10 3)
traverse (\a -> if a > 4 then Just (a ^ 2) else Nothing) s1 == Nothing
traverse (\a -> if a < 4 then Just (a ^ 2) else Nothing) s1 == Just (fmap (^2) s1)
```

Definiálj egy függvényt, amely egy streamet listává konvertál. A `Cycle` konstruktor esetén végtelenszer ismételjük a lista elemeit! (Feltehetjük, hogy a `Cycle` konstruktor paramétere nem üres) __(1 pont)__

```hs
streamToList :: Stream a -> [a]
streamToList = undefined
```

Tesztek a működésre:
```hs
take 4 (streamToList s1) == [1,2,3,3]
take 4 (streamToList s2) == [1,2,3,4]
take 20 (streamToList s2) == ([1..10] ++ [1..10])
```

Definiálj egy függvényt, amely egy streamet és egy listát össsezfűz. A listaelemek beszúrása a `Cycle` konstruktor elé kerüljön! __(2 pont)__

```hs
insertBeforeCycle :: Stream a -> [a] -> Stream a
insertBeforeCycle = undefined
```

Tesztek a működésre:
```hs
insertBeforeCycle s1 [1,2,3] == (1 `Append` 2 `Append` 1 `Append` 2 `Append ` 3 `Append` (Cycle [3]))
insertBeforeCycle s1 [] == s1
take 12 (streamToList (insertBeforeCycle s2 [0])) == ([0..10] ++ [1])
```

Definiálj egy függvényt, amely a standard bemenetről beolvas egy streamet! A függvény olvasson be két sor számot, az első a stream nem ismédlődő, a másik az ismétlődő része lesz (A `Cycle` konstruktor paramétere)! __(2 pont)__

Feltehetjük, hogy a bemenet mindig helyes és a számok space-el vannak elválasztva.

__Segítség:__ A `read` függvény segítségével tudunk stringből számot csinálni.

```hs
readStream :: IO (Stream Int)
readStream = undefined
```

Példa bemenet (s1):

```
1 2
3
```

Példa bemenet (s2):

```

1 2 3 4 5 6 7 8 9 10
```

Definiálj egy függvényt, ami megfordítja egy `Stream`-ben az elemek sorrendjét. Az eredmény `Cycle` konstruktorában legyen ugyanannyi elem mint a bemeneti paraméter `Cycle` konstruktorában! __(3 pont)__

```hs
reverseSt :: Stream a -> Stream a
reverseSt = undefined
```

Tesztek a működésre:
```hs
reverseSt s1 == (3 `Append` 2 `Append` (Cycle [1]))
reverseSt s2 == Cycle [10,9..1]
reverseSt (1 `Append` s2) == (10 `Append` Cycle ([9,8..1] ++ [1]))
```
## Monád transzformeres feladatok (10 pont)

A feladatok során használt monád transzformer felépíthető típusosztályok használatával vagy a teljes monád stack előre definiálásával.

Modellezzünk egy bankautomatát monádok segítségével! Azt szeretnénk ha a monád az alábbiakra lenne képes:
- Legyen egy `[Int]` típusú írási környezete (ez a kiadottm összegek listája)
- Legyen egy `Int` típusú olvasási környezete (ez az automatában lévő elérhető kézpénz)
- Legyen egy `ATMError` típusú hibakezelési környezet (ez lesz a működési hibák)

A `ATMError` típusnak egy konstruktora legyen: `OutOfMoney`, amelynek egy `Int` típusú paramétere van, amely a hiányos összeget jelzi. A típusra generáljuk `Eq` és `Show` instance-ot `deriving` segítségével!

__(3 pont)__

Az alábbi műveletet használjuk a tesztek során.
```hs
runATM atmMonad initialState = runExcept $ runWriterT $ runReaderT atmMonad initialState
```

Definiáljunk egy `verifyWithdraw` függvényt, amely ki próbál venni egy `Int` paraméternyi pénzt az automatából - ha van benne elég - és azt kiírja az írási környezetbe. Ha nincs elég pénz az automatában, dobjunk egy `OutOfMoney` errort és annak adjuk át a különbséget paraméterül. Ha van benne elég
akkor csak térjen vissza a függvény. A függvénynek ne legyen eredménye.
__(3 pont)__

Tesztek:
```hs
runATM (verifyWithdraw 10) 10 == Right ((),[10])
runATM (verifyWithdraw 20) 15 == Left (OutOfMoney 5)
runATM (verifyWithdraw 20 >> verifyWithdraw 10 >> catchError (verifyWithdraw 30) (const $ pure ())) 25 == Right ((),[20,10])
```

Definiáljunk egy `withdrawAll` függvényt amely egy listányi számot kap paraméterül. Sorban minden számra vizsgálja meg a `verifyWithdraw` függvény segítségével, hogy van-e elég pénz az automatában:
- Ha nincs térjen vissza a maradék listával.
- Ha van, vonja le a maradék számítás végéig és folytassa amíg a lista nem üres.
__(4 pont)__

Tesztek:
```hs
runATM (withdrawAll [1,2,3]) 10 == Right ([],[1,2,3])
runATM (withdrawAll [1,2,3]) 5 == Right ([3],[1,2])
runATM (withdrawAll [3,2,1]) 5 == Right ([1],[3,2])
```


## Parser Interpreter feladatok (10 pont)

Egészítsük ki a nyelvet listákkal. 

### Parser

A szintaxis a következő:

- Adjuk a szintaxishoz az `(:!!) :: Exp -> Exp ->  Exp`, `List :: [Exp] -> Exp`, `Length :: Exp -> Exp` és `AssignAt :: String -> Exp -> Exp -> Statement` konstruktorokat __(1 pont)__
- Az `(:!!)` művelet egy 16-os erősségű, balra kötő operátor legyen a `!!` szimbólummal, a `Length` művelet pedig egy 20-as erősségű, nem kötő, prefix operátor legyen a `length` szimbólummal. A `length` kulccszót adjuk hozzá a kulcsszavak listájához. __(1.5 pont)__
- A `List` konstruktor reprezentáljon lista literálokat (amelyeket a literálok szintjén parseoljunk). Egy listaliterál kezdődjön egy nyitó szögletes zárójellel, ami után 0 vagy több darab vesszővel elválasztott kifejezés jöjjön, majd egy bezáró kapcsoszárójel. __(1.5 pont)__
- Az `AssignAt` állítás szintaxisa egyezen meg az értékadáséval, kivéve, hogy az értékadás bal oldalán a váltózónevet kövesse egy `!!` szimbólum és egy kifejezés. __(1 pont)__

Tesztek a működésre:
```hs
runParser pExp "1 + [2,3   , 4]" == Right (IntLit 1 :+ List [IntLit 2,IntLit 3,IntLit 4],"")
runParser pExp "[] !! 3" == Right (List [] :!! IntLit 3,"")
runParser pExp "[                      [       [ ] ]  ]" == Right (List [List [List []]],"")
runParser pExp "[1 + 2, 3 + 4 !! 2 * length 1]" == Right (List [IntLit 1 :+ IntLit 2,IntLit 3 :+ (IntLit 4 :!! (IntLit 2 :* Length (IntLit 1)))],"")
runParser program "x !! 1 := 2;" == Right ([AssignAt "x" (IntLit 1) (IntLit 2)],"")
runParser program "x !! length x := length (1 + length a) !! 2;" == Right ([AssignAt "x" (Length (Var "x")) (Length (IntLit 1 :+ Length (Var "a")) :!! IntLit 2)],"")
(case (runParser pExp "length length x") of { Left _ -> True; Right _ -> False; })
```

### Interpreter

- Egészítsd ki a `Val` típust egy `VList :: [Val] -> Val` konstruktorral és az `InterpreterError` típust egy `IndexOutOfRange { message :: String }` konstruktorral. __(1 pont)__
- A `List` literál értékelje ki a benne tárolt összes kifejezést és adja őket egy `VList`-ben vissza. __(1 pont)__
- Az `Index` művelet csak akkor helyes, ha az első paraméter egy `VList`-re, a második pedig egy `VInt`-re értékelődik ki és a kapott index kisebb vagy egyenlő a lista hosszánál és nagyobb mint nulla. Ha az index kilóg, dobjuk `IndexOutOfRange` errort, egyéb esetben típushibát. Ha a paraméter értékei helyes, adjuk vissza az első paraméter n-edik elemét, ahol n a második paraméter értéke. __(1 pont)__
- Az `AssignAt` művelet ugyanabban az esetben dobjon hibát mint az indexelés. Ha viszont az első két paraméter helyes, az első paraméter n-edik elemét írjuk felül a harmadik paraméter értékével, ahol n a második paraméter értéke. __(1.5 pont)__
- A `Length` művelet adja vissza a lista hosszát. __(0.5 pont)__

Tesztek a működésre:
```hs
run "x := 1; y := [1,2,3,4,5];" == [("x",VInt 1),("y",VList [VInt 1,VInt 2,VInt 3,VInt 4,VInt 5])]
run "x := [1,2]; y := [3,x]; if (x !! 1) + 1 == (y !! 0) then x := [1,2,3]; end;" == [("x",VList [VInt 1,VInt 2,VInt 3]),("y",VList [VInt 3,VList [VInt 1,VInt 2]])]
run "x := 0; y := [1,2,3,4,5]; while not (x == length y) do y !! x := (y !! x) + x; x := x + 1; end;" == [("x",VInt 5),("y",VList [VInt 1,VInt 3,VInt 5,VInt 7,VInt 9])]
```
