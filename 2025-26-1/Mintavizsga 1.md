# Funkcionális Nyelvek Vizsga (2025.06.06. 12:00 - 14:30)

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
data Heap a = Node (Heap a) a (Heap a) | Empty
    deriving (Show, Eq)

singleton :: a -> Heap a
singleton a = Node Empty a Empty
```

Írjunk a fenti típusra (algoritmikusan generálható) `Functor`, `Foldable` és `Traversable` instance-ot! __(2 = 0.5 + 0.5 + 1 pont)__

`deriving` semmilyen formában nem használható.

A tesztek során használt példányok:
```
h1 :: Heap Int
h1 = Node Empty 1 Empty

h2 :: Heap Char
h2 = Node (Node (singleton 'h') 'e' (Node (singleton 'l') 'l' (singleton 'o'))) ' ' (Node (Node (singleton 'w') 'o' (singleton 'r')) 'l' (singleton 'd'))

h3 :: Heap Bool
h3 = Node (singleton False) True h3
```

Tesztek a működésre:
```hs
sum h1 == 1
not $ and h3
toList h2 == "hello world"
fmap (+1) h1 == Node Empty 2 Empty
(3 <$ h2) == Node (Node (Node Empty 3 Empty) 3 (Node (Node Empty 3 Empty) 3 (Node Empty 3 Empty))) 3 (Node (Node (Node Empty 3 Empty) 3 (Node Empty 3 Empty)) 3 (Node Empty 3 Empty))
traverse (\a -> if a > 1 then Just ((a + 1) ^ 2) else Nothing) h1 == Nothing
traverse (\a -> if a >= 1 then Just ((a + 1) ^ 2) else Nothing) h1 == Just (Node Empty 4 Empty)
```

A kupac struktúra invariánsa, hogy minden részfában a legkisebb elem a gyökér. Definiálj egy `verifyInvariant` függvényt, amely megvizsgálja egy kupacra, hogy teljesül-e az invariáns! Illetve definiálj egy `heapDepth` függvényt, amely kiszámolja milyen mély egy kupac! __(1 = 0.5 + 0.5 pont)__
```hs
verifyInvariant :: Ord a => Heap a -> Bool
verifyInvariant = undefined

heapDepth :: (Ord a, Num a) => Heap b -> a
heapDepth = undefined
```

Tesztek:
```
verifyInvariant h1
not (verifyInvariant h2)
heapDepth h1 == 1
heapDepth h2 == 4
```

Definiálj egy `heapInsert` függvényt, amely beszúr egy elemet a kupacba, megtartva a kupacinvariánst. Két szituációt különböztetünk meg:
- Ha a kupac gyökere nagyobb vagy egyenlő a beszúrandó elemnél, akkor egy új kupacot állítunk elő, ahol a beszúrt elem az új gyökér és a bal részkupac az eredeti kupac, a jobb oldali pedig üres.
- Ha a kupac gyökere kisebb, szúrjuk be a kisebb mélységű részkupacba az elemet.
__(2 pont)__
```hs
heapInsert :: Ord a => a -> Heap a -> Heap a
```

Tesztek:
```
heapInsert 2 h1 == Node (Node Empty 2 Empty) 1 Empty
heapInsert 3 (heapInsert 2 h1) == Node (Node Empty 2 Empty) 1 (Node Empty 3 Empty)
```

Definiálj egy `heapDeleteMin` függvényt, amely a kupac legkisebb elemét kitörli. Két szituációt különböztetünk meg:
- Ha a kupacban csak egy elem van, akkor azt szedjük ki és a maradék faként adjunk vissza üres fát.
- Ha a kupacban több elem van, szedjük ki a kupac legfelső elemét és rakjuk a második legkisebb elemet (ami valamelyik részfa csúcsa) a helyére és a második legkisebb elemet meg töröljük ki abból a részfából (rekurzívan)

A legkisebb elemet adjuk is vissza. Üres kupac esetén adjunk vissza `Nothing`-ot. __(2 pont)__

```hs
heapDeleteMin :: Ord a => Heap a -> Maybe (a, Heap a)
```

Tesztek:
```
heapDeleteMin h1 == Just (1,Empty)
heapDeleteMin (heapInsert 2 $ heapInsert 1 Empty) == Just (1,Node Empty 2 Empty)
heapDeleteMin (heapInsert 3 $ heapInsert 4 $ heapInsert 2 $ heapInsert 1 $ heapInsert 5 Empty) == Just (1,Node (Node (Node Empty 5 Empty) 4 Empty) 2 (Node Empty 3 Empty))
heapDeleteMin Empty == Nothing
```

Definiálj egy `heapsort` nevű függvényt, amely a heapsort algoritmust implementálja. Az algoritmus egy listát kap paraméterül, amelyből egyesével felépít egy kupacot, majd a kupacból egyesével kiszedi a legkisebb elemeket, amíg el nem fogy a kupac. **Max pont csak akkor jár, ha kiegyensúlyozott kupacot épít fel a háttérben!** __(3 pont)__

```hs
heapsort :: Ord a => [a] -> [a]
heapsort = undefined
```

A függvény érdemi múködési megegyezik egy rendezéssel.

## Monád transzformeres feladatok (10 pont)

A feladatok során használt monád transzformer felépíthető típusosztályok használatával vagy a teljes monád stack előre definiálásával.

Modellezzük a NAV-ot monádok segítségével! Azt szeretnénk ha a monád az alábbiakra lenne képes:
- Legyen egy `[(String, (Int, Int))]` típusú olvasási környezete (kicsoda, mennyit kellene adóznia, mennyit adózott)
- Legyen egy `[String]` típusú állapotváltozási környezete (kiknek kell adóznia)
- Legyen IO műveletekre képes

__(3 pont)__

Az alábbi műveletet használjuk a tesztek során.
```hs
runNAV navMonad initialState initialReader = runStateT (runReaderT navMonad initialReader) initialState
```

Definiálj egy `audit` nevezetű függvényt, amely megnézi az adott nevű embernek mennyi adót kell még fizetnie. Ha az adott ember egyáltalán nem fizetett még adót (azaz nincs benne az olvasási környezetben), írjuk ki a standard kimenetre, hogy "X adót csal!". Eredményül a maradék fizetendő összeget adjuk vissza. __(3 pont)__

Tesztek (nem összehasonlítás, hanem a GHCiben meghívott függvény kimenete):
```
runNAV (audit "Pisti") ["Pisti", "Gyuszi"] [("Gergő", (10, 0)), ("Pisti", (10, 9)), ("Artúr", (100, 101))]
===
(1,["Pisti","Gyuszi"])

runNAV (audit "Gyuszi") ["Pisti", "Gyuszi"] [("Gergő", (10, 0)), ("Pisti", (10, 9)), ("Artúr", (100, 101))]
===
Gyuszi adót csal!
(0,["Pisti","Gyuszi"])

runNAV (audit "Artúr") ["Pisti", "Gyuszi"] [("Gergő", (10, 0)), ("Pisti", (10, 9)), ("Artúr", (100, 101))]
===
(0,["Pisti","Gyuszi"])
```

Definiálj egy `payTaxes` függvényt, amely beolvas standard inputról egy számot és egy paraméterül kapott néven annyi adót befizet, ezután megnézi, még mennyi adót kell az adott embernek fizetnie az `audit` függvény segítségével. Ha a paraméterül kapott ember túl sok adótt fizetett, akkor írjuk ki standard kimenetre, mennyi juttatásban lesz része. Ha nem kell adóznia az embernek, csak írjuk azt ki. __(4 pont)__

Tesztek (nem összehasonlítás, hanem a GHCiben meghívott függvény kimenete):
```
runNAV (payTaxes "Pisti") ["Pisti", "Gyuszi"] [("Gergő", (10, 0)), ("Pisti", (10, 9)), ("Artúr", (100, 101))]
10
===
Pisti 9 Ft visszatérítést kap
(-9,["Pisti","Gyuszi"])

runNAV (payTaxes "Gergő") ["Pisti", "Gyuszi"] [("Gergő", (10, 0)), ("Pisti", (10, 9)), ("Artúr", (100, 101))]
10
===
Gergő-nek/nak nem kell adóznia
(0,["Pisti","Gyuszi"])

runNAV (payTaxes "Gyuszi") ["Pisti", "Gyuszi"] [("Gergő", (10, 0)), ("Pisti", (10, 9)), ("Artúr", (100, 101))]
100
===
Gyuszi 100 Ft visszatérítést kap
(-100,["Pisti","Gyuszi"])
```

## Parser Interpreter feladatok (10 pont)

A feladat a `while` nyelv kiegészítése `for` ciklussal és a `(<=)`, `(<)` operátorokkal.

### Parser

Vegyük fel a kifejezésnyelvbe a `(:<=) :: Exp -> Exp -> Exp` és a `(:<) :: Exp -> Exp -> Exp` konstruktorokat. A utasítás nyelvbe vegyük fel a `For :: Statement -> Exp -> Statement -> [Statement] -> Statement` konstruktort.

Az operátorok szintaxisa a következő:

- A `(<=)` legyen egy 8-as erősségű, nem kötő operátor. A `(<)` pedig egy 9-es erősségű, nem kötő operátor. __(2 pont)__

A `for` ciklus szintaxisas a következő:
- A `for` kulcsszó
- utánna zárójelek között, vesszővel elválasztva szerepel:
  - egy utasítást (`Statement`), ami inicializálja majd a ciklust
  - egy állítás (`Exp`) amit a ciklus feltétele
  - egy utasítást (`Statement`), ami a ciklus teste lefutása után fut majd le
- majd `do` és `end` kulcsszavak között egy program. __(3 pont)__

Példa `for` ciklusra:

 ```txt
for (x := 0 , x < 10 , x := x + 1) do y := y + x; end;
```

Példák a működésre:

```hs
runParser program "x := 0 <  1 <= 1;" == Right ([Assign "x" (IntLit 0 :< (IntLit 1 :<= IntLit 1))],"")
runParser program "x := 0 <  1;"      == Right ([Assign "x" (IntLit 0 :< IntLit 1)],"")
runParser program "x := 0 <= 1;"      == Right ([Assign "x" (IntLit 0 :<= IntLit 1)],"")
runParser program "for (x := 0 , true , x := 0) do end;" == Right ([For (Assign "x" (IntLit 0)) (BoolLit True) (Assign "x" (IntLit 0)) []],"")
runParser program "y := 0; for (x := 0 , x < 10 , x := x + 1) do y := y + x; end;" == Right ([Assign "y" (IntLit 0),For (Assign "x" (IntLit 0)) (Var "x" :< IntLit 10) (Assign "x" (Var "x" :+ IntLit 1)) [Assign "y" (Var "y" :+ Var "x")]],"")
Right [] == (fst <$> runParser program "for (x := 0; , true , x := 0;) do end;")
Right [] == (fst <$> runParser program "for := 2;")
```

### Interpreter

Egészítsd ki az interpretert `for` ciklussal és a `(<=)`, `(<)` operátorokkal!

A `(<=)` és a `(<)` operátorok működése a következő:

- Int és Float-ra elvégik a megfelelő összehasonlítást (`<=` : kisebb egyenlő mint , `<` : kisebb mint).
- Minden másra típushibát adnak vissza! __(2 pont)__

A `for` ciklus a más nyelvekből (C++, C#) megszokott `for` ciklus. Működése a következő: __(2 pont)__

- Először lefuttatja az utasítását ami inicializálja a ciklust (az első tárolt utasítás)
- Ezután kiértékeli és eldönti hogy a tárolt kifejezés (a feltétel) igaz-e :
  - ha igen akkor, lefuttatja a ciklus testés (a tárolt program), és a második tárolt utasítást, majd újra kezdődik a ciklus (hasonlóan a `While` ciklus szemantikájához)
  - ha nem akkor, nem csinál semmit

A `For` ciklusban deklarált váltózók ne látszódjanak a ciklus testén kívül, ebbe beletartoznak azok a változók amiket az inicializáló és a ciklustest után lefutó utasítás esetleg létrehoz. __(1 pont)__

Példák a működésre:
```hs
run "y := 0; for (x := 0 , x < 10 , x := x + 1) do y := y + x; end;" == [("y",VInt 45)]
run "for (x := 0 , x < 0 , x := 0) do y := 0; end;" == []
run "z := 0; for (x := 0 , x <= 10, x := x + 1) do for (y := 0, y <= 10, y := y + 1) do z := z + 10 * x + y; end; end;" == [("z",VInt 6655)]
-- This should fail
run "for (y := 0 , y <= 10, y := y + 2) do y := y - 1; end; x := y;"
```
