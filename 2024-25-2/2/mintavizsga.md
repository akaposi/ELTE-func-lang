# Funkcionális Nyelvek Vizsga (2025.01.17. 14:00 - 16:30)

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
data NTree a = Bind a [NTree a]
  deriving (Show, Eq)
```

Írjunk a fenti típusra (algoritmikusan generálható) `Functor`, `Foldable` és `Traversable` instance-ot! __(2 = 0.5 + 0.5 + 1 pont)__

`deriving` semmilyen formában nem használható.

A tesztek során használt példányok:
```hs
t1, t2, t3 :: NTree Integer
t1 = Bind 10 [Bind 3 [], Bind 5 [ Bind 1 [], Bind 2 []], Bind 9 [Bind 6 []]]
t2 = Bind 0 []
t3 = Bind 1 [Bind 2 [Bind 3 [Bind 4 [Bind 5 [Bind 6 [Bind 7 [ Bind 8 []]]]]]]]

t4 :: NTree (NTree Integer)
t4 = Bind t3 [Bind t1 [Bind t2 [], Bind t1 [], Bind t2 [Bind t1 []]]]
```

Tesztek a működésre:
```hs
sum t1 == 36
not $ and $ fmap (<5) t3
fmap (+1) t1 == Bind 11 [Bind 4 [],Bind 6 [Bind 2 [],Bind 3 []],Bind 10 [Bind 7 []]]
(3 <$ t2) == Bind 3 []traverse (\a -> if a > 9 then Just (a ^ 2) else Nothing) t3 == Nothing
traverse (\a -> if a < 9 then Just (a ^ 2) else Nothing) t3 == Just (Bind 1 [Bind 4 [Bind 9 [Bind 16 [Bind 25 [Bind 36 [Bind 49 [Bind 64 []]]]]]]])
```

Definiálj egy függvényt, amely az NTree-kból álló NTree-t Ntree-vé alakítja. __(1 pont)__

```hs
unNest :: NTree (NTree a) -> NTree a
unNest = undefined
```

Tesztek a működésre:
```hs
unNest t4 == Bind 1 [Bind 2 [Bind 3 [Bind 4 [Bind 5 [Bind 6 [Bind 7 [Bind 8 []]]]]]],Bind 10 [Bind 3 [],Bind 5 [Bind 1 [],Bind 2 []],Bind 9 [Bind 6 []],Bind 0 [],Bind 10 [Bind 3 [],Bind 5 [Bind 1 [],Bind 2 []],Bind 9 [Bind 6 []]],Bind 0 [Bind 10 [Bind 3 [],Bind 5 [Bind 1 [],Bind 2 []],Bind 9 [Bind 6 []]]]]]
unNest (Bind (Bind 1 []) []) == Bind 1 []
unNest (Bind (Bind 2 [Bind 3 []]) []) == Bind 2 [Bind 3 []]
unNest (Bind (Bind 3 []) [Bind (Bind 2 [Bind 5 []]) [] , Bind (Bind 7 []) []]) == Bind 3 [Bind 2 [Bind 5 []],Bind 7 []]
unNest (Bind (Bind 3 []) [Bind (Bind 2 []) []]) == Bind 3 [Bind 2 []]
unNest (Bind (Bind 4 [Bind 1 []]) [Bind (Bind 5 []) []]) == Bind 4 [Bind 1 [],Bind 5 []]
```

Definiálj egy függvényt, amely listává alakítja az NTree-t! __(2 pont)__

```hs
flattenT :: NTree a -> [a]
flattenT = undefined
```

Tesztek a működésre:
```hs
flattenT t1 == [10,3,5,1,2,9,6]
flattenT t2 == [0]
flattenT t3 == [1,2,3,4,5,6,7,8]
flattenT t4 == [Bind 1 [Bind 2 [Bind 3 [Bind 4 [Bind 5 [Bind 6 [Bind 7 [Bind 8 []]]]]]]],Bind 10 [Bind 3 [],Bind 5 [Bind 1 [],Bind 2 []],Bind 9 [Bind 6 []]],Bind 0 [],Bind 10 [Bind 3 [],Bind 5 [Bind 1 [],Bind 2 []],Bind 9 [Bind 6 []]],Bind 0 [],Bind 10 [Bind 3 [],Bind 5 [Bind 1 [],Bind 2 []],Bind 9 [Bind 6 []]]]
```

Definiálj egy függvényt, ami megvizsgálja, hogy egy feltétel teljesül a gyökér elemre, de semmelyik másikra nem! __(2 pont)__
```hs
notAllButHead :: (a -> Bool) -> NTree a -> Bool
notAllButHead = undefined
```

Tesztek a működésre:
```hs
not (notAllButHead (== 3) t2)
notAllButHead (<= 1) t3
```

## Monád transzformeres feladatok (10 pont)

A feladatok során használt monád transzformer felépíthető típusosztályok használatával vagy a teljes monád stack előre definiálásával.

Modellezzünk egy HTTP szervert Monád Trafók segtségével!
- WriterT [String]: Log-olási környezet
- StateT [(Int, Int, Maybe String)]: Üzenet azonosító, HTTP kód, és üzenet szövege
- ReaderT [(String, String)]: Elérhető fájlok
- IO: A legbelső monád

__(3 pont)__

Az alábbi műveletet használjuk a tesztek során.
```hs
runServer = runWriterT . flip runStateT ([] :: [(Int, Int, Maybe String)]) . flip runReaderT []
```

Írjuk meg a `send` függvényt, ami egy paraméterül kapott fájl elérési útjából megszerzi a fájlt, logolja, hogy létezik-e, és amnnyiben létezik, írjuk bele a state-be az eddigi legnagyobb azonosító +1-es azonosítóval, és a "megfelelő" HTTP kóddal.
- Megtaláltulk a fájlt: 200-as a kód, és a fájl szövege a String
- Nem találtuk meg a fájlt: 404-es hibakód, és a szöveg `Nothing`
__(3 pont)__

Tesztek:
```hs
(== (((),[(1,404,Nothing)]),["The file was not found!"])) <$> (runServer (send "test.txt"))
====
True
```

Készítsük el az "append" függvényt, ami kap paraméterül egy elérési utat, és hozzá egy fájl tartalmat. Visszatérési értéke egy lista, ami tartalmazza a "megváltoztatott" olvasási környezetet. Amennyiben az olvasási környezet tartalmazta az eddigi fájl elérési útját, ezt log-oljuk, és "írjuk felül"! Amennyiben nem tartalmazta, adjuk hozzá, és ezt is log-oljuk!
__(4 pont)__

Tesztek:
```hs
(== (([("test.txt","Én elmentem a vásárba")],[]),["Updating file tree with: test.txt"])) <$> (runServer (append ("test.txt", "Én elmentem a vásárba")))
====
True

(== (((),[(1,200,Just "Én elmentem a vásárba")]),["Updating file tree with: test.txt","Path: test.txt  was found!","Value: Én elmentem a vásárba"])) <$> (runServer (append ("test.txt", "Én elmentem a vásárba") >>= \n -> local (const n) (send "test.txt")))
====
True
```

Készítsük el a "start" függvényt, ami "elindítja" a szerverünket. A függvény olvasson be a konzolról egy sort (getLine).  
Amennyiben a sor ':'-val kezdődik, hívjuk meg a ':' karatker utáni elérési úttal az "append" függvényt, aminek a paraméterei az előbb elmített elérési út, majd utána egy szóközzel elválasztva a maradéka a beolvasott szövegnek. (takeWhile!)
Ezek után hívjuk meg rekurzívan az új függvényt az új írási környezettel.
Pl: ":test.txt Én elmentem a vásárba." -> append "test.txt" "Én elmentem a vásárba."
Amennyiben nem kezdődik ':'-tal, úgy kezeljük a beolvasott sort, mintha elérési út lenne. Erre az elérési útra hívjuk meg a "send" függvényt!
Miután végeztünk a függvény hívásokkal, írjuk ki a log-olási környezetben található String-eket a konzolra!


## Parser Interpreter feladatok (10 pont)

A feladat a `while` nyelv kiegészítése `do-with` blokkal.

### Parser

Egészítsük ki a nyelvet lokális scope-okkal.
- Ehhez vegyünk fel 2 kulcsszót: `do` és `with`
- Adjuk hozzá a szintaxishoz a `Do :: [Statement] -> Maybe [Statement] -> Statement` állatást!
- A `Do` szintaxis a `do` kulcsszóval kezdődjön, és az `end` kulcsszóval végződjön. A két kulscszó között egy programot várunk.
- Opcionálisan cserélhessük le az `end` -et egy `with`-re, amely után kifejezéseket várunk. Az a blokk is záruljon `end` kulcsszóval!

__(5 pont)__
Példák a működésre:
```hs
runParser program "x := 1; do x := 2 with x:= 3 end" == Right ([Assign "x" (IntLit 1),Do [Assign "x" (IntLit 2)] (Just [Assign "x" (IntLit 3)])],"")
runParser program "match x: 2 -> x := 0; y := 1; | _ -> x := 1; end;" == Right ([PatternMatch (Var "x") [(ByValue (VInt 2),[Assign "x" (IntLit 0),Assign "y" (IntLit 1)]),(Wildcard,[Assign "x" (IntLit 1)])]],"")
runParser program "x := 1; do x := 2; y := 4 with x:= 3; end" == Right ([Assign "x" (IntLit 1),Do [Assign "x" (IntLit 2),Assign "y" (IntLit 4)] (Just [Assign "x" (IntLit 3)])],"")
runParser program "x := 1; do x := 1 / x; y := 4 with x := 0; end" == Right ([Assign "x" (IntLit 1),Do [Assign "x" (IntLit 1 :/ Var "x"),Assign "y" (IntLit 4)] (Just [Assign "x" (IntLit 0)])],"")
runParser program "x := 1; do with while x == 1 do end end" == Right ([Assign "x" (IntLit 1),Do [] (Just [While (Var "x" :== IntLit 1) []])],"")
```

### Interpreter

Egészítsd ki az interpretert az új konstrukciók kiértékelésével! __(4 pont)__

A működés a következő:

- Amennyiben nincsen `with` kiefejezésünk, a `do` blokkban lévő programot futtassuk le!
- A `do` blokkban történő környezeti változások (új változó felvétele, stb) ne hasson ki a do blokkkon kívülre!
- Amennyiben van `with` blokkunk, annak tagjait vegyük hozzá a `do` blokk környezetéhez!
- Amennyiben a `with`-ben nem csak értékadások vannak, dobjunk egy új hibát: `InvaidExpressionInWith:: String -> InterpreterError`

Példák a működésre:
```hs
runProgram "x := 1; do x := 2; y := 4 with x:= 3; end" == Right ((),[("x",VInt 1)])
runProgram "x := 1; do x := 1 / x; y := 4 with x := 0; end" == Left (DivByZeroError "Tried to divide by zero in expression: IntLit 1 :/ Var \"x\"")
runProgram "x := 1; do with while x == 1 do end end" == Left (InvaidExpressionInWith "Illegal statement in expression: [While (Var \"x\" :== IntLit 1) []]")
```
