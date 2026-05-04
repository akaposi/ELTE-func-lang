# Funkcionális Nyelvek Mintavizsga

A feladatsor megoldására 2.5 óra áll rendelkezésre. Külső segítség és kollaboráció nem megengedett (együttműködés más vizsgázóval, külső személlyel vagy mesterséges intelligenciával : ChatGPT, BingAI stb). A megoldást akárhányszor be lehet küldeni, az utolsó megoldás számít. Részpontszám kapható minden feladatra. A leírás végén megtaláljátok a teljes Haskell fájlt, amit ki kell egészíteni és feltölteni.

Akinek meg van a 28 pontja a gyakorlati számonkérésekből, annak a +1 jegyet automatikusan beszámoljuk, ha a vizsga legalább az eléséges határt eléri.


# Ponthatárok

  - __2__: 15 - 17
  - __3__: 18 - 21
  - __4__: 22 - 25
  - __5__: 26 - 30

## Datás feladatok (10 pont)

```hs
data BiList a b = BiNil | BiCons a b (BiList a b) | ACons a (BiList a b) | BCons b (BiList a b) deriving (Eq, Show)
```

Írjunk a fenti típusra (algoritmikusan generálható) `Functor`, `Foldable` és `Traversable` instance-ot! __(3 = 1 + 1 + 1 pont)__

`deriving` semmilyen formában nem használható.

A tesztek során használt példányok:
```hs
b1 :: BiList Double Int
b1 = BiCons 1.0 1 $ ACons 2.0 $ BCons 2 BiNil

b2 :: BiList Bool Char
b2 = BCons 'h' $ BCons 'e' $ BCons 'l' $ BCons 'l' $ BCons 'o' $ BCons ' ' $ BCons 'w' $ BCons 'o' $ BCons 'r' $ BCons 'l' $ BCons 'd' BiNil

b3 :: BiList Bool Bool
b3 = BiCons False True $ b3

b4 :: BiList Int Bool
b4 = ACons 4 BiNil
```

Tesztek:
```
-- Functor tesztek
fmap (+1) b1 == (BiCons 1.0 2 $ ACons 2.0 $ BCons 3 BiNil)
undefined <$ b4 == b4
-- Foldable tesztek
sum b1 == 3
sum (fmap (+1) b1) == 5
or b3
and b4
toList b2 == "hello world"
-- Traversable tesztek
traverse (\x -> if x >= 1 then Just (x + 1) else Nothing) b1 == Just (fmap (+1) b1)
traverse (\x -> if x > 1 then Just (x + 1) else Nothing) b1 == Nothing
```

Definiáljuk a `mapL` és `foldL` függvényeket, amely a `Functor` és `Foldable` típusosztályok `fmap` és `foldMap` műveleteit végzik el a baloldali típusparaméteren. __(1 + 1 pont)__
```
mapL :: (a -> b) -> BiList a c -> BiList b c
foldL :: Monoid m => (a -> m) -> BiList a b -> m

mapL = undefined
foldL = undefined
```

Tesztek:
```
getSum (foldL Sum b1) == 3.0
getProduct (foldL Product b1) == 2.0
mapL (\_ -> undefined) b2 == b2
mapL (*2) b4 == ACons 8 BiNil
```

Definiáljuk a `separate` függvényt, amely kettéválasztja az `a` és `b` típusú elemeket a listában! __(2 pont)__
```
separate :: BiList a b -> ([a], [b])
separate = undefined
```

Tesztek:
```
separate b1 == ([1.0, 2.0], [1, 2])
separate b2 == ([], "hello world")
```

Definiáljuk a `printLR` függvényt, amely először kiírja egy `BiList` összes `a` típusú elemét, majd az összes `b` típusú elemét. __(3 pont)__

```
printLR :: (Show a, Show b) => BiList a b -> IO ()
```

A `printLR b1` standard kimenetre írt tartalma:
```
1.0 2.0
1 2
```

## Monád transzformeres feladatok (10 pont)

A feladatok során használt monád transzformer felépíthető típusosztályok használatával vagy a teljes monád stack előre definiálásával.

Modellezzünk egy scheduling rendszert monádok segítségével! Azt szeretnénk ha a monád az alábbiakra lenne képes:
- Legyen egy `[String]` típusú írási környezete (milyen taskokat végzett el a processzor)
- Legyen egy `[(String, Int)]` típusú állapotváltozási környezete (egy adott taskból mennyi idő van még hátra)
- Legyen egy `Int` típusú olvasási környezete (hány taskot tud egyszerre a processzor csinálni)

__(3 pont)__

Az alábbi műveletet használjuk a tesztek során.
```hs
runScheduler schedulerMonad initialState initialCoreCount = runWriter $ runStateT (runReaderT schedulerMonad initialCoreCount) initialState
```

Definiálj egy `tick` függvényt amely egy órajelet elvégez a processzoron. Egy órajel alatt a processzor az első `N` állapotban lévő taskon operál, ahol `N` a tasksok száma, amennyit a processzor egyszerre tud csinálni. Egy órajel alatt a processzor az alábbi dolgokat végzi el ilyen sorrendben:
- Az állapotban lévő első `N` task hátralévő idejét csökkenti egyel.
- Kiszedi az állapot listából minden olyan taskot, amely hátralévő ideje kevesebb mint 1 és ezeket kiírja az írási környezetbe.

Az egyszerűség kedvéért a tesztek során az alábbi segédfüggvényt fogjuk használni (A típus kitöltése itt is a feladat része):
```
schedule task time = modify (++ [(task, time)])
```

Tesztek:
```hs
runScheduler (schedule "a" 1 >> tick >> schedule "b" 3 >> tick >> schedule "c" 4 >> replicateM_ 5 tick) [] 2 == (((),[]),["a","b","c"])
runScheduler (schedule "a" 1 >> tick >> schedule "b" 3 >> tick >> schedule "c" 4 >> replicateM_ 2 tick) [] 2 == (((),[("c",2)]),["a","b"])
```

Definiálj egy `reschedule` függvényt, amely megkeresi az állapot listában az első olyan taskot amelynek a hátralévő ideje több mint az összes utána lévő taskok hátralévő idejének összege és ezt az állapot elejére hozza, majd kiírja az írási környezetbe, hogy:
```
Rescheduled task $TASK from $POS
```
ahol `$TASK` a task neve és `$POS` az eredeti pozíciója az állapotban. Ha a pozíciója eredetileg is a lista elején volt, akkor az üzenetet ne írjuk ki. __(4 pont)__

Tesztek:
```hs
runScheduler (schedule "a" 2 >> tick >> schedule "b" 2 >> schedule "c" 1 >> reschedule) [] 2 == (((),[("b",2),("a",1),("c",1)]),["Rescheduled task b from 1"])
runScheduler (schedule "a" 2 >> tick >> schedule "b" 2 >> schedule "c" 1 >> reschedule >> tick >> schedule "d" 4 >> schedule "e" 5 >> reschedule >> tick >> tick) [] 2 == (((),[("e",3),("d",4)]),["Rescheduled task b from 1","a","Rescheduled task e from 3","b","c"])
```


# Parser interpreter feladatok (10 pont)

A feladat a `while` nyelv kiegészítése mintaillesztéssel.

Másold be az alábbi deklarációt a kódba:
```hs
data Pattern = ByValue Val | Wildcard | Named String
  deriving (Eq, Show)
```
Add a szintaxishoz a `PatternMatch :: Exp -> [(Pattern, [Statement])] -> Statement` konstruktort!

### Parser

A szintaxis a következő:

- A mintaillesztés a `match` kulccszóval kezdődik. A kulcsszó után space-el elválasztva egy kifejezés lesz, amire majd mintaillesztünk. A kifejezés után egy `:` legyen.
- Ezután legyenek a minták és az azokhoz asszociált programok:
  - Először a mintát, utána egy jobbra nyilat (`->`), majd egy programot parse-olunk.
  - A minta egy literál kifejezés (`1`, `2`, `3`, `true`, `false`, stb.) egy alsóvonal a wildcardnak (`_`) vagy egy tetszőleges változonév lehet.
- Az állítás az `end` kulcsszóval legyen bezárva.
- A `match` kulcsszót adjuk hozzá a kulcsszavak listájához.

__(5 pont)__
Példák a működésre:
```hs
runParser statement "match 1: 1 -> x := 0; end" == Right (PatternMatch 1 [(ByValue (VInt 1),[Assign "x" 0])],"")
runParser statement "match x: 2 -> x := 0; y := 1; _ -> x := 1; end" == Right (PatternMatch (Var "x") [(ByValue (VInt 2),[Assign "x" 0,Assign "y" 1]),(Wildcard,[Assign "x" 1])],"")
isLeft (runParser statement "match := 1")
runParser statement "match x: x -> x := x; end" == Right (PatternMatch (Var "x") [(Named "x",[Assign "x" (Var "x")])],"")
runParser statement "match y + x: true -> match true: false -> x := 1; _ -> x := 2; end; 2 -> x := 3; x -> x := 4; end" == Right (PatternMatch (Var "y" + Var "x") [(ByValue (VBool True),[PatternMatch (BoolLit True) [(ByValue (VBool False),[Assign "x" 1]),(Wildcard,[Assign "x" 2])]]),(ByValue (VInt 2),[Assign "x" 3]),(Named "x",[Assign "x" 4])],"")
```

### Interpreter

Egészítsd ki az interpretert az új konstrukciók kiértékelésével!

A működés a következő:

- Értékeljük ki először az illesztendő kifejezést.
- A kiértékelés az első illeszkedő mintához tartozó programmal folytatódik.
- Illeszkedés vizsgálata:
  - Ha a minta `ByValue` és az érték típusa nem egyezik az illesztendő érték típusúval, akkor a minta nem illeszkedik, és **nem dobunk típushibát**. Ha a típusok egyeznek és az értékek is egyenlők, akkor a minta illeszkedik.
  - `Wildcard` minta minden értékre illeszkedik.
  - `Named` minta is illeszkedik minden értékre, viszont ekkor a környezetbe lokálisan felvesszük a név-érték párost.
- Ha az érték egyik mintára sem illeszkedik, akkor az állítás nem csinál semmit.

Példák a működésre:
```hs
run "x := 1; match x: 1 -> x := 2 end" == [("x",VInt 2)]
run "x := 1; match x: true -> x := 3; y -> x := y + 1 end" == [("x",VInt 2)]
run "x := 1; match x: _ -> x := 2; _ -> x := 3 end" == [("x",VInt 2)]
run "x := 1; y := 0; while not (x == 5) do match (x + 1): 3 -> y := 10000; 5 -> y := 2; x := 5; 2 -> x := 4 end end" == [("x",VInt 5),("y",VInt 2)]
```
