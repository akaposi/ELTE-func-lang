# Funkcionális Nyelvek Vizsga (2024.06.05. 13:00 - 15:30)

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
data JaggedList a = JCons [a] (JaggedList a) | JNil
  deriving (Eq, Show)

infixr 5 `JCons`
```

Írjunk a fenti típusra (algoritmikusan generálható) `Functor`, `Foldable` és `Traversable` instance-ot! __(3 = 1 + 1 + 1 pont)__

`deriving` semmilyen formában nem használható.

A tesztek során használt példányok:
```hs
j1 :: JaggedList Int
j1 = [1,2,3] `JCons` [4,5,6] `JCons` JNil

j2 :: JaggedList Char
j2 = "alma" `JCons` "barack" `JCons` "kókuszdió" `JCons` JNil

j3 :: JaggedList Int
j3 = JCons [1] j3
```


Tesztek:
```hs
fmap (*2) j1 ==  [2,4,6] `JCons` [8,10,12] `JCons` JNil
fmap (const "text") j2 == ["text","text","text","text"] `JCons` ["text","text","text","text","text","text"] `JCons` ["text","text","text","text","text","text","text","text","text"] `JCons` JNil
sum j1 == 21
not $ all (>1) j3
sequenceA (fmap (\x -> if x < 6 then Just x else Nothing) j1) == Nothing
traverse (\x -> if x < 7 then Just x else Nothing) j1 == Just j1
evalState (traverse (\x -> if odd x then get >>= \n -> Just (n,x) <$ put (n + 1) else pure Nothing) j1) 0 == [Just (0,1),Nothing,Just (1,3)] `JCons` [Nothing,Just (2,5),Nothing] `JCons` JNil
```

Definiáljunk egy függvényt, ami egy `JaggedList` leghosszab oszlopának hosszát visszaadja (itt az oszlop a `JCons` konstruktorban lévő `[a]` típusú paramétert jelenti!) Feltehetjük, hogy a lista és minden oszlop véges hosszú. __(1 pont)__

```hs
longestColumn :: JaggedList a -> Int
longestColumn = undefined
```

Tesztek:

```hs
longestColumn j1 == 3
longestColumn j2 == 9
longestColumn JNil == 0
longestColumn ([1..10] `JCons` [] `JCons` JNil) == 10
```

Definiáljunk egy függvényt, ami egy paraméterül kapott függvényt alkalmaz egy `JaggedList` összes oszlopára! Feltehetjük, hogy a lista és minden oszlop véges hosszú. __(1 pont)__

```hs
mapOverColumns :: ([a] -> [b]) -> JaggedList a -> JaggedList b
mapOverColumns = undefined
```

Tesztek:

```hs
mapOverColumns (map (+1)) j1 == fmap (+1) j1
mapOverColumns (\xs -> case xs of { [] -> [1..10]; (y:ys) -> [y]; }) ([] `JCons` j1) == [1,2,3,4,5,6,7,8,9,10] `JCons` [1] `JCons` [4] `JCons` JNil
mapOverColumns (concatMap (\x -> replicate x x)) j1 == [1,2,2,3,3,3] `JCons` [4,4,4,4,5,5,5,5,5,6,6,6,6,6,6] `JCons` JNil
```

Definiáljunk egy függvényt, amely kitölti egy `JaggedList` összes oszlopát úgy, hogy a hosszuk megegyezzen a `JaggedList` leghosszabb oszlopával! A kitöltéshez használjuk a (már létező) `mempty :: Monoid a => a` függvényt. Feltehetjük, hogy a lista és minden oszlop véges hosszú. __(2 pont)__

```hs
matricize :: Monoid a => JaggedList a -> JaggedList a
matricize = undefined
```

Tesztek (ezek futtatásához a `Data.Semigroup` modul importálása szükséges):

```hs
matricize (JCons [] $ JCons ["a", "b", "c"] JNil) == JCons ["","",""] (JCons ["a","b","c"] JNil)
matricize (() <$ j2) == [(),(),(),(),(),(),(),(),()] `JCons` [(),(),(),(),(),(),(),(),()] `JCons` [(),(),(),(),(),(),(),(),()] `JCons` JNil
fmap getSum (matricize $ Sum <$> (JCons [1..5] j1)) == [1,2,3,4,5] `JCons` [1,2,3,0,0] `JCons` [4,5,6,0,0] `JCons` JNil
fmap getProduct (matricize $ Product <$> (JCons [1..5] j1)) == [1,2,3,4,5] `JCons` [1,2,3,1,1] `JCons` [4,5,6,1,1] `JCons` JNil
fmap getSum (matricize $ Sum <$> (JCons [] j1)) == [0,0,0] `JCons` [1,2,3] `JCons` [4,5,6] `JCons` JNil
```

Definiáljunk egy függvényt, ami a konzolba kirajzol egy `JaggedList` értéket. Az elemek space-el legyenek elválasztva és minden lista, ami a `JCons` konstruktorokban van külön sorba legyen kiírva (extra whitespace lehet a sor végén)! __(3 pont)__

```hs
prettyPrint :: Show a => JaggedList a -> IO ()
prettyPrint = undefined
```
A `j1` kirajzolása:
```
1 2 3
4 5 6
```
A `j2` kirajzolása:
```
'a' 'l' 'm' 'a'
'b' 'a' 'r' 'a' 'c' 'k'
'k' 'ó' 'k' 'u' 's' 'z' 'd' 'i' 'ó'
```


## Monád transzformeres feladatok (10 pont)

A feladatok során használt monád transzformer felépíthető típusosztályok használatával vagy a teljes monád stack előre definiálásával.

Modellezzünk egy farmot monádok segítségével! Azt szeretnénk ha a monád az alábbiakra lenne képes:
- Legyen egy `[String]` típusú írási környezete (ez a növesztett növények listája)
- Legyen képes `IO` műveletek elvégzésére (monád stack legalja `IO` vagy a `MonadIO` típusosztály segítségével)
- Legyen egy `[Maybe String]` típusú állapotváltozási környezete (ez a termőföldek és az azon növő növények listája)
- Legyen egy `FarmError` típusú hibakezelési környezet (ez lesz a működési hibák)

A `FarmError` típusnak egy konstruktora legyen: `FarmlandOccupied`. A típusra generáljuk `Eq` és `Show` instance-ot `deriving` segítségével!

__(3 pont)__

Az alábbi műveletet használjuk a tesztek során.
```hs
runFarm farmMonad initialState = runExceptT $ runWriterT $ runStateT farmMonad initialState
```

Definiáljuk a `growPlant` függvényt, amely egy növénynevet (`String`) és egy (tfh pozitív) számot kap paraméterül.
- Ha a paraméterül kapott szám nagyobb mint a termőföld hossza, növeljük meg a termőföldlista hosszát és az adott indexre szúrjuk be a növényt Just-ba csomagolva.
- Ha az adott indexen nincs semmi (`Nothing`) szúrjuk be a növényt Just-ba csomagolva.
- Ha az adott indexen már van valami (`Just`-ba csomagolt érték), írjuk ki az STDOUT-ra milyen növény foglalta a helyet, majd dobjunk `FarmlandOccupied` errort.
- Ha beszúrunk valamit, írjuk ki az írási környezetbe azt, hogy mit ültettünk.

A függvénynek ne legyen eredménye.
__(4 pont)__

Tesztek (konzolra írt szöveg, nem == vizsgálat):
```
runFarm (growPlant "alma" 0) [] >>= print
===
Right (((),[Just "alma"]),["alma"])

runFarm (growPlant "alma" 0) [Nothing] >>= print
===
Right (((),[Just "alma"]),["alma"])

runFarm (growPlant "alma" 0) [Just "szilva"] >>= print
===
szilva
Left FarmlandOccupied

runFarm (growPlant "szilva" 13) [Nothing, Just "alma", Just "szilva"] >>= print
===
Right (((),[Nothing,Just "alma",Just "szilva",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just "szilva"]),["szilva"])

runFarm (growPlant "szilva" 13 >> growPlant "eper" 5) [Nothing, Just "alma", Just "szilva"] >>= print
===
Right (((),[Nothing,Just "alma",Just "szilva",Nothing,Nothing,Just "eper",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just "szilva"]),["szilva","eper"])

runFarm (harvest $ growPlant "szilva" 0 >> growPlant "alma" 11) [Nothing, Just "szilva", Just "barack"] >>= print
===
Right ((3,[Nothing,Nothing,Just "barack",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]),["szilva","alma"])
```


Definiáljuk a `harvest` függvényt, amely egy farm monád típusú, eredménynélküli műveletet (`w ()`, ahol `w` a gyár monád típusa) kap paraméterül. A függvény szedje fel az állapotlistából azokat a növénytípusokat amelyeket a paraméterül kapott számítás ültetett. Adjuk vissza a felszedett növények számát.
__(3 pont)__

Tesztek (konzolra írt szöveg, nem == vizsgálat):
```
runFarm (harvest $ growPlant "szilva" 0) [] >>= print
===
Right ((1,[Nothing]),["szilva"])

runFarm (harvest $ growPlant "szilva" 0) [Nothing, Just "szilva"] >>= print
===
Right ((2,[Nothing,Nothing]),["szilva"])
```


## Parser Interpreter feladatok (10 pont)

Egészítsük ki a nyelvet halmazokkal.

### Parser (5 pont)

- Adjuk a szintaxishoz a `ForEach :: Exp -> String -> [Statement] -> Statement`, `SetLit :: [Exp] -> Exp`, `Union :: Exp -> Exp -> Exp`, `Elem :: Exp -> Exp -> Exp`, `SetComprehension :: Exp -> String -> Exp -> Exp` és `Intersect :: Exp -> Exp -> Exp` konstruktorokat. __(0.5 pont)__
- Adjuk a `foreach` és `into` kulcsszavakat a kulcsszavak listájához. __(0.5 pont)__
- A `ForEach` állítás szintaxisa kezdődjőn egy `foreach` kulcsszóval, utána folytatódjon egy kifejezéssel, majd az `into` kulcsszóval és egy változónévvel, ezt kövesse egy `do` kulcsszó, utána egy teljes program, majd egy `end` kulcsszó. __(1 pont)__
- A `SetLit` halmazliterál kapcsos zárójelek között vesszővel elválasztva tartalmazzon akármennyi kifejezést. __(0.5 pont)__
- A `Union` és `Intersect` binér műveletek mind jobbra kötő, 11-es erősségű operátorok legyenek a `∪` és `∩` szimbólumokkal. __(1 pont)__
- Az `Elem` binér művelet egy nem kötő, 9-es erősségű operátor legyen a `∈` szimbólummal. __(0.5 pont)__
- A `SetComprehension` literál kapcsos zárójelek között legyen egy darab kifejezés, amit egy `|` szimbólum követne, ami után egy változónév, egy `<-` szimbólum, majd mégegy kifejezés (hasonlóan a haskell-beli listagenerátor kifejezésekhez). __(1 pont)__

Tesztek:
```
runParser pExp "{ 1 , 2 , 3 } ∪ {}" == Right (Union (SetLit [IntLit 1,IntLit 2,IntLit 3]) (SetLit []),"")
runParser program "foreach x into y do x := x + 1; end;" == Right ([ForEach (Var "x") "y" [Assign "x" (Var "x" :+ IntLit 1)]],"")
runParser pExp "{ 1 ∪ 2 - 3, true ∩ true ∈ true }" == Right (SetLit [Union (IntLit 1) (IntLit 2 :- IntLit 3),Elem (Intersect (BoolLit True) (BoolLit True)) (BoolLit True)],"")
runParser pExp "1 ∈ 2 ∈ 3" == Left "nonAssoc: too many or too few associations"
runParser pExp "{ f $ 13 + x ∈ x | x <- f ∩ x }" == Right (SetComprehension (Var "f" :$ Elem (IntLit 13 :+ Var "x") (Var "x")) "x" (Intersect (Var "f") (Var "x")),"")
asum "task failed successfully" (map (runParser pAtom) ["foreach", "into"]) == Left "task failed successfully"
```

### Interpreter (5 pont)

- Egészítsük ki az értékek típusát egy `VSet :: [Val] -> Val` konstruktorral. __(0.5 pont)__
- Az `Elem` művelet adja vissza `VBool`-ban, hogy a baloldali operandus tartalmazza-e a jobb oldali operandust. Ha a baloldali operandus nem `VSet`-re értékelődik ki, akkor dobjunk típushibát. __(0.5 pont)__
- A `Union` és `Intersect` műveletek csak akkor vannak értelmezve, ha mindkét operandus `VSet`-re értékelődnek ki (különben típushiba). A `Union` az ismert halmazunió, az `Intersect` pedig az ismert halmazmetszer műveleteknek felelnek meg. **Segítség:** A `Data.List` modul tartalmazhat hasznos műveleteket. __(0.5 pont)__
- A `SetLit` egy `VSet`-re értékelődjön ki. A visszaadott `VSet` ne tartalmazzon duplikált elemeket és az elemek növekvő sorrendben legyenek. A `Val` típusra definiáljunk `Ord` instance-ot a `deriving` kulcsszó segítségével. Az egyenlőség művelet működjön `VSet` típusú értékekre is. __(1 pont)__
- A `SetComprehension` művelet csak akkor van definiálva, ha a harmadik paramétere `VSet`-re értékelődik ki. A halmazra végezzünk el egy 'map' (nem a map függvény!) műveletet, ahol a függvényt az első paraméterül kapott kifejezés reprezentálja. A kifejezés környezetét az éppen mappolt elemmel egészítsük ki, ahol a változónevet a második paraméter definiál! A halmaz újraépítése után ne felejtsük el újraszortírozni és a duplikált elemeket elhagyni. __(1.5 pont)__
- A `ForEach` kifejezés iteráljon végig az első paraméterül kapott kifejezés elemein (ha nem `VSet` akkor típushiba). Minden elemre értékelje ki a kapott programot, ahol a második paraméterben lévő változóba menti el a jelenlegi értéket. Az alprogram block scopeban legyen lefuttatva. __(1 pont)__

Tesztek:
```
run "x := { 1 , 1 , 0 , 1 };" == [("x",VSet [VInt 0,VInt 1])]
run "x := 1; foreach { 1 , 2 , 3 } into y do x := x + 1; end;" == [("x",VInt 4)]
run "x := 0; foreach { 1 , 2 , 3 , 10 } into y do x := x + y; end;" == [("x",VInt 16)]
run "x := { lam x -> x }; foreach x into y do x := y $ x; end; y := (lam x -> x) ∈ x;" == [("x",VSet [VLam "x" [] (Var "x")]),("y",VBool False)]
run "x := (lam x -> x) ∈ { lam x -> x };" == [("x",VBool True)]
run "x := { 1, 2, 3 }; y := x ∪ { a + 1 | a <- x };" == [("x",VSet [VInt 1,VInt 2,VInt 3]),("y",VSet [VInt 1,VInt 2,VInt 3,VInt 4])]
run "x := { 1, 2, 3 }; y := x ∩ { a + 1 | a <- x };" == [("x",VSet [VInt 1,VInt 2,VInt 3]),("y",VSet [VInt 2,VInt 3])]
run "x := {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}};" == [("x",VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet []]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]])]
run "x := {{{{{{{{{{{{{{{{{{true, {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}};" == [("x",VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VBool True,VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet [VSet []]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]])]

```
