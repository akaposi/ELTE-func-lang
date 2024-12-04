# Mintavizsga

A feladatsor megoldására 2.5 óra áll rendelkezésre. Külső segítség és kollaboráció nem megengedett. A megoldást akárhányszor be lehet küldeni, az utolsó megoldás számít. Részpontszám kapható minden feladatra. A leírás végén megtaláljátok a teljes Haskell fájlt, amit ki kell egészíteni és feltölteni.


# Ponthatárok

  - __2__: 15 - 17
  - __3__: 18 - 21
  - __4__: 22 - 25
  - __5__: 26 - 30

## Datás feladatok (10 pont)

```hs
data Infinitree f a = Leaf a | Node (f (Infinitree a))
-- ignore me, mágia
deriving instance (Eq a, forall a. (Eq a) => Eq (f a)) => Eq (Infinitree f a)
deriving instance (Show a, forall a. (Show a) => Show (f a)) => Show (Infinitree f a)
-- mágia over
```

Írjunk a fenti típusra (algoritmikusan generálható) `Functor`, `Foldable` és `Traversable` instance-ot! __(5 = 1 + 2 + 2 pont)__

`deriving` semmilyen formában nem használható.

A tesztek során használt példányok:
```hs
inf1 :: Infinitree [] Int
inf1 = Node [Leaf 1, Node [Leaf 2, Leaf 3], Leaf 4, Node [Leaf 5, Leaf 6, Leaf 7]]

inf2 :: Infinitree NonEmpty Char
inf2 = Node (Leaf 'h' :| [Leaf 'e', Leaf 'l', Leaf 'l', Node (Leaf 'o' :| []), Leaf ' ', Leaf 'w', Node (Leaf 'o' :| [Leaf 'r', Leaf 'l', Leaf 'd'])])

inf3 :: Infinitree Maybe Bool
inf3 = Node Nothing

inf4 :: Infinitree [] Bool
inf4 = Node [Leaf True, inf4]
```

Tesztek:
```
-- Functor tesztek
fmap (+1) inf1 == Node [Leaf 2, Node [Leaf 3, Leaf 4], Leaf 5, Node [Leaf 6, Leaf 7, Leaf 8]]
fmap not inf3 == inf3
fmap (\x -> replicate x x) inf1 == Node [Leaf [1], Node [Leaf [2,2], Leaf [3,3,3]], Leaf [4,4,4,4], Node [Leaf [5,5,5,5,5], Leaf [6,6,6,6,6,6], Leaf [7,7,7,7,7,7,7]]]
('a' <$ inf1) == Node [Leaf 'a', Node [Leaf 'a', Leaf 'a'], Leaf 'a', Node [Leaf 'a', Leaf 'a', Leaf 'a']]
-- Foldable tesztek
sum inf1 == 28
toList inf2 == "hello world"
and inf3
not (or inf3)
or inf4
-- Traversable tesztek
traverse (\x -> if x >= 1 then Just (x + 1) else Nothing) inf1 == Just (fmap (+1) inf1)
traverse (\x -> if x > 1 then Just (x + 1) else Nothing) inf1 == Nothing
```

Definiáljuk a `semiFlattener` függvényt, amely kilapított formában visszaad egy fát. Ha bejárás közben egy ágon fel vagy le mennénk, szúrjunk be egy `Left`-et előtte ami azt tárolja melyik irányba mentünk egy ágban! __(2 pont)__
```hs
data Dir = Down | Up deriving (Eq, Show, Ord, Enum)

semiFlattener :: Infinitree [] a -> [Either Dir a]
semiFlattener = undefined
```

Tesztek:
```hs
semiFlattener inf1 == [Left Down, Right 1, Left Down, Right 2, Right 3, Left Up, Right 4, Left Down, Right 5, Right 6, Right 7, Left Up, Left Up]
semiFlattener (Leaf 1) == [Right 1]
semiFlattener (Node []) == [Left Down, Left Up]
take 4 (semiFlattener inf4) == [Left Down, Right True, Left Down, Right True]
```

Definiáljuk a `semiUnflattener` függvényt, amely egy kilapított fából visszaállítja az eredetit (Feltehetjük, hogy hibásan épített fa nincs, tehát minden `Left Down`-hoz egy `Left Up` is tartozik utána)! __(3 pont)__
```hs
semiUnflattener :: [Either Dir a] -> Infinitree [] a
```

Tesztek:
```hs
semiUnflattener (semiFlattener inf1) == inf1
semiUnflattener [Left Down, Left Up] == Node []
semiUnflattener [Right 1] == Leaf 1
```

## Monád transzformeres feladatok (10 pont)

A feladatok során használt monád transzformer felépíthető típusosztályok használatával vagy a teljes monád stack előre definiálásával.

Modellezzünk egy ATM-et monádok segítségével! Azt szeretnénk ha a monád az alábbiakra lenne képes:
- Legyen egy `[String]` típusú írási környezete (ezek lesznek az elérési logok)
- Legyen egy `Maybe String` típusú olvasási környezete (ez lesz a jelenlegi felhasználó kártyaszáma, ha van éppen felhasználó)
- Legyen egy `[(String, Int)]` típusú állapotváltozási környezete (ez lesz a bankkártyákhoz asszociált pénznem)
- Legyen egy `ATMError` típusú hibakezelési környezet (ez lesz a működési hibák)

Az `ATMError` típusnak két konstruktora legyen: `NotEnoughFunds` és `NoUser`. A `NotEnoughFunds` konstruktornak legyen egy `String` paramétere, ami a kártyaszámot jelzi. A típusra generáljuk `Eq` és `Show` instance-ot `deriving` segítségével!

__(3 pont)__

Az alábbi műveletet használjuk a tesztek során
```hs
runAtm atmMonad initialState initialUser = runExcept $ runWriterT $ runReaderT (runStateT atmMonad initialState) initialUser
```

Definiáljuk a `loginAs` függvényt, amely egy `String` bankkáryát kap paraméterül és egy atm monád transzformert. A függvény futassa le a paraméterül kapott transzformerta abban az olvasási környezetben, amelyben a jelen bankkártya a paraméterül kapott `String`. A belépést logoljuk az írási környezetbe.
__(2 pont)__

Tesztek:
```
runAtm (loginAs "0000" ask) [] Nothing == Right (("0000", []), ["0000 belépett"])
runAtm (loginAs "1100" (ask >>= \n -> read n <$ put [(n, 9999)])) [] Nothing == Right ((1100, [("1100", 9999)]), ["1100 belépett"])
runAtm (loginAs "1110" (throwError NoUser)) [] Nothing == Left NoUser
```

Definiáljuk a `withdrawOrDeposit` függvényt, amely egy `Int`-et vár paraméterül. A függvény az alábbi lépéseket végezze el:
- Ha az olvasási környezet szerint nincs senki belépve dobjunk `NoUser` errort.
- A paraméterül kapott számot adjuk hozzá az állapotban elárolt listában a jelen felhasználó listaelemhéz. Ha az így kapott szám kisebb mint 0, dobjuk `NotEnoughFunds` errort a jelenlegi felhasználó bankkártyájával.
- Sikeres tranzakció esetén ezt írjuk be az írási környezetbe.
- Az eredmény az újonnan kiszámított összeg legyen.
__(5 pont)__

Tesztek:
```
runAtm (withdrawOrDeposit 100) [] Nothing == Left NoUser
runAtm (loginAs "0000" $ withdrawOrDeposit 1000) [] Nothing == Right ((1000, [("0000", 1000)]), ["0000 belépett", "0000 feltöltött 1000-et"])
runAtm (loginAs "0000" $ withdrawOrDeposit (-1000)) [] Nothing == Left (NotEnoughFunds "0000")
runAtm (loginAs "1000" $ withdrawOrDeposit 1000 >> withdrawOrDeposit (-1000)) [("0101", 1234)] Nothing == Right ((0, [("0101", 1234), ("1000", 0)]), ["1000 belépett", "1000 feltöltött 1000-et", "1000 feltöltött -1000-et"])
```

## Parser Interpreter feladatok (10 pont)

# `while` nyelv kiegészítése (10 pont)

A feladat a `while` nyelv kiegészítése zárakkal.

Adjuk a szintaxishoz a `IsLocked :: Exp -> Exp` és `Fallback :: Exp -> Exp -> Exp` kifejezéseket, illetve a `ReadWriteLock :: String -> Statement`, `WriteLock :: String -> Statement` és `Unlock :: String -> Statement` állításokat! __(1 pont)__

### Parser

A szintaxis a következő:

- Az `IsLocked` egy `locked` nevű prefix operátor 9-es kötési erősséggel. A `locked` kulcsszót adjuk hozzá a kulcsszavak listájához.
- A `Fallback` egy `?` nevű balra kötő operátor 5-ös kötési erősséggel.
- A `ReadWriteLock` állítás kezdődjön egy `rwlock` kulcsszóval, majd egy változónévvel. Az `rwlock` kulcsszót adjuk hozzá a kulcsszavak listájához.
- A `WriteLock` és `Unlock` szintaxisa megegyezik a `ReadWriteLock`-éval, csak `wlock` és `unlock` kulcsszavakat használva (ezeket is adjuk hozzá a kulcsszavak listájához).

__(5 = 1 + 1 + 1 + 1 + 1 pont)__

Tesztek a működésre:
```hs
runParser pExp "locked a" == Just ("",IsLocked (Var "a"))
runParser pExp "not locked a" == Just ("",Not (IsLocked (Var "a")))
runParser pExp "locked not a" == Nothing
runParser pExp "locked (1 + x)" == Just ("",IsLocked (Add (IntLit 1) (Var "x")))
runParser pExp "a ? 1" == Just ("",Fallback (Var "a") (IntLit 1))
runParser pExp "a ? 1 ? b" == Just ("",Fallback (Fallback (Var "a") (IntLit 1)) (Var "b"))
runParser pExp "locked 1 + a ? b * locked b ? b" == Just ("",Fallback (Fallback (Add (IsLocked (IntLit 1)) (Var "a")) (Mul (Var "b") (IsLocked (Var "b")))) (Var "b"))
runParser pExp "locked 1 + a ? b - locked b ? b" == Just ("",Sub (Fallback (Add (IsLocked (IntLit 1)) (Var "a")) (Var "b")) (Fallback (IsLocked (Var "b")) (Var "b")))
runParser program "wlock x; rwlock x; unlock x" == Just ("",[WriteLock "x",ReadWriteLock "x",Unlock "x"])
runParser statement "wlock 1" == Nothing
asum (runParser statement <$> ["locked := 1", "unlock := 1", "rwlock := 1", "wlock := 1"]) == Nothing
```

### Interpreter

Egészítsük ki a nyelvet egy `LockType` típussal aminek három nullaparaméteres konstruktora van, ezek a `Write`, `ReadWrite` és `None` (a konstruktorok lehetnek tetszőleges sorrendben és lehet tetszőleges típusosztályokat implementálni rá). `deriving` segítségével implementáljuk `Eq` instance-ot erre a típusra!

Változtassuk meg az `Env` típusszinonímát arra, hogy `[(String, (LockType, Val))]`. A fordítási hibákat az `updateEnv` és `evalExp` függvényekben javítsuk ki (per pillanat a `LockType` paramétert elég ignorálni).
__(2 pont)__

A kifejezések kiértékelése az alábbi módon változik:

- Az `IsLocked` kifejezés dobjon kivételt, ha nem `Var` típusú a paramétere. Ha igen, adja vissza, hogy a környezetben `ReadWrite` vagy `Write` értékű-e a `Var`-ban lévő változóhoz asszociált `LockType`.
- A `Fallback` kifejezés szintén dobjon kivételt, ha a bal oldali paramétere nem egy `Var` kifejezés. Ha a bal oldali paraméterében lévő változón `ReadWrite` típusú lock van, akkor adja vissza a jobb oldali paramétert kiértékelve, egyébként pedig a változó `Val` értékét.
- A `Var` kifejezés kiértékelésekor, ha `ReadWrite` lock van a változón, dobjunk kivételt.

Az állítások kiértékelése az alábbi módon változik:

- Az `Unlock`, `ReadWriteLock` és `WriteLock` állítások csak a változó zárának típusát változtassák (`None`, `ReadWrite` és `Write`).
- Ha `Assign`-al felül szeretnénk egy változó értékét írni és a változón lévő zár `ReadWrite` vagy `Write`, dobjunk kivételt.
- Egy változó alap lockja legyen `None`.

__(5 = 2 + 2 (kifejezések) + 1 (állítások) pont)__

__Figyelem:__ a `throws`-os tesztesetek azt vizsgálnák, hogy az adott futás hibát dob-e, de ez egy elég instabil technológia. Elég azt megnézni ezeknél, hogy a `throws`-ban lévő kifejezés kivételt dob-e.

Tesztek a működésre:
```hs
run "x := 1" == [("x",(None,VInt 1))]
run "x := 1; wlock x" == [("x",(Write,VInt 1))]
throws $ run "x := 1; wlock x; x := 2"
run "x := 1; wlock x; y := x" == [("x",(Write,VInt 1)),("y",(None,VInt 1))]
throws (snd $ snd $ run "x := 1; rwlock x; y := x" !! 1)
run "x := 1; rwlock x; y := locked x" == [("x",(ReadWrite,VInt 1)),("y",(None,VBool True))]
run "x := 1; rwlock x; unlock x" == [("x",(None,VInt 1))]
run "x := true; rwlock x; while (locked x) do unlock x end" == [("x",(None,VBool True))]
run "x := 1; wlock x; y := x ? 2; rwlock y; unlock x; x := y ? 3" == [("x",(None,VInt 3)),("y",(ReadWrite,VInt 1))]
```
