# Funckionális nyelvek, 2025 ősz, EA + GY

Fontos, hogy megfelelő kóddal vedd fel a tárgyat:

 * BSc: IP-18KVFPNYEG
 * MSc: IPM-18sztKVFPNYEG
 * MSc esti: IPM-18EsztKVFPNYEG

#### Oktatásforma

- A gyakorlatok jelenléti látogatása kötelező, maximum három hiányzás engedélyezett.
- Az előadások látogatása (BSc hallgatóknak) kötelező.

#### Időpontok és helyszínek

- Előadás (Xie Zongpu / Szumi): Déli Tömb 2-502, Csütörtök 17:45-19:15.
- Gyakorlatok:
  - 1-es csoport (Suszter Roland): Déli Tömb 00-524 (PC 4), Péntek 8:15-9:45 
  - 3-as csoport (Petes Márton): Déli Tömb 00-412 (PC 6), Kedd 17:45-19:15


#### Elérhetőségek

- Xie Zongpu, email: TBA
- Petes Márton, email: tx0lwm @ inf.elte.hu (szóköz nélkül)
- Suszter Roland, Teams, email: xjq2db @ inf.elte.hu (szóköz nélkül)

#### Infrastruktúra

- Haskell installáció: az ajánlott mód a [ghcup](https://www.haskell.org/ghcup/), ha ez valamiért nem működik, akkor a ["via native OS manager"](https://www.haskell.org/downloads/) opciókat érdemes megnézni. Tetszőleges szövegszerkesztő használható.

- Feladatok és vizsgák beadásának helye: https://tms.inf.elte.hu/student/task-manager

- Előadás jegyzetek helye: https://github.com/akaposi/ELTE-func-lang/tree/master/2025-26-1/ea

#### Előzetes ismeretek

Az ELTE BSc-s "funkcionális programozás" tárgy anyagának készség szintű ismerete szükséges, vagy hasonló szintű előzetes ismeret a Haskell programozási nyelvről. Ajánlott a [kezdő Haskell](http://lambda.inf.elte.hu/Index.xml) jegyzet átismétlése.

#### Követelmények

- Gyakorlati követelmény:
  + Az első gyakorlat kivételével minden gyakorlat elején 10-15 perces számokérés van, a megoldást TMS-be kell feltölteni. 0, 1, vagy 2 pont jár egy feladatért.
  + Három nagyobb házi feladat ki lesz írva a félév során, mindegyikért 4 pont jár. Az összes nagy házi beadási határideje a vizsgaidőszak vége. A ponthatárokba ezek a feladatok csak akkor számítanak bele ha a vizsga napja előtt le lettek adva.
  + Minimum 13 pont szükséges a gyakorlati követelmény teljesítéséhez. Ez tetszőlegesen elérhető óra eleji feladatokból vagy házi feladatokból.
  + 28 pont megszerzése esetén a vizsgán +1 jegyet lehet szerezni, legalább 2-es megszerzése esetén.
- Gyakorlatok látogatása kötelező, maximum három hiányzás engedélyezett.
- Az előadások látogatása (BSc hallgatóknak) kötelező.
- A tárgyi jegy vizsgaidőszaki vizsgából áll elő. A vizsga jelenléti. A vizsgázás előfeltétele a gyakorlati követelmény teljesítése. A vizsga gépes feladmegoldásból áll és két órás. Vizsgán minden segédeszköz/internet használható, viszont együttműködés más vizsgázóval, külső személlyel vagy mesterséges intelligenciával (ChatGPT, BingAI stb) nem engedélyezett, aki ezt megsérti, elégtelen jegyet kap és nem vizsgázhat újra az adott vizsgaidőszakban. A vizsga megoldást akárhányszor be lehet küldeni, az utolsó beküldött verzió számít. A vizsgán csak az önmagában forduló kód lesz értékelve, a kommentek figyelembe lesznek véve.

#### Tematika

| Hét | Előadás | Gyakorlat                                                                               |
|-----|---------|-----------------------------------------------------------------------------------------|
| 1   | Lustaság (CbN, CbV), ADTk, lusta Nat, Kind rendszer, Típuscsaládok, Típusellenőrzés | Ismétlés, parametrikus ADTk (, , Either, ->), case, let kötések, Eq, Show, Ord          |
| 2   |         | Eq, Show, Ord folytatás, Magasabbrendű polimorfizmus, Functor, Magasabbrendű megkötések |
| 3   |         | Hajtogatás, Foldable, Semigroup, Monoid                                                 |
| 4   |         | Monad, IO                                                                               |
| 5   |         | State, Except                                                                           |
| 6   |         | Reader, Writer, Except, Monád transzformerek                                            |
| 7   |         | Traversable, Applicative                                                                |
| 8   |         | Parser, Alternative (reguláris kifejezések)                                             |
| 9   |         | Parser kombinátorok, egyszerű kifejezésnyelv, recursive descent parsing algoritmus      |
| 10  |         | While nyelv állításokkal, interpreter                                                   |
| 11  |         | Interpreter kiegészítése                                                                |


#### Anyagok

- Órai jegyzetek felkerülnek a github repóba minden gyakorlat/előadás után. A gyakorlati anyagok a "gyak_X" mappákba kerülnek, ahol "X" a neptunban található csoport kód.
- Korábbi szemeszterek jegyzetei és feladatai elérhetők a github repóban.
- Ajánlott irodalom: http://learnyouahaskell.com/chapters, illetve https://en.wikibooks.org/wiki/Haskell. További irodalom órai jegyzetekben lehet hivatkozva.
