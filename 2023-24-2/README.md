# Funckionális nyelvek, 2024 tavasz, EA + GY

Fontos, hogy megfelelő kóddal vedd fel a tárgyat:

 * BSc: IP-18KVFPNYEG
 * MSc: IPM-18sztKVFPNYEG
 * MSc esti: IPM-18EsztKVFPNYEG

#### Oktatásforma

- A gyakorlatok jelenléti látogatása kötelező, maximum három hiányzás engedélyezett. Gyakorlatokon jelenléti ív van.
- Az előadások látogatása (BSc hallgatóknak) kötelező.

#### Időpontok és helyszínek

- Előadás (Kaposi Ambrus): Déli Tömb 2-502 (Interaktív tábla), Hétfő 19:30-21:00
- Gyakorlatok:
  - 1.-es csoport (Török Bálint Bence): Déli Tömb 2-520 (Mesterséges Intelligencia labor), Szerda 17:45-19:15
  - 2.-es csoport (Petes Márton): Déli Tömb 2-124 (Microsoft Labor), Szerda 19:30-21:00
  - 3.-as csoport (Páll Dominik): Déli Tömb 2-520 (Mesterséges Intelligencia labor), Szerda 19:30-21:00

#### Elérhetőségek

A tantárgyal kapcsolatos kérdéseket a közös teams csoportban tegyétek fel. Ennek előnye, hogy mások is látják a kérdést/választ.

- Kaposi Ambrus, email: akaposi @ inf.elte.hu (szóköz nélkül)
- Török Bálint Bence (Zserbó), email: fcjylp @ inf.elte.hu (szóköz nélkül)
- Páll Dominik, email: palldominiko8 @ gmail.com (szóköz nélkül)
- Petes Márton, email: tx0lwm @ inf.elte.hu (szóköz nélkül)

#### Infrastruktúra

- Haskell installáció: az ajánlott mód a [ghcup](https://www.haskell.org/ghcup/), ha ez valamiért
  nem működik, akkor a ["via native OS manager"](https://www.haskell.org/downloads/) opciókat érdemes
  megnézni. Tetszőleges szövegszerkesztő használható, én személy szerint VS Code-ot vagy Emacs-ot ajánlok.

- Microsoft Teams:
  + A Teams csoport kódja: `l7ebybr`
  + A Teams-hez inf-es felhasználónévvel és jelszóval lehet csatlakozni.
  + A Teams a tananyag megbeszélésre és kérdések feltevésére van.

- Feladatok és vizsgák beadásának helye: https://tms.inf.elte.hu/student/task-manager

- Előadás jegyzetek helye: https://github.com/akaposi/ELTE-func-lang/tree/master/2023-24-2/ea

#### Előzetes ismeretek

Az ELTE BSc-s "funkcionális programozás" tárgy anyagának készség szintű ismerete
szükséges, vagy hasonló szintű előzetes ismeret a Haskell programozási
nyelvről. Ajánlott a [kezdő Haskell](http://lambda.inf.elte.hu/Index.xml)
jegyzet átismétlése.

#### Követelmények

- Gyakorlati követelmény:
  + Az első gyakorlat kivételével minden gyakorlat elején 10-15 perces számokérés van, a megoldást
    TMS-be kell feltölteni. 0, 1, vagy 2 pont jár egy feladatért.
  + Három nagyobb házi feladat ki lesz írva a félév során, mindegyikért 4 pont
    jár. Az összes nagy házi beadási határideje a vizsgaidőszak vége.
  + Minimum 13 pont szükséges a gyakorlati követelmény teljesítéséhez. Ez
    tetszőlegesen elérhető óra eleji feladatokból vagy házi feladatokból.
  + 28 pont megszerzése esetén a vizsgán +1 jegyet lehet szerezni, legalább 2-es megszerzése esetén.
- Gyakorlatok látogatása kötelező, maximum három hiányzás engedélyezett.
- Az előadások látogatása (BSc hallgatóknak) kötelező.
- A tárgyi jegy vizsgaidőszaki vizsgából áll elő. A vizsga jelenléti. A
  vizsgázás előfeltétele a gyakorlati követelmény teljesítése. A vizsga gépes
  feladmegoldásból áll és két órás. Vizsgán minden segédeszköz/internet
  használható, viszont együttműködés más vizsgázóval, külső személlyel vagy
  mesterséges intelligenciával (ChatGPT, BingAI stb) nem engedélyezett, aki 
  ezt megsérti, elégtelen jegyet kap és nem vizsgázhat újra az adott 
  vizsgaidőszakban. A vizsga megoldást akárhányszor be lehet küldeni,
  az utolsó beküldött verzió számít. A vizsgán csak az önmagában forduló 
  kód lesz értékelve, a kommentek figyelembe lesznek véve.

#### Tematika

| Hét | Előadás | Gyakorlat                                                                               |
|-----|---------|-----------------------------------------------------------------------------------------|
| 1   |         | Ismétlés, parametrikus ADTk (, , Either, ->), case, let kötések, Eq, Show, Ord          |
| 2   |         | Eq, Show, Ord folytatás, Magasabbrendű polimorfizmus, Functor, Magasabbrendű megkötések |
| 3   |         | Hajtogatás, Foldable, Semigroup, Monoid                                                 |
| 4   |         | Monad, State, IO                                                                        |
| 5   |         | Reader, Writer, Except, Monád transzformerek                                            |
| 6   |         | Traversable, Applicative                                                                |
| 7   |         | Parser, Alternative (reguláris kifejezések)                                             |
| 8   |         | Parser kombinátorok, egyszerű kifejezésnyelv, recursive descent parsing algoritmus      |
| 9   |         | While nyelv állításokkal, interpreter                                                   |
| 10  |         | Monad Transformer, Interpreter kiegészítése                                             |
| 11  |         | <Lemaradások behozása>                                                                  |
| 12  |         | Vizsga gyakorlás                                                                        |


#### Anyagok

- Órai jegyzetek felkerülnek a github repóba minden gyakorlat/előadás után. A
  gyakorlati anyagok a "gyak_X" mappákba kerülnek, ahol "X" a neptunban
  található csoport kód.
- Korábbi szemeszterek jegyzetei és feladatai elérhetők a github repóban.
- Ajánlott irodalom: http://learnyouahaskell.com/chapters, illetve
  https://en.wikibooks.org/wiki/Haskell. További irodalom órai jegyzetekben
  lehet hivatkozva.
