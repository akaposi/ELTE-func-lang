# Funckionális nyelvek, 2023 ősz, EA + GY

Fontos, hogy megfelelő kóddal vedd fel a tárgyat:

 * BSc: IP-18KVFPNYEG
 * MSc: IPM-18sztKVFPNYEG
 * MSc esti: IPM-18EsztKVFPNYEG

#### Oktatásforma

- A gyakorlatok jelenléti látogatása kötelező, maximum három hiányzás engedélyezett. Gyakorlatokon jelenléti ív van.
- Az előadások látogatása (BSc hallgatóknak) kötelező.

#### Időpontok és helyszínek

- Előadás (Kaposi Ambrus): szerda 16:00-17:30 (D-1-819 Riesz Frigyes terem)
- Gyakorlatok:
  + Kód: 1, Páll Dominik , szerda  19:30-21:00 (D-2-124 Microsoft labor)
  + Kód: 2, Petes Márton, hétfő  19:30-21:00 (D-2-124 Microsoft labor)
  + Kód: 3, Bálint János Gergely , hétfő    17:45-19:15 (D-2-124 Microsoft labor)

#### Elérhetőségek

A tantárgyal kapcsolatos kérdéseket a közös teams csoportban tegyétek fel. Ennek előnye, hogy mások is látják a kérdést/választ.

- Kaposi Ambrus, email: akaposi @ inf.elte.hu (szóköz nélkül)
- Bálint János Gergely, email: koflyy @ inf.elte.hu (szóköz nélkül)
- Páll Dominik, email: palldominiko8 @ gmail.com (szóköz nélkül)
- Petes Márton, email: marci.petes @ gmail.com (szóköz nélkül)

#### Infrastruktúra

- Haskell installáció: az ajánlott mód a [ghcup](https://www.haskell.org/ghcup/), ha ez valamiért
  nem működik, akkor a ["via native OS manager"](https://www.haskell.org/downloads/) opciókat érdemes
  megnézni. Tetszőleges szövegszerkesztő használható, én személy szerint VS Code-ot vagy Emacs-ot ajánlok.

- Microsoft Teams:
  + A Teams csoport kódja: `97lqbje`
  + A Teams-hez inf-es felhasználónévvel és jelszóval lehet csatlakozni.
  + A Teams a tananyag megbeszélésre és kérdések feltevésére van.

- Feladatok és vizsgák beadásának helye: https://canvas.elte.hu

- Órai jegyzetek helye: https://github.com/akaposi/ELTE-func-lang/tree/master/2022-24-1

#### Előzetes ismeretek

Az ELTE BSc-s "funkcionális programozás" tárgy anyagának készség szintű ismerete
szükséges, vagy hasonló szintű előzetes ismeret a Haskell programozási
nyelvről. Ajánlott a [kezdő Haskell](http://lambda.inf.elte.hu/Index.xml)
jegyzet átismétlése.

#### Követelmények

- Gyakorlati követelmény:
  + Az első gyakorlat kivételével minden gyakorlat elején 10 perces számokérés van, a megoldást
    canvas-ra kell feltölteni. 0, 1, vagy 2 pont jár egy feladatért.
  + Három nagyobb házi feladat ki lesz írva a félév során, mindegyikért 4 pont
    jár. Az összes nagy házi beadási határideje a vizsgaidőszak kezdete.
  + Minimum 13 pont szükséges a gyakorlati követelmény teljesítéséhez. Ez
    tetszőlegesen elérhető óra eleji feladatokból vagy házi feladatokból.
- Gyakorlatok látogatása kötelező, maximum három hiányzás engedélyezett.
- Az előadások látogatása (BSc hallgatóknak) kötelező.
- A tárgyi jegy vizsgaidőszaki vizsgából áll elő. A vizsga jelenléti. A
  vizsgázás előfeltétele a gyakorlati követelmény teljesítése. A vizsga gépes
  feladmegoldásból áll és két órás. Vizsgán minden segédeszköz/internet
  használható, viszont együttműködés más vizsgázóval vagy külső személlyel nem
  engedélyezett, aki ezt megsérti, elégtelen jegyet kap és nem vizsgázhat újra
  az adott vizsgaidőszakban. A vizsga megoldást akárhányszor be lehet küldeni,
  az utolsó beküldött verzió számít.

#### Tematika

|  hét | előadás | gyakorlat                                               |  
|------|---------|---------------------------------------------------------|
|    1 |         | ismétlés                                                |
|      |         | (,), Either, lyuk, ->, polimorfizmus, case, let, []     |
|    2 |         | instance-ok írása sok adattípusra                       |
|      |         | Eq, Show, Ord, Semigroup, Monoid instance-ok            |
|      |         | (,), Either, List, különböző Tree típusok               |
|    3 |         | Functor, instance-ok írása és használata                |
|    4 |         | Foldable                                                |
|    5 |         | Monad, State, IO                                        |
|    6 |         | Applicative, Traversable                                |
|    7 |         | parser, regex, Alternative                              |
|    8 |         | parser combinators                                      |
|    9 |         | whitespace, infix parsing, simple exp language          |
|   10 |         | while nyelv parser, interpreter                         |
|   11 |         | while nyelv kiegészítés új típussal / szint.cukorkákkal |


#### Anyagok

- Órai jegyzetek felkerülnek a github repóba minden gyakorlat/előadás után. A
  gyakorlati anyagok a "gyak_X" mappákba kerülnek, ahol "X" a neptunban
  található csoport kód.
- Korábbi szemeszterek jegyzetei és feladatai elérhetők a github repóban.
- Ajánlott irodalom: http://learnyouahaskell.com/chapters, illetve
  https://en.wikibooks.org/wiki/Haskell. További irodalom órai jegyzetekben
  lehet hivatkozva.
