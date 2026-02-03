# Funckionális nyelvek, 2025/26 tavasz, EA + GY

Fontos, hogy megfelelő kóddal vedd fel a tárgyat:

 * BSc: IP-18KVFPNYEG
 * MSc: IPM-18sztKVFPNYEG
 * MSc esti: IPM-18EsztKVFPNYEG

#### Oktatásforma

- A gyakorlatok jelenléti látogatása kötelező, maximum három hiányzás engedélyezett.
- Az előadások látogatása (BSc hallgatóknak) kötelező.

#### Időpontok és helyszínek

| Csoport      | Időpont               | Terem            | Oktató          | Nyelv  |
|--------------|-----------------------|------------------|-----------------|--------|
| 90 (Előadás) | Kedd 10:00-12:00      | Déli Tömb 2-502  | Szumi           | Angol  |
| 1            | Péntek 16:00-17:30    | Déli Tömb 00-411 | Petes Márton    | Magyar |
| 2            | Csütörtök 16:00-17:30 | Déli Tömb 2-710  | Suszter Roland  | Magyar |
| 3            | Kedd 14:00-16:00      | Déli Tömb 2-202  | Stefania Damato | Angol  |

#### Elérhetőségek

| Név                               | Email (szóköz nélkül)          | Teams                               |
|-----------------------------------|--------------------------------|-------------------------------------|
| Szumi                             | szumi @ inf.elte.hu            |                                     |
| Petes Márton                      | tx0lwm @ inf.elte.hu           | Petes Márton / TX0LWM (Preferált)   |
| Suszter Roland                    | xjq2db @ inf.elte.hu           | Suszter Roland / XJQ2DB (Preferált) |
| Stefania Damato                   | stefania.damato @ inf.elte.hu  |                                  |
| Szávó Tamás (Javító Demonstrátor) | hq11w9 @ inf.elte.hu           | Szávó Tamás / HQ11W9 (Preferált)    |


#### Infrastruktúra

- Haskell installáció: az ajánlott mód a [ghcup](https://www.haskell.org/ghcup/), ha ez valamiért nem működik, akkor a ["via native OS manager"](https://www.haskell.org/downloads/) opciókat érdemes megnézni. Tetszőleges szövegszerkesztő használható.

- Feladatok és vizsgák beadásának helye: https://tms.inf.elte.hu/student/task-manager

- Előadás jegyzetek helye: https://github.com/akaposi/ELTE-func-lang/tree/master/2025-26-2/ea

#### Előzetes ismeretek

Az ELTE BSc-s "funkcionális programozás" tárgy anyagának készség szintű ismerete szükséges, vagy hasonló szintű előzetes ismeret a Haskell programozási nyelvről. Ajánlott a [kezdő Haskell](http://lambda.inf.elte.hu/Index.xml) jegyzet átismétlése.

#### Követelmények

- Gyakorlati követelmény:
  + A gyakorlatok látogatása kötelező, maximum 3-szor lehet hiányozni.
  + Minden héten lesz egy házi feladat azon gyakorlat anyagábol és azzal kapcsolódó témakörökből. A házi feladatok kitöltésé a gyakorlat végétől számolva 2 hét. A házi feladatok kitöltése kötelező.
  + A félév második felében három nem kötelező nagy beadandó lesz kiadva. A beadandók mind 4 pontot érnek, határidejük nincsen.
  + 8 pont megszerzése esetén a vizsgán +1 jegyet lehet szerezni, legalább 2-es megszerzése esetén
- Előadás követelményei:
  + Az előadás látogatása BSc hallagtóknak kötelező.
  + A gyakorlati követelmények teljesítése esetén, a félév végen egy gyakorlati vizsgát kell tenni.
  + A vizsga nem tartalmaz elméleti részt és csak a gyakorlaton tanított anyagot, illetve a BSc Funkcionális Programozáson tanult anyagot kéri számon.
- A számonkérések során (mind a házi feladatban, beadandókban és a vizsgán) teszőleges segédezközt lehet használni, akár saját gépet is, kivéve emberi segítséget vagy mesteséges intelligenciát (ChatGPT, Copilot, Claude, stb.). Vizsga esetén, aki Visual Studio Code-ot használ, attól megkérjük a Copilotot a vizsga előtt kapcsolja még ki. Minden számonkérésen a gyakorlatvezetőktöl lehet kérdezni és segítséget kérni.

#### Tematika

| Hét | Előadás | Gyakorlat                                                                          |
|-----|---------|------------------------------------------------------------------------------------|
| 1   |         | Ismétlés                                                                           |
| 2   |         | Funktor, Kind rendszer, Higher-Kinded Polymorphism                                 |
| 3   |         | Hajtogatás, Foldable, Semigroup, Monoid                                            |
| 4   |         | Monad, IO                                                                          |
| 5   |         | State, Except                                                                      |
| 6   |         | Reader, Writer, Except, Monád transzformerek                                       |
| 7   |         | Traversable, Applicative                                                           |
| 8   |         | Parser, Alternative (reguláris kifejezések)                                        |
| 9   |         | Parser kombinátorok, egyszerű kifejezésnyelv, recursive descent parsing algoritmus |
| 10  |         | While nyelv állításokkal, interpreter                                              |
| 11  |         | Interpreter kiegészítése                                                           |


#### Anyagok

- Órai jegyzetek felkerülnek a github repóba minden gyakorlat/előadás után. A gyakorlati anyagok az "X" mappákba kerülnek, ahol "X" a neptunban található csoport kód.
- Korábbi szemeszterek jegyzetei és feladatai elérhetők a github repóban.
- Ajánlott irodalom: http://learnyouahaskell.com/chapters, illetve https://en.wikibooks.org/wiki/Haskell. További irodalom órai jegyzetekben lehet hivatkozva.
