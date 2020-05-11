# Funckionális nyelvek (IPM-18sztKVFPNYEG, IP-18KVFPNYEG, IPM-18EsztKVFPNYEG), 2020 tavasz, EA + GY

#### Időpontok és helyszínek

- Előadás (Kovács András): csütörtök 16:00-17:30, északi épület -1.75
- Gyakorlatok:
  + Kód: 1, Kovács András, csütörtök 17:45-19:15, északi épület 7.16 (PC 12)
  + Kód: 2, Podlovics Péter, csütörtök 19:30-21:00, 00-410 (PC 1)
  + Kód: 3, Podlovics Péter, csütörtök 17:45-19:15, 00-411 (PC 7)
  + Kód: 4, Rafaël Bocquet, (**angol** nyelven) hétfő 17:45-19:15, 2-107 (PC2)

#### Elérhetőségek

- Kovács András, email: kovacsandras @ inf.elte.hu (szóköz nélkül).
- Podlovics Péter, email: anabra @ inf.elte.hu (szóköz nélkül).
- Rafaël Bocquet, email: bocquet @ inf.elte.hu (szóköz nélkül).
- Szeretnénk kérni, hogy a fenti címekre küldött levelek tárgya `[fny] <tárgy>` formátumú legyen.
- Feladatok beadásának helye: https://bead.inf.elte.hu/
  + Ide regisztrálni kell ugyanazzal a névvel/jelszóval, mint amivel labor
    gépekbe lehet belépni.

#### Infrastruktúra

- Compiler installáció: legcélszerűbb [Haskell
  Platform](https://www.haskell.org/platform/) vagy
  [stack](https://docs.haskellstack.org/en/stable/README/)
  segítségével. Szerkesztéshez megfelel tetszőleges szerkesztő GHCI-vel, de
  nyilván használható szofisztikáltabb környezet (pl. emacs
  [haskell-mode](http://haskell.github.io/haskell-mode/), VSCode), aki ismer
  ilyet.

#### Előzetes ismeretek

Az ELTE BSc-s "funkcionális programozás" tárgy anyagának készség szintű ismerete
szükséges, vagy hasonló szintű előzetes ismeret a Haskell programozási
nyelvről. Ajánlott a [kezdő Haskell](http://lambda.inf.elte.hu/Index.xml)
jegyzet átismétlése.

#### Követelmények

- Gyakorlatok látogatása kötelező, maximum három hiányzás engedélyezett.
- Előadás látogatása *nem kötelező*, viszont erősen ajánlott.
- A tárgyi jegy vizsgaidőszaki vizsgából áll elő. Ez gépes feladmegoldásból áll
  és kb. két órás. Vizsgán minden segédeszköz/internet használható, kommunikáció
  más vizsgázóval vagy külső személlyel nem engedélyezett.
- A vizsgázás előfeltétele a gyakorlati követelmény teljesítése.
- Gyakorlati követelmény:
  + **Módosulás**: a 2020 tavaszi szemeszterbeli távoktatás miatt **nincs** elvárt követelmény a gyakorlaton. Vizsgára jelentkezhez minden tárgyra regisztrált hallgató.

#### Tematika

- Algebrai adattípusok
- Típusosztályok
- Egyszerű osztályok: Eq, Ord, Show, Monoid
- Functor, Foldable
- Monad, egyes monádok: Reader, State, Maybe, lista, IO
- Applicative, Traversable
- Parser monád
- Monad transformer
- Parser és interpreter egyszerű nyelvekhez

#### Anyagok

- Órai feladatok és megoldásaik felkerülnek ebbe a repóba minden
  gyakorlat/előadás után. Gyakorlati anyagok a "gyak_X" mappákba kerülnek, ahol
  "X" a neptunban található csoport kód.
- Ajánlott irodalom: http://learnyouahaskell.com/chapters, illetve
  https://en.wikibooks.org/wiki/Haskell. További irodalom órai jegyzetekben
  lehet hivatkozva.
