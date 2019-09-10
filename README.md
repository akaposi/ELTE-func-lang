# Funckionális nyelvek (IPM-18sztKVFPNYEG, IP-18KVFPNYEG, IPM-18EsztKVFPNYEG), 2019 ősz, EA + GY

#### Időpontok és helyszínek

- Előadás: szerda 16:00-17:30, **lovarda** (déli épület északnyugati sarok földszinten, gépterem)
- Gyakorlatok: 
  + szerda 17:45-19:15, 00-411 (PC7)
  + péntek 12:00-13:30, 00-412 (PC6)

#### Elérhetőségek

- Kovács András (előadás), email: kovacsandras @ inf.elte.hu (szóköz nélkül).
- Podlovics Péter (gyakorlatok), email: anabra @ inf.elte.hu (szóköz nélkül).
- Szeretnénk kérni, hogy a fenti címekre küldött levelek tárgya `[fny] <tárgy>` formátumú legyen.
- Feladatok beadásának helye: https://bead.inf.elte.hu/
  + Ide regisztrálni kell ugyanazzal a névvel/jelszóval, mint amivel labor
    gépekbe lehet belépni. Gyakorlati csoportok itt még nincsenek létrehozva,
    ne vegyetek föl egyelőre semmit.

#### Infrastruktúra

- Compiler installáció: legcélszerűbb [Haskell Platform](https://www.haskell.org/platform/) vagy [stack](https://docs.haskellstack.org/en/stable/README/) segítségével. Szerkesztéshez megfelel tetszőleges szerkesztő GHCI-vel, de nyilván használható szofisztikáltabb környezet (pl. emacs [haskell-mode](http://haskell.github.io/haskell-mode/), VSCode), aki ismer ilyet.

#### Előzetes ismeretek

Az ELTE BSc-s "funkcionális programozás" tárgy anyagának készség szintű ismerete szükséges, vagy hasonló szintű előzetes ismeret a Haskell programozási nyelvről. Ajánlott a [kezdő Haskell](http://lambda.inf.elte.hu/Index.xml) jegyzet átismétlése.

#### Követelmények

- Gyakorlatok látogatása kötelező, maximum három hiányzás engedélyezett.
- Előadás látogatása *nem kötelező*, viszont erősen ajánlott. Az előadás csak névlegesen előadás, gépteremben van, és többnyire gyakorlófeladatok megoldásából és konzultációból áll.
- A tárgyi jegy vizsgaidőszaki vizsgából áll elő. Ez gépes feladmegoldásból áll és kb. két órás. 
- A vizsgázás előfeltétele a gyakorlati követelmény teljesítése.
- Gyakorlati követelmény:
  + Gyakorlatok elején 10 perces feladatmegoldás BEAD-ban. Ezekre 0, 1 vagy 2 pont kapható. Az első gyakorlaton
    nem lesz ilyen feladat. Összesen 12 darab feladat várható.
  + Továbbá, ki lesz írva félév során 3 darab házi feladat, mindegyik 4 pontot ér. 
  + Legalább 13 gyakorlati pontot kell megszerezni a félév során. 
  
#### Tematika

- Algebrai adattípusok
- Típusosztályok
- Egyszerű osztályok: Eq, Ord, Show, Monoid
- Functor, Foldable
- Monad, egyes monádok: Reader, State, Maybe, lista, IO
- Applicative
- Parser monád
- Monad transformer
- Parser és interpreter egyszerű nyelvekhez
  
#### Anyagok

- Órai feladatok és megoldásaik felkerülnek ebbe a repóba minden gyakorlat/előadás után. 
- Ajánlott irodalom: http://learnyouahaskell.com/chapters, illetve https://en.wikibooks.org/wiki/Haskell. További irodalom órai jegyzetekben lehet hivatkozva.
