# Funckionális nyelvek (IPM-18sztKVFPNYEG, IP-18KVFPNYEG, IPM-18EsztKVFPNYEG), 2021 tavasz, EA + GY

#### Időpontok és helyszínek

- Előadás (Kovács András): péntek 16:00-17:30
- Gyakorlatok:
  + Kód: 1, Kovács András, péntek 17:45-19:15
  + Kód: 2, Donkó István, kedd 19:30-21:00
  + Kód: 3, Donkó István, szerda 17:45-19:15
  + Kód: 4, Rafaël Bocquet, (**angol** nyelven), szerda 17:45-19:15
 - Konzultáció: péntek 15:00-16:00, MS Teams General csoport meeting

#### Elérhetőségek

- Kovács András, email: kovacsandras @ inf.elte.hu (szóköz nélkül).
- Rafaël Bocquet, email: bocquet @ inf.elte.hu (szóköz nélkül).
- Donkó István, email: isti115 @ inf.elte.hu (szóköz nélkül)

#### Infrastruktúra, online oktatás

- Compiler installáció: legcélszerűbb [Haskell
  Platform](https://www.haskell.org/platform/) vagy
  [stack](https://docs.haskellstack.org/en/stable/README/)
  segítségével. Szerkesztéshez megfelel tetszőleges szerkesztő GHCI-vel, de
  nyilván használható szofisztikáltabb környezet (pl. emacs
  [haskell-mode](http://haskell.github.io/haskell-mode/), VSCode), aki ismer
  ilyet.

- Microsoft Teams: az *előadások* Teams-en történnek, és rögzítésre kerülnek.
  A vizsgák során kérdések/egyéb kommunikáció szintén Teams-en történik.
  + A Teams-hez inf-es felhasználónévvel és jelszóval lehet csatlakozni.
  + Minden hallgató email-ben kap linket, amivel csatlakozni lehet a tárgyi team-hez.

- Discord: a *gyakorlatok* [Discord](https://discord.com/)-on folynak,
  elsősorban az ottani párhuzamos képernyőmegosztás feature miatt.
  + A tárgynak egy szerver van Discord-on, az egyes gyakorlatoknak külön voice channel van.
  + Minden hallgató email-ben kap linket.

- Feladatok beadásának helye: https://bead.inf.elte.hu/
  + Regisztrálni lehet inf-es azonosítóval és jelszóval.
  + Minden hallgató a saját gyakorlati csoportjához csatlakozzon (a csoport
    számok megegyeznek a neptun-ban találhatóval).

#### Előzetes ismeretek

Az ELTE BSc-s "funkcionális programozás" tárgy anyagának készség szintű ismerete
szükséges, vagy hasonló szintű előzetes ismeret a Haskell programozási
nyelvről. Ajánlott a [kezdő Haskell](http://lambda.inf.elte.hu/Index.xml)
jegyzet átismétlése.

#### Követelmények

- Gyakorlati követelmény:
  + Az első gyakorlat kivételével minden gyakorlat elején 10 perces számokérés, a megoldást
    BEAD-ba kell feltölteni. 0, 1, vagy 2 pont jár egy feladatért.
  + Három nagyobb házi feladat ki lesz írva, mindegyikért 4 pont jár.
  + Minimum 13 pont szükséges a gyakorlati követelmény teljesítéséhez. Ez tetszőlegesen
    elérhető óra eleji feladatokból vagy házi feladatokból.
- Gyakorlatok látogatása kötelező, maximum három hiányzás engedélyezett.
  A gyakorlati jelenlétet a beküldött óra eleji BEAD feladatok tartják nyilván.
  Ezért fontos, hogy akkor is küldjünk BEAD megoldást, ha az hiányos!
- Előadás látogatása nem kötelező.
- A tárgyi jegy vizsgaidőszaki vizsgából áll elő. A vizsgázás előfeltétele a
  gyakorlati követelmény teljesítése. A vizsga gépes feladmegoldásból áll és
  kb. két órás, bonyolítása szintén BEAD-on történik. Vizsgán minden
  segédeszköz/internet használható, kommunikáció más vizsgázóval vagy külső
  személlyel nem engedélyezett.

#### Tematika

- Algebrai adattípusok
- Típusosztályok
- Egyszerű osztályok: Eq, Ord, Show, Monoid
- Functor, Foldable
- Monad, egyes monádok: Reader, State, Maybe, lista, IO
- Applicative, Traversable
- Parser monád
- Monad transformer-ek
- Parser és interpreter egyszerű nyelvekhez

#### Anyagok

- Órai feladatok és megoldásaik felkerülnek ebbe a repóba minden
  gyakorlat/előadás után. Gyakorlati anyagok a "gyak_X" mappákba kerülnek, ahol
  "X" a neptunban található csoport kód.
- Korábbi szemeszterek vizsgasorai és feladatai elérhetők ugyanebben a repo-ban.
- Ajánlott irodalom: http://learnyouahaskell.com/chapters, illetve
  https://en.wikibooks.org/wiki/Haskell. További irodalom órai jegyzetekben
  lehet hivatkozva.
