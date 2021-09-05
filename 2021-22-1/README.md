# Funckionális nyelvek (IPM-18sztKVFPNYEG, IP-18KVFPNYEG, IPM-18EsztKVFPNYEG), 2021 ősz, EA + GY

#### Időpontok és helyszínek

- Előadás (Kovács András): szerda 16:00-17:30 (LD-1-820 Hajós György terem)
- Gyakorlatok:
  + Kód: 1, Kovács András, csütörtök 16:00-17:30 (LD-2-710 (PC10))
  + Kód: 2, Rafaël Bocquet, csütörtök 17:45-19:15 (LD-2-710 (PC10))
  + Kód: 3, Rafaël Bocquet, csütörtök 19:30-21:00 (LD-00-503)
 - Konzultáció: hamasoran kitűzzük az időpontot

#### Elérhetőségek

- Kovács András, email: kovacsandras @ inf.elte.hu (szóköz nélkül).
- Rafaël Bocquet, email: bocquet @ inf.elte.hu (szóköz nélkül).

#### Infrastruktúra, online oktatás

- Compiler installáció: legcélszerűbb [Haskell
  Platform](https://www.haskell.org/platform/) vagy
  [stack](https://docs.haskellstack.org/en/stable/README/)
  segítségével. Szerkesztéshez megfelel tetszőleges szerkesztő GHCI-vel, de
  nyilván használható szofisztikáltabb környezet (pl. emacs
  [haskell-mode](http://haskell.github.io/haskell-mode/), VSCode), aki ismer
  ilyet.

- Microsoft Teams: az *előadások* Teams-en történnek (szinkronban a tantermi előadással), és rögzítésre kerülnek
  A vizsgák során kérdések/egyéb kommunikáció szintén Teams-en történik.
  + A Teams-hez inf-es felhasználónévvel és jelszóval lehet csatlakozni.
  + Minden hallgató email-ben kap linket, amivel csatlakozni lehet a tárgyi team-hez.

- Discord: a *gyakorlatok* [Discord](https://discord.com/)-on folynak, szintén szinkron a tantermi laborral.
  + A tárgynak egy szerver van Discord-on, az egyes gyakorlatoknak külön voice channel van.
  + Minden hallgató email-ben kap linket.

- Feladatok és vizsgák beadásának helye: https://canvas.elte.hu
  + A feladatok beadásának technikai részleteit az első héten tisztázzuk.

- Órai jegyzetek helye: https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2021-22-1

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
    jár. Az összes nagy házi beadási határideje a szorgalmi időszak vége.
  + Minimum 13 pont szükséges a gyakorlati követelmény teljesítéséhez. Ez tetszőlegesen
    elérhető óra eleji feladatokból vagy házi feladatokból.
- Gyakorlatok látogatása kötelező (online vagy helyszínen, a hibrid oktatási szabályzás szerint), maximum három hiányzás engedélyezett.
  + Az online részvétel esetén az óra eleji feladat tartja számon a
    jelenlétet. Ezért fontos, hogy akkor is küldjünk be megoldást, ha az hiányos!
  + A helyszíni részvételt hagyományos névsorral tartjuk számon.
- Előadás látogatása nem kötelező.
- A tárgyi jegy vizsgaidőszaki vizsgából áll elő. A vizsgázás előfeltétele a gyakorlati követelmény teljesítése.
  A vizsga gépes feladmegoldásból áll és kb. két órás, beadni szintén canvas-on
  kell. Vizsgán minden segédeszköz/internet használható, viszont együttműködés
  más vizsgázóval vagy külső személlyel nem engedélyezett, aki ezt megsérti,
  elégtelen jegyet kap és nem vizsgázhat újra a vizsgaidőszakban. A vizsga
  megoldást akárhányszor be lehet küldeni, az utolsó beküldött verzió számít.

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

- Órai feladatok és megoldásaik felkerülnek a github repóba minden
  gyakorlat/előadás után. Gyakorlati anyagok a "gyak_X" mappákba kerülnek, ahol
  "X" a neptunban található csoport kód.
- Korábbi szemeszterek jegyzetei és feladatai elérhetők a github repóban.
- Ajánlott irodalom: http://learnyouahaskell.com/chapters, illetve
  https://en.wikibooks.org/wiki/Haskell. További irodalom órai jegyzetekben
  lehet hivatkozva.
