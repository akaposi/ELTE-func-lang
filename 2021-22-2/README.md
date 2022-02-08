# Funckionális nyelvek (IPM-18sztKVFPNYEG, IP-18KVFPNYEG, IPM-18EsztKVFPNYEG), 2022 tavasz, EA + GY

#### Online vs. jelenléti oktatás

A szemeszter legalább három hét **távolléti** oktatással indul. Utána jelenléti
oktatás várható, hogy pontosan mikor történik a váltás, az később derül ki.

#### Időpontok és helyszínek

- Előadás (Kovács András): csütörtök 18:00-19:30 (00-803 Programozási Nyelvi labor)
- Gyakorlatok:
  + Kód: 1, Kovács András, kedd   19:30-21:00 (LD-2-202 (PC3))
  + Kód: 2, Bense Viktor,  szerda 17:45-19:15 (LÉ-7-16 (PC12))
  + Kód: 3, Donkó István,  szerda 19:30-21:00 (LD-2-202 (PC3))

#### Elérhetőségek

- Kovács András, email: kovacsandras @ inf.elte.hu (szóköz nélkül)
- Donkó István, email: isti115 @ inf.elte.hu (szóköz nélkül)
- Bense Viktor, email: qils07 @ inf.elte.hu (szóköz nélkül)

#### Infrastruktúra, online oktatás

- Compiler installáció: legcélszerűbb [Haskell
  Platform](https://www.haskell.org/platform/) vagy
  [stack](https://docs.haskellstack.org/en/stable/README/)
  segítségével. Szerkesztéshez megfelel tetszőleges szerkesztő GHCI-vel, de
  nyilván használható szofisztikáltabb környezet (pl. emacs
  [haskell-mode](http://haskell.github.io/haskell-mode/), VSCode), aki ismer
  ilyet.

- Microsoft Teams: az *előadások* Teams-en történnek a távolléti oktatásban, és rögzítésre kerülnek
  + A Teams-hez inf-es felhasználónévvel és jelszóval lehet csatlakozni.
  + Minden hallgató email-ben kap linket, amivel csatlakozni lehet a tárgyi team-hez.

- Discord: a *gyakorlatok* [Discord](https://discord.com/)-on folynak a távolléti oktatásban.
  + A tárgynak egy szerver van Discord-on, az egyes gyakorlatoknak külön voice channel van.
  + Minden hallgató email-ben kap linket a szerverhez.

- Feladatok és vizsgák beadásának helye: https://canvas.elte.hu

- Órai jegyzetek helye: https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2021-22-2

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
  + Minimum 13 pont szükséges a gyakorlati követelmény teljesítéséhez. Ez
    tetszőlegesen elérhető óra eleji feladatokból vagy házi feladatokból.
- Gyakorlatok látogatása kötelező (online vagy helyszínen, a hatályos rend szerint), maximum három hiányzás engedélyezett.
  + Az online részvétel esetén az óra eleji feladat tartja számon a
    jelenlétet. Ezért fontos, hogy akkor is küldjünk be megoldást, ha az hiányos!
  + A helyszíni részvételt hagyományos névsorral tartjuk számon.
- Előadás látogatása nem kötelező.
- A tárgyi jegy vizsgaidőszaki vizsgából áll elő. A vizsgázás előfeltétele a
  gyakorlati követelmény teljesítése.  A vizsga gépes feladmegoldásból áll és
  két órás. Vizsgán minden segédeszköz/internet használható, viszont
  együttműködés más vizsgázóval vagy külső személlyel nem engedélyezett, aki ezt
  megsérti, elégtelen jegyet kap és nem vizsgázhat újra az adott
  vizsgaidőszakban. A vizsga megoldást akárhányszor be lehet küldeni, az utolsó
  beküldött verzió számít.

#### Tematika

- Algebrai adattípusok
- Típusosztályok
- Egyszerű osztályok: Eq, Ord, Show, Monoid
- Functor, Foldable
- Monad, egyes monádok: State, Maybe, lista, IO
- Applicative, Traversable
- Parser monád
- Parser és interpreter egyszerű nyelvekhez

#### Anyagok

- Órai feladatok és megoldásaik felkerülnek a github repóba minden
  gyakorlat/előadás után. Gyakorlati anyagok a "gyak_X" mappákba kerülnek, ahol
  "X" a neptunban található csoport kód.
- Korábbi szemeszterek jegyzetei és feladatai elérhetők a github repóban.
- Ajánlott irodalom: http://learnyouahaskell.com/chapters, illetve
  https://en.wikibooks.org/wiki/Haskell. További irodalom órai jegyzetekben
  lehet hivatkozva.
