-- algebrai adattipusok algebraiak ket ok miatt:
-- 1. inicialis algebrak (pl. Nat az inicialis "pontozott halmaz endofuggvennyel")
-- 2. a tipusok exponential commutative rig-et alkotnak
{-
Either a (Either b c) ≅ Either (Either a b) c
Either Empty a ≅ a
Either a Empty ≅ a
Either a b ≅ Either b a

((a,b),c) ≅ (a,(b,c))   -- (a,b,c)
((),a) ≅ a
(a,()) ≅ a
(a,b) ≅ (b,a)

(a,Either b c) ≅ (Either a b,Either a c)

(b,c) -> a ≅ b->c->a                 a^(b*c) = (a^b)^c
Either b c -> a ≅ (b->a,c->a)        a^(b+c) = a^b * a^c
Empty ->      a ≅ Unit               a^0 = 1
Unit  ->      a ≅ a                  a^1 = a
a     -> Unit ≅ Unit                 1^a = 1
-}

-- Eq, Show, Ord folytatás, Magasabbrendű polimorfizmus, Functor, Magasabbrendű megkötések
-- Hajtogatás, Foldable, Semigroup, Monoid


-- koinduktiv tipusok potencialisan vegtelen melysegu fak, ezek is Church kodolhatok
