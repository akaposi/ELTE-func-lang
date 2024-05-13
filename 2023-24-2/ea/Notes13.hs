kutatasi temak

- analizist Agdaban formalizalni *konstruktivan*
  - Lean-ben formalizaltak az egesz analizist, klasszikusan

  Hilbert:
  Taking the principle of excluded middle from the mathematician
  would be the same, say, as proscribing the telescope to the as-
  tronomer or to the boxer the use of his fists. To prohibit existence
  statements and the principle of excluded middle is tantamount to
  relinquishing the science of mathematics altogether.

  Brouwer: intuicionista

  Bishop: Constructive analysis
  - borzaszto
  
  homotopia tipuselmelet

  ℤ := ℕ×ℕ/~     (a,b) ~ (a',b') := (a+b' = a'+b)  (2,1)~(1,0)~(100,99)
    fst : ℤ → ℕ

  valos szamok:
     ℝ := (f : ℕ → ℚ | ∀ε.∃N.∀n,m>N, |f m - f n∣<ε)/~
        f ~ g := ∀ε.∃N.∀n>N.|f n - g n| < ε
     (f : ℕ → ℝ , f Cauchy, akkor f ∈ ℝ), HoTT konyv
     
  ∀n,m∃k.k|n∧k|m∧k a legnagyobb ilyen
  
  p : (n m : ℕ) → Σ (k : ℕ) . k | n × k | m × ...
  fst ∘ p : ℕ → ℕ → ℕ

  f : ℕ → ℕ → ℕ

  - téma: kubikalis Agdaban analizis felepitese konstruktivan

  A : Type
  B : A → Type
  ∥_∥ : Type → Type       propositional truncation
  Choice : ((a : A) → ∥B a∥) → ∥(a : A) → B a∥
  unique choice : ((a:A)→∥Σ(b:B a).∀b'.b=b'∥) → ∥(a:A) → B a∥

  topos theory
  reverse mathematics
  proof theoretic strength
- HOTT  <-  HoTT
  - Voevodsky: HoTT := MLTT + univalence axiom     (Agda + ((A≅B) → A=B))
    - canonicity: (b : Tm ◇ Bool) → (b = true) ⊎ (b = false)
    - conjecture: homotopy canonicity:
      (b : Tm ◇ Bool) → Tm ◇ (Id Bool b true) ⊎ Tm ◇ (Id Bool b false)
  - kubikalis tipuselmelet, Id A a b := (f : 𝕀 → A) × (f 0 = a) × (f 1 = b)
      transport, hcomp<- Kan operation
    kanonicitas igaz, normalizalas igaz, van implementacio: Cubical Agda
  - H.O.T.T. Id (A×B) (a₀,b₀) (a₁,b₁) := Id A a₀ a₁ × Id B b₀ b₁
             Id ℕ zero zero := ⊤
             Id ℕ (suc n) (suc m) := Id ℕ n m
             Id ℕ _ _ := ⊥
             Id (Σ A B) (a₀,b₀) (a₁,b₁) :=
             Id Type A B := (A ≃ B)
    + egyszerubb, konnyen elmagyarazhato
    + technikai: tobb szamitasi szabaly van benn
    + hatekonyabb (?)
    + filozofiailag matematika elotti intuiciokkal leirhato


    data Tree : Set where
      node : Tree → Tree → Tree
      leaf : Tree

    data Tree= : Tree → Tree → Set where
      node= : Tree= l1 l2 → Tree= r1 r2 → Tree= (node l1 r1) (node l2 r2)
      leaf= : Tree= leaf leaf

    record A : Set
      des : Id A a b → Bool             des refl : Bool


    data UP (A : Set) : Set             record UP' (A : Set) where
      _,_ : A → A → UP A                   fst : A
      eq  : a , b = b , a                  snd : A
                                           eq  : Id (UP' A) u v →

     0 = 1 → ⊥

-- jovo ora 39+18+18 perccel rovidebb (ez mar kumulalt ertek)
