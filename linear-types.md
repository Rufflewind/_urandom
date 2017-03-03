# Linear logic

## Rules

Source: Philip Wadler, [*A taste of linear logic*](http://homepages.inf.ed.ac.uk/wadler/papers/lineartaste/lineartaste-revised.pdf).

### ⟨Id⟩

    . ⟨a : A⟩ ⊢ a : A

### [Id]

    . [a : A] ⊢ a : A

### Exchange

    Γ, Δ ⊢ a : A
    . Δ, Γ ⊢ a : A

### Contraction

    Γ, [a : A], [a′ : A] ⊢ b[a, a′] : B
    . Γ, [a : A] ⊢ b[a, a] : B

### Weakening

    Γ ⊢ b : B
    . Γ, [a : A] ⊢ b : B

### !-I

    [Γ] ⊢ a : A
    . [Γ] ⊢ !a : !A

### !-E

    Γ ⊢ a′ : !A
    Δ, [a : A] ⊢ b[a] : B
    . Γ, Δ ⊢ case a′ of !a → b[a] : B

### ⊸-I

    Γ, ⟨a : A⟩ ⊢ b[a] : B
    . Γ ⊢ λa . b[a] : A ⊸ B

### ⊸-E

    Γ ⊢ f : A ⊸ B
    Δ ⊢ a : A
    . Γ, Δ ⊢ f(a) : B

### ⊗-I

    Γ ⊢ a : A
    Δ ⊢ b : B
    . Γ, Δ ⊢ (a, b) : A ⊗ B

### ⊗-E

    Γ ⊢ t : A ⊗ B
    Δ, ⟨a : A⟩, ⟨b : B⟩ ⊢ c[a, b] : C
    . Γ, Δ ⊢ case t of (a, b) → c[a, b] : C

### &-I

    Γ ⊢ a : A
    Γ ⊢ b : B
    . Γ ⊢ ⟨a, b⟩ : A & B

### &-E1

    Γ ⊢ t : A & B
    . Γ ⊢ fst(t) : A

### &-E2

    Γ ⊢ t : A & B
    . Γ ⊢ snd(t) : B

### ⊕-I1

    Γ ⊢ a : A
    . Γ ⊢ inl(a) : A ⊕ B

### ⊕-I2

    Γ ⊢ b : B
    . Γ ⊢ inr(b) : A ⊕ B

### ⊕-E

    Γ ⊢ t : A ⊕ B
    Δ, ⟨a : A⟩ ⊢ c[a] : C
    Δ, ⟨b : B⟩ ⊢ c′[b] : C
    . Γ, Δ ⊢ case t of { inl(a) → c[a]; inr(b) → c′[b] } : C
