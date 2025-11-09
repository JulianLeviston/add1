module Add1.StoreAlgebra where

open import Level
open import Relation.Binary.PropositionalEquality using (_≡_)

record RawStore {ℓ : Level} (A : Set ℓ) : Set (suc ℓ) where
  field
    Store : Set ℓ
    empty : Store
    insert : A → Store → Store
    fold : ∀ {B : Set ℓ} → (A → B → B) → B → Store → B

record IsStore {ℓ : Level} {A : Set ℓ} (raw : RawStore A) : Set (suc ℓ) where
  open RawStore raw
  field
    fold-empty : ∀ {B : Set ℓ} (f : A → B → B) (z : B) → fold f z empty ≡ z

module MakeStore {ℓ : Level} {A : Set ℓ}
                 (Container : Set ℓ → Set ℓ)
                 (empty-impl : Container A)
                 (insert-impl : A → Container A → Container A)
                 (fold-impl : ∀ {B : Set ℓ} → (A → B → B) → B → Container A → B) where
  storeOps : RawStore A
  storeOps = record
    { Store = Container A
    ; empty = empty-impl
    ; insert = insert-impl
    ; fold = fold-impl
    }

record StoreBundle {ℓ : Level} (A : Set ℓ) : Set (suc ℓ) where
  field
    rawStore : RawStore A
    isStore : IsStore rawStore

  open RawStore rawStore public
  open IsStore isStore public
