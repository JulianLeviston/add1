module Add1.Everything where

open import Relation.Binary.PropositionalEquality as Eq
open import Level using (Level; zero; suc)
open Eq.≡-Reasoning
open import Add1.NumberAlgebra using (NumberAlgebra; RawNumberAlgebra; sumIdentity)
open import Add1.StoreAlgebra using (StoreBundle; RawStore; IsStore)


module Theory {ℓ : Level} (N : NumberAlgebra ℓ) where
  open NumberAlgebra N
  open RawNumberAlgebra rawNum

  module WithStore (S : StoreBundle Number) where 
    open StoreBundle S

    sumStore : Store → Number
    sumStore = fold _+_ (sumIdentity {ℓ} {N})
    
    sumStore-empty : sumStore empty ≡ sumIdentity {ℓ} {N}
    sumStore-empty = 
      let
        sumIdentityValue = sumIdentity {ℓ} {N}
      in begin
        sumStore empty
      ≡⟨⟩
        fold _+_ sumIdentityValue empty
      ≡⟨ fold-empty _+_ sumIdentityValue ⟩
        sumIdentityValue
      ∎


