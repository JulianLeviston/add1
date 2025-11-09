module Add1.NumberAlgebra where

open import Level
open import Relation.Binary.PropositionalEquality using (_≡_)

record RawNumberAlgebra (ℓ : Level) : Set (suc ℓ) where
  field
    Number : Set ℓ
    _+_     : Number → Number → Number
    _*_     : Number → Number → Number
    _/_     : Number → Number → Number
    0#      : Number
    1#      : Number


-- Laws only placeholder -- note: commented out because I couldn't get record fields to be able to use infix operators
-- and they're not needed yet. Should add them in, but more to learn first
record IsNumber {ℓ : Level} (N : RawNumberAlgebra ℓ) : Set ℓ where
  -- open RawNumberAlgebra N using (Number; _+_; _*_; 0#; 1#)
  -- infixl 6 _+_
  -- infixl 7 _*_

  -- field
  --   +-assoc    : ∀ x y z → (x + y) + z ≡ x + (y + z)
  --   +-comm     : ∀ x y → x + y ≡ y + x
  --   +-identity : ∀ x → x + 0# ≡ x

  --   *-assoc    : ∀ x y z → (x * y) * z ≡ x * (y * z)
  --   *-comm     : ∀ x y → x * y ≡ y * x
  --   *-identity : ∀ x → x * 1# ≡ x

  --   distrib   : ∀ x y z → x * (y + z) ≡ x * y + x * z
  --   inv-law   : ∀ {x} → x ≠ 0# → x * (x / x) ≡ 1#

-- -- Full Field bundle
-- record Field : Set₁ where
--   field
--     raw     : RawField
--     isNumber : IsField raw

-- -- Now define additive inverse and subtraction
-- module Subtraction (F : RawField) (fieldLaws : IsField F) where
--   open RawField F using (Number; _+_; 0#)
--   open IsField fieldLaws using (+-identity)

--   -- Postulate additive inverse (can also derive if we like)
--   postulate
--     neg : Number → Number
--     +-inverse : ∀ x → x + neg x ≡ 0#

--   _-_ : Number → Number → Number
--   x - y = x + neg y

record NumberAlgebra (ℓ : Level) : Set (suc ℓ) where
  field
    rawNum : RawNumberAlgebra ℓ
    isNum  : IsNumber rawNum

sumIdentity : {ℓ : Level} {N : NumberAlgebra ℓ} → RawNumberAlgebra.Number (NumberAlgebra.rawNum N)
sumIdentity {N = N} = RawNumberAlgebra.0# (NumberAlgebra.rawNum N)

