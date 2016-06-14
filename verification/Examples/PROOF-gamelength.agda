-- Agda program using the Iowa Agda library
{-# OPTIONS --termination-depth=2 #-}

module PROOF-gamelength
  where

open import eq
open import bool
open import nat
open import nat-thms
open import list
open import nondet
open import nondet-thms

---------------------------------------------------------------------------
-- Translated Curry operations:

data Move : Set where
   L : Move
   R : Move

solve2 : ℕ → ℕ → ND (𝕃 Move)
solve2 zero    zero    = Val []
solve2 (suc x) zero    = mapND (_::_ L) (solve2 x zero)
solve2 zero    (suc y) = mapND (_::_ R) (solve2 zero y)
solve2 (suc z) (suc u) = (mapND (_::_ L) (solve2 z (suc u)))
                      ?? (mapND (_::_ R) (solve2 (suc z) u))

len : {a : Set} → 𝕃 a → ℕ
len [] = zero
len (x :: y) = suc (len y)

---------------------------------------------------------------------------

-- Theorem: the length of every solution is the sum of the input arguments
theorem'gamelength : (x : ℕ) → (y : ℕ)
              → (solve2 x y) satisfy (λ xs → length xs =ℕ x + y) ≡ tt
theorem'gamelength zero zero = refl
theorem'gamelength zero (suc y)
 rewrite
    satisfy-mapND (_::_ R) (solve2 zero y) (λ xs → length xs =ℕ zero + suc y)
  | theorem'gamelength zero y = refl
theorem'gamelength (suc x) zero
 rewrite 
   satisfy-mapND (_::_ L) (solve2 x zero) (λ xs → length xs =ℕ suc x + zero)
 | theorem'gamelength x zero = refl
theorem'gamelength (suc x) (suc y)
 rewrite
  satisfy-mapND (_::_ L) (solve2 x (suc y)) (λ xs → length xs =ℕ suc x + suc y)
 | satisfy-mapND (_::_ R) (solve2 (suc x) y) (λ xs → length xs =ℕ suc x + suc y)
 | theorem'gamelength x (suc y)
 | +suc x y
 | theorem'gamelength (suc x) y
 = refl

---------------------------------------------------------------------------
