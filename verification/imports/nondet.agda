-- Some basic structures and operation for dealing with non-deterministic values

module nondet where

open import bool
open import nat
open import list

infixr 8 _??_

----------------------------------------------------------------------
-- A tree datatype to represent non-deterministic value of some type.
-- It is either a value, a failure, or a choice between
-- non-deterministic values.
----------------------------------------------------------------------

data ND (A : Set) : Set where
  Val  : A → ND A
  Fail : ND A
  _??_ : ND A → ND A → ND A

-- count the number of values:
#vals : {A : Set} → ND A → ℕ
#vals (Val _) = 1
#vals Fail = 0
#vals (t1 ?? t2) = #vals t1 + #vals t2

-- extract the list of all values:
vals-of : {A : Set} → ND A → 𝕃 A
vals-of (Val v)    = v :: []
vals-of Fail       = []
vals-of (t1 ?? t2) = vals-of t1 ++ vals-of t2

-- all values in a Boolean tree are true
always : ND 𝔹 → 𝔹
always Fail       = tt
always (Val b)    = b
always (t1 ?? t2) = always t1 && always t2

-- There exists some true value in a Boolean tree:
eventually : ND 𝔹 → 𝔹
eventually Fail       = ff
eventually (Val b)    = b
eventually (t1 ?? t2) = eventually t1 || eventually t2

-- all non-deterministic values satisfy a predicate:
_satisfy_ : {A : Set} → ND A → (A → 𝔹) → 𝔹
_satisfy_ Fail       _ = tt
_satisfy_ (Val n)    p = p n
_satisfy_ (t1 ?? t2) p = _satisfy_ t1 p && _satisfy_ t2 p

-- There exists at least one value:
exists : {A : Set} → ND A → 𝔹
exists Fail = ff
exists (Val n) = tt
exists (t1 ?? t2) = exists t1 || exists t2

-- There is not value:
failing : {A : Set} → ND A → 𝔹
failing Fail       = tt
failing (Val _)    = ff
failing (t1 ?? t2) = failing t1 && failing t2

-- every value in a tree is equal to 1st arg
every : {A : Set} → (eq : A → A → 𝔹) → A → ND A → 𝔹
every eq x xs = xs satisfy (eq x)

-- map a function on non-deterministic values:
mapND : {A B : Set} → (A → B) → ND A → ND B
mapND f (Val xs) = Val (f xs)
mapND f Fail = Fail
mapND f (t1 ?? t2) = mapND f t1 ?? mapND f t2

-- extend first argument to nd one:
with-nd-arg : {A B : Set} → (A → ND B) → ND A → ND B
with-nd-arg f (Val x)    = f x
with-nd-arg f Fail       = Fail
with-nd-arg f (t1 ?? t2) = with-nd-arg f t1 ?? with-nd-arg f t2

-- extend first argument of a binary function to nd one:
with-nd-arg2 : {A B C : Set} → (A → B → ND C) → ND A → B → ND C
with-nd-arg2 f (Val x)    y = f x y
with-nd-arg2 f Fail       _ = Fail
with-nd-arg2 f (t1 ?? t2) y = with-nd-arg2 f t1 y ?? with-nd-arg2 f t2 y

-- extend first argument of a ternary function to nd one:
with-nd-arg3 : {A B C D : Set} → (A → B → C → ND D)
                               → ND A → B → C → ND D
with-nd-arg3 f (Val x)    y z = f x y z
with-nd-arg3 f Fail       _ _ = Fail
with-nd-arg3 f (t1 ?? t2) y z = with-nd-arg3 f t1 y z ?? with-nd-arg3 f t2 y z

-- extend a deterministic function to one with non-deterministic result:
toND : {A B : Set} → (A → B) → A → ND B
toND f x = Val (f x)

-- extend a deterministic function to a non-deterministic one:
det-to-nd : {A B : Set} → (A → B) → ND A → ND B
det-to-nd f = with-nd-arg (toND f)

----------------------------------------------------------------------
