------------------------------------------------------------------------------
--- Library for representation and computation of critical pairs.
---
--- @author Jan-Hendrik Matthes
--- @version May 2016
--- @category algorithm
------------------------------------------------------------------------------

module Rewriting.CriticalPairs
  ( CPair
  , showCPair, cPairs
  ) where

import Either (rights)
import List (maximum, nub)
import Rewriting.Position (eps, positions, (|>), replaceTerm)
import Rewriting.Rules (TRS, rVars, isVariantOf, renameRVars)
import Rewriting.Substitution (applySubst)
import Rewriting.Term (Term, showTerm, isVarTerm)
import Rewriting.Unification (unify)

-- ---------------------------------------------------------------------------
-- Representation of critical pairs
-- ---------------------------------------------------------------------------

--- A critical pair represented as a pair of terms and parameterized over the
--- kind of function symbols, e.g., strings.
type CPair f = (Term f, Term f)

-- ---------------------------------------------------------------------------
-- Pretty-printing of critical pairs
-- ---------------------------------------------------------------------------

-- \x3008 = LEFT ANGLE BRACKET
-- \x3009 = RIGHT ANGLE BRACKET

--- Transforms a critical pair into a string representation.
showCPair :: (f -> String) -> CPair f -> String
showCPair s (l, r)
  = "\x3008" ++ (showTerm s l) ++ ", " ++ (showTerm s r) ++ "\x3009"

-- ---------------------------------------------------------------------------
-- Computation of critical pairs
-- ---------------------------------------------------------------------------

--- Computes the critical pairs of a term rewriting system.
cPairs :: Eq f => TRS f -> [CPair f]
cPairs trs = nub [(applySubst sub r1,
                   replaceTerm (applySubst sub l1) p (applySubst sub r2)) |
                  rule1@(l1, r1) <- trs,
                  let vMax = (maximum (0:(rVars rule1))) + 1,
                  rule2@(l2, r2) <- map (renameRVars vMax) trs,
                  p <- positions l1,
                  let l1p = l1 |> p, not (isVarTerm l1p),
                  sub <- rights [unify [(l1p, l2)]],
                  p /= eps || not (isVariantOf rule1 rule2)]