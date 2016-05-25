------------------------------------------------------------------------------
--- Library for representation of rules and term rewriting systems.
---
--- @author Jan-Hendrik Matthes
--- @version May 2016
--- @category algorithm
------------------------------------------------------------------------------

module Rewriting.Rules
  ( Rule, TRS
  , showRule, showTRS, rRoot, rCons, rVars, normalise, isVariantOf
  , renameVarsBy
  ) where

import Function (on)
import List (nub)
import Rewriting.Substitution (emptySubst, extendSubst, applySubst)
import Rewriting.Term (VarIdx, Term (..), showTerm, tRoot, tCons, tVars)

-- ---------------------------------------------------------------------------
-- Representation of rules and term rewriting systems
-- ---------------------------------------------------------------------------

--- A rule represented as a pair of terms and parameterized over the kind of
--- function symbols, e.g., strings.
type Rule f = (Term f, Term f)

--- A term rewriting system represented as a list of rules and parameterized
--- over the kind of function symbols, e.g., strings.
type TRS f = [Rule f]

-- ---------------------------------------------------------------------------
-- Pretty-printing of rules and term rewriting systems
-- ---------------------------------------------------------------------------

-- \x2192 = RIGHTWARDS ARROW

--- Transforms a rule into a string representation.
showRule :: Rule String -> String
showRule (l, r) = showTerm l ++ " \x2192 " ++ showTerm r

--- Transforms a term rewriting system into a string representation.
showTRS :: TRS String -> String
showTRS trs = unlines (map showRule trs)

-- ---------------------------------------------------------------------------
-- Functions for rules and term rewriting systems
-- ---------------------------------------------------------------------------

--- Returns the root symbol (variable or constructor) of a rule.
rRoot :: Rule f -> Either VarIdx f
rRoot (l, _) = tRoot l

--- Returns all constructors in a rule.
rCons :: Rule f -> [f]
rCons (l, r) = nub (tCons l ++ tCons r)

--- Returns all variables in a rule.
rVars :: Rule _ -> [VarIdx]
rVars (l, _) = tVars l

--- Normalises a rule by renaming all variables in increasing order.
normalise :: Rule f -> Rule f
normalise rule@(l, r) = (applySubst sigma l, applySubst sigma r)
  where
    vMap = zip (rVars rule) (map TermVar [0..])
    sigma = foldr (\(v, t) subst -> extendSubst subst v t) emptySubst vMap

--- Checks whether rule `a` is a variant of rule `b`.
isVariantOf :: Rule f -> Rule f -> Bool
isVariantOf = on (==) normalise

--- Increases the variables in a rule by the given number.
renameVarsBy :: Int -> Rule f -> Rule f
renameVarsBy i (l, r) = (rename l, rename r)
  where
    rename (TermVar v)     = TermVar (v + i)
    rename (TermCons c ts) = TermCons c (map rename ts)