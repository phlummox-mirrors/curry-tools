------------------------------------------------------------------------------
--- Library for representation of first-order terms.
---
--- This library is the basis of other libraries for the manipulation of
--- first-order terms, e.g., unification of terms. Therefore, this library
--- also defines other structures, like term equations.
---
--- @author Jan-Hendrik Matthes
--- @version May 2016
--- @category algorithm
------------------------------------------------------------------------------

module Rewriting.Term
  ( VarIdx, Term (..), TermEq, TermEqs
  , showVarIdx, showTerm, showTermEq, showTermEqs
  , mapTerm, tConst, tRoot, tCons, tVars
  , isConsTerm, isVarTerm, isGround, isLinear
  ) where

import List (intercalate, nub)

-- ---------------------------------------------------------------------------
-- Representation of first-order terms and term equations
-- ---------------------------------------------------------------------------

--- A variable represented as an integer greater than or equal to zero.
type VarIdx = Int

--- Representation of a first-order term, parameterized over the kind of
--- function symbols, e.g., strings.
---
--- @cons TermVar i          - The variable with index `i`.
--- @cons TermCons name args - The constructor with constructor `name` and
---                            argument terms `args`.
data Term f = TermVar VarIdx | TermCons f [Term f]

--- A term equation represented as a pair of terms and parameterized over the
--- kind of function symbols, e.g., strings.
type TermEq f = (Term f, Term f)

--- Multiple term equations represented as a list of term equations and
--- parameterized over the kind of function symbols, e.g., strings.
type TermEqs f = [TermEq f]

-- ---------------------------------------------------------------------------
-- Pretty-printing of first-order terms and term equations
-- ---------------------------------------------------------------------------

--- Transforms a variable into a string representation.
showVarIdx :: VarIdx -> String
showVarIdx v | v >= 0 = if q == 0
                          then [c]
                          else c:(show q)
  where
    (q, r) = divMod v 26
    c = "abcdefghijklmnopqrstuvwxyz" !! r

--- Transforms a term into a string representation.
showTerm :: Term String -> String
showTerm (TermVar v)     = showVarIdx v
showTerm (TermCons c ts) = case ts of
                             []    -> c
                             (_:_) -> c ++ "(" ++ args ++ ")"
  where
    args = intercalate ", " (map showTerm ts)

--- Transforms a term equation into a string representation.
showTermEq :: TermEq String -> String
showTermEq (l, r) = showTerm l ++ " = " ++ showTerm r

--- Transforms multiple term equations into a string representation.
showTermEqs :: TermEqs String -> String
showTermEqs eqs = unlines (map showTermEq eqs)

-- ---------------------------------------------------------------------------
-- Functions for first-order terms
-- ---------------------------------------------------------------------------

--- Transforms a term by applying a transformation on all function symbols.
mapTerm :: (a -> b) -> Term a -> Term b
mapTerm _ (TermVar v)       = TermVar v
mapTerm m (TermCons f args) = TermCons (m f) (map (mapTerm m) args)

--- Returns a term with the given constructor and no argument terms.
tConst :: f -> Term f
tConst c = TermCons c []

--- Returns the root symbol (variable or constructor) of a term.
tRoot :: Term f -> Either VarIdx f
tRoot (TermVar v)    = Left v
tRoot (TermCons c _) = Right c

--- Returns all constructors in a term.
tCons :: Term f -> [f]
tCons (TermVar _)     = []
tCons (TermCons c ts) = nub (c:(concatMap tCons ts))

--- Returns all variables in a term.
tVars :: Term _ -> [VarIdx]
tVars (TermVar v)     = [v]
tVars (TermCons _ ts) = nub (concatMap tVars ts)

--- Checks whether a term is a constructor.
isConsTerm :: Term _ -> Bool
isConsTerm (TermVar _)    = False
isConsTerm (TermCons _ _) = True

--- Checks whether a term is a variable.
isVarTerm :: Term _ -> Bool
isVarTerm (TermVar _)    = True
isVarTerm (TermCons _ _) = False

--- Checks whether a term is a ground term (contains no variables).
isGround :: Term _ -> Bool
isGround = null . tVars

--- Checks whether a term is linear (contains no variable more than once).
isLinear :: Term _ -> Bool
isLinear = unique . vars
  where
    unique :: [_] -> Bool
    unique []                    = True
    unique (x:xs) | notElem x xs = unique xs
                  | otherwise    = False
    vars :: Term _ -> [VarIdx]
    vars (TermVar v)     = [v]
    vars (TermCons _ ts) = concatMap vars ts