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
  , showVarIdx, showTerm, showTermEq, showTermEqs, tConst, tRoot, tCons, tVars
  , tVarsL, isConsTerm, isVarTerm, isGround, isLinear, mapTerm, renameTVars
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
--- @cons TermVar v     - The variable with index `v`.
--- @cons TermCons c ts - The constructor with constructor `c` and argument
---                       terms `ts`.
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
showTerm :: (f -> String) -> Term f -> String
showTerm _ (TermVar v)     = showVarIdx v
showTerm s (TermCons c ts) = case ts of
                               []    -> s c
                               (_:_) -> (s c) ++ "(" ++ args ++ ")"
  where
    args = intercalate ", " (map (showTerm s) ts)

--- Transforms a term equation into a string representation.
showTermEq :: (f -> String) -> TermEq f -> String
showTermEq s (l, r) = (showTerm s l) ++ " = " ++ (showTerm s r)

--- Transforms multiple term equations into a string representation.
showTermEqs :: (f -> String) -> TermEqs f -> String
showTermEqs s eqs = unlines (map (showTermEq s) eqs)

-- ---------------------------------------------------------------------------
-- Functions for first-order terms
-- ---------------------------------------------------------------------------

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
tVars = nub . tVarsL

--- Returns a list of all variables in a term.
tVarsL :: Term _ -> [VarIdx]
tVarsL (TermVar v)     = [v]
tVarsL (TermCons _ ts) = concatMap tVarsL ts

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
isLinear = unique . tVarsL

--- Transforms a term by applying a transformation on all function symbols.
mapTerm :: (a -> b) -> Term a -> Term b
mapTerm _ (TermVar v)     = TermVar v
mapTerm f (TermCons c ts) = TermCons (f c) (map (mapTerm f) ts)

--- Increases the variables in a term by the given number.
renameTVars :: Int -> Term f -> Term f
renameTVars i (TermVar v)     = TermVar (v + i)
renameTVars i (TermCons c ts) = TermCons c (map (renameTVars i) ts)

-- ---------------------------------------------------------------------------
-- Definition of helper functions
-- ---------------------------------------------------------------------------

--- Checks whether a list contains no element more than once.
unique :: [_] -> Bool
unique []                    = True
unique (x:xs) | notElem x xs = unique xs
              | otherwise    = False