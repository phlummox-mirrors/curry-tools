------------------------------------------------------------------------------
--- Library for general unification - specification
---
--- This library implements a general unification algorithm.
--- Because the algorithm is easy to understand, but rather slow,
--- it serves as a specification for more elaborate implementations.
---
--- @author  Jonas Oberschweiber, Björn Peemöller
--- @version February 2013
------------------------------------------------------------------------------
module UnificationSpec
  ( VarIdx, Term (..), TermEq, TermEqs, Subst, UnificationError (..)
  , showSubst, emptySubst, extendSubst, lookupSubst, applySubst, unify
  ) where

import FiniteMap

-- ---------------------------------------------------------------------------
-- representation of terms
-- ---------------------------------------------------------------------------

--- Variable index, identifying a variable.
type VarIdx = Int

--- Representation of a constructor term.
---
--- @cons TermVar i          - The variable with index `i`
--- @cons TermCons name args - The constructor with constructor `name`
---                            and argument terms `args`
data Term = TermVar VarIdx | TermCons String [Term]

--- The type of an equation.
type TermEq = (Term, Term)

--- The type of multiple equations.
type TermEqs = [TermEq]

-- ---------------------------------------------------------------------------
-- substitution
-- ---------------------------------------------------------------------------

--- The (abstract) data type for substitutions.
type Subst = FM VarIdx Term

showSubst :: Subst -> String
showSubst = unlines . map showOne . fmToList
  where showOne (k, v) = show k ++ " -> " ++ show v

--- The empty substitution
emptySubst :: Subst
emptySubst = emptyFM (<)

--- Extend the substitution with the given mapping.
---
--- @param subst         - the substitution
--- @param index         - the variable which should be mapped
--- @param term          - the term the variable should be mapped to
--- @return                the extended substitution
extendSubst :: Subst -> VarIdx -> Term -> Subst
extendSubst = addToFM

--- Searches the substitution for a mapping from the given variable index
--- to a term.
---
--- @param subst - the substitution to search
--- @param i - the index to search for
--- @return the found term or Nothing
lookupSubst :: Subst -> VarIdx -> Maybe Term
lookupSubst = lookupFM

--- Applies a substitution to a single term.
---
--- @param sub - the substitution to apply
--- @param t - the term to apply the substitution to
--- @return the resulting term
applySubst :: Subst -> Term -> Term
applySubst s t@(TermVar   n) = maybe t id (lookupSubst s n)
applySubst s (TermCons c vs) = TermCons c (map (applySubst s) vs)

--- Applies a substitution to a single equation.
---
--- @param sub - the substitution to apply
--- @param eq - the equation to apply the substitution to
--- @return the resulting equation
substituteSingle :: Subst -> TermEq -> TermEq
substituteSingle s (a, b) = (applySubst s a, applySubst s b)

--- Applies a substitution to a list of equations.
---
--- @param sub - the substitution to apply
--- @param eqs - the equations to apply the substitution to
--- @return the resulting equations
substitute :: Subst -> TermEqs -> TermEqs
substitute s eqs = map (substituteSingle s) eqs

-- ---------------------------------------------------------------------------
-- unification
-- ---------------------------------------------------------------------------

--- The data type for the different kinds of errors that can occur during
--- unification.
---
--- @cons Clash eq      - Two term constructors with different names
---                       are supposed to be equal.
--- @cons OccurCheck eq - A term is supposed to be equal to a term
---                       in which it occurs as a subterm.
data UnificationError = Clash Term Term | OccurCheck VarIdx Term

--- Unifies the given equations.
---
--- @param eqs - the equations to unify
--- @return either an UnificationError or a substitution
unify :: TermEqs -> Either UnificationError Subst
unify ts = either Left (Right . eqsToSubst) (unify' ts [])

eqsToSubst :: TermEqs -> Subst
eqsToSubst []            = emptySubst
eqsToSubst ((a, b) : ts) = case a of
  TermVar n      -> extendSubst (eqsToSubst ts) n b
  TermCons _ _   -> case b of
    TermVar n    -> extendSubst (eqsToSubst ts) n a
    _            -> error $ "eqsToSubst: " ++ show (a, b)

unify' :: TermEqs -> TermEqs -> Either UnificationError TermEqs
unify' s [] = Right s
unify' s (((TermVar      i), b@(TermCons _ _)):e) = elim s i b e
unify' s ((a@(TermCons _ _), (TermVar      i)):e) = elim s i a e
unify' s ((TermVar        i, b@(TermVar   i')):e) | i == i'   = unify' s e
                                                  | otherwise = elim s i b e
unify' s ((a@(TermCons ac as), b@(TermCons bc bs)):e)
  | ac == bc  = unify' s ((zip as bs) ++ e)
  | otherwise = Left (Clash a b)

elim :: TermEqs -> VarIdx -> Term -> TermEqs -> Either UnificationError TermEqs
elim s i t e
  | dependsOn (TermVar i) t = Left (OccurCheck i t)
  | otherwise               = unify' s' (substitute' i t e)
    where s' = (TermVar i, t) : map (\(x, y) -> (x, termSubstitute' i t y)) s

--- Substitutes a variable with the given index by a term inside another term.
---
--- @param i - the index of the variable to substitute
--- @param t - the term to substitute
--- @param subj - the term to substitute in
--- @return the resulting term
termSubstitute' :: VarIdx -> Term -> Term -> Term
termSubstitute' b s v@(TermVar n)
    | n == b    = s
    | otherwise = v
termSubstitute' b s (TermCons t vars) = TermCons t (termsSubstitute' b s vars)

--- Substitute a variable with the given index by a term inside a list of terms.
---
--- @param i - the index of the variable to substitute
--- @param t - the term to substitute
--- @param subjs - the terms to substitute in
--- @return the resulting terms
termsSubstitute' :: VarIdx -> Term -> [Term] -> [Term]
termsSubstitute' i t ts = map (termSubstitute' i t) ts

--- Substitute a variable with the given index by a term inside a list of equations.
---
--- @param i - the index of the variable to substitute
--- @param t - the term to substitute
--- @param eqs - the equations to substitute in
--- @return the resulting equations
substitute' :: VarIdx -> Term -> TermEqs -> TermEqs
substitute' i t ts = map (substituteSingle' i t) ts

--- Substitute a variable with the given index by a term inside a single equation.
---
--- @param i - the index of the variable to substitute
--- @param t - the term to substitute
--- @param eq - the equation to substitute in
--- @return the resulting equation
substituteSingle' :: VarIdx -> Term -> TermEq -> TermEq
substituteSingle' i t (a, b) = (termSubstitute' i t a, termSubstitute' i t b)

--- Checks wether the first term occurs as a subterm of the second term.
--- @param a - the term to search for
--- @param b - the term to search in
--- @return whether the first term is found in the second term
dependsOn :: Term -> Term -> Bool
dependsOn a b = and [(not (a == b)), dependsOnRecurse a b]
 where dependsOnRecurse c v@(TermVar _) = c == v
       dependsOnRecurse c (TermCons _ vars) = any id (map (dependsOnRecurse c) vars)
