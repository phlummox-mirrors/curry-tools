module Unification
  ( module UnificationSpec
  , unify
  ) where

import UnificationSpec
  ( VarIdx, Term (..), TermEq, TermEqs, Subst, UnificationError (..)
  , showSubst, emptySubst, extendSubst, lookupSubst )

import FiniteMap
import List (mapAccumL)

--- An RTerm is the unification algorithm's internal term representation.
--- Its RTermCons and RTermVar constructors are similar to the TermCons
--- and TermVar constructors of the original Term data type, but it has
--- an additional Ref constructor. This Ref constructor is used to
--- represent references into a RefTable.
data RTerm
  = RTermCons String [RTerm]
  | RTermVar VarIdx
  | Ref VarIdx

--- Type of the reference table used to store the values referenced
--- by Ref RTerms.
type RefTable = FM Int RTerm

--- An equation of RTerms.
type REq = (RTerm, RTerm)

--- A list of equations of RTerms.
type REqs = [REq]

--- Unifies the given equations.
---
--- @param eqs - the equations to unify
--- @return either an UnificationError or a substitution
unify :: TermEqs -> Either UnificationError Subst
unify ts = let (r, rts) = termEqsToREqs ts in case unify' r rts [] of
  Right (r', ts') -> Right (eqsToSubst r' ts')
  Left err        -> Left err

-- ---------------------------------------------------------------------------
-- conversion to internal structure
-- ---------------------------------------------------------------------------

--- Converts a list of equations of terms into a list of equations
--- of RTerms. Places references into a fresh RefTable.
---
--- @param eqs - the equations to convert
--- @return a tuple of the newly created RefTable and the converted
--- equations
termEqsToREqs :: TermEqs -> (RefTable, REqs)
termEqsToREqs l = mapAccumL termEqToREq (emptyFM (<)) l

--- Converts an equation of terms into an equation of RTerms.
---
--- @param tab - the RefTable to use for storing references
--- @param eq - the equation to convert
--- @return a tuple of the RefTable and the converted equation
termEqToREq :: RefTable -> TermEq -> (RefTable, REq)
termEqToREq r (a, b) = let (r1, a') = termToRTerm r  a
                           (r2, b') = termToRTerm r1 b
                       in  (r2, (a', b'))

--- Converts a Term to an RTerm, placing all TermVars in the given RefTable
--- and replacing them by references inside the result RTerm.
---
--- @param tab - the RefTable to place references in
--- @param term - the Term to convert to RTerm
--- @return a tuple of the RefTable and the converted RTerm
termToRTerm :: RefTable -> Term -> (RefTable, RTerm)
termToRTerm r (TermVar    i) = (addToFM r i (RTermVar i), Ref i)
termToRTerm r (TermCons n l) = let (r', l') = mapAccumL termToRTerm r l
                               in  (r', RTermCons n l')

-- ---------------------------------------------------------------------------
-- conversion from internal structure
-- ---------------------------------------------------------------------------

--- Converts a list of term equations to a substitution
--- by turning every equation of the form (TermVar n, a) or (a, TermVar n)
--- into a mapping (n, a).
--- Equations that do not have a TermVar on either side are ignored.
--- Works on RTerms, dereferences all Refs.
---
--- @param eqs - the equations to convert
--- @return the resulting substitution
eqsToSubst :: RefTable -> REqs -> Subst
eqsToSubst _ []            = emptySubst
eqsToSubst r ((a, b) : ts) = case a of
  Ref _         -> eqsToSubst r ((deref r a, b) : ts)
  RTermVar n    -> extendSubst (eqsToSubst r ts) n (rTermToTerm r b)
  RTermCons _ _ -> case b of
    RTermVar n  -> extendSubst (eqsToSubst r ts) n (rTermToTerm r a)
    Ref _       -> eqsToSubst r ((a, deref r b) : ts)
    _           -> eqsToSubst r ts

--- Converts an RTerm to a Term, dereferencing all references inside
--- the RTerm.
---
--- @param tab - the RefTable to use for reference lookups
--- @param term - the RTerm to convert to Term
--- @return the converted Term
rTermToTerm :: RefTable -> RTerm -> Term
rTermToTerm r i@(Ref       _) = rTermToTerm r (deref r i)
rTermToTerm _ (RTermVar    n) = TermVar n
rTermToTerm r (RTermCons n l) = TermCons n (map (rTermToTerm r) l)

--- Dereferences an RTerm, following chained references.
--- Simply returns the same value for RTermVar and RTermCons.
---
--- @param tab - the RefTable to use for reference lookups
--- @param t - the RTerm to dereference
--- @return the dereferenced RTerm
deref :: RefTable -> RTerm -> RTerm
deref t (Ref           i) = case lookupFM t i of
  Nothing -> error $ "Unification.deref: " ++ show i
  Just a  -> case a of
    RTermVar _    -> a
    RTermCons _ _ -> a
    Ref _         -> deref t a
deref _ a@(RTermVar    _) = a
deref _ a@(RTermCons _ _) = a

-- ---------------------------------------------------------------------------
-- unification algorithm
-- ---------------------------------------------------------------------------

--- Internal unification function, the core of the algorithm.
unify' :: RefTable -> REqs -> REqs -> Either UnificationError (RefTable, REqs)
-- No equations left, we are done.
unify' r []                                    s = Right (r, s)
-- Substitute the constructor for the variable.
unify' r (((RTermVar i), b@(RTermCons _ _)):e) s = elim r i b e s
-- Substitute the constructor for the variable.
unify' r ((a@(RTermCons _ _), (RTermVar i)):e) s = elim r i a e s
-- If both vars are equal, simply remove the equation. Otherwise substitute
-- the second var for the first var.
unify' r ((RTermVar i, b@(RTermVar i')):e)     s | i == i'   = unify' r e s
                                                 | otherwise = elim r i b e s
-- If both constructors have the same name, build equations between their arguments.
-- Otherwise fail with clash.
unify' r ((a@(RTermCons aname as), b@(RTermCons bname bs)):e) s
  | aname == bname = unify' r ((zip as bs) ++ e) s
  | otherwise      = Left (Clash (rTermToTerm r a) (rTermToTerm r b))
-- If we encounter a Ref, simply dereference it and try again.
unify' r ((a@(Ref _), b@(RTermVar _)):e)       s = unify' r ((deref r a, b):e) s
unify' r ((a@(Ref _), b@(RTermCons _ _)):e)    s = unify' r ((deref r a, b):e) s
unify' r ((a@(Ref _), b@(Ref _)):e)            s = unify' r ((deref r a, deref r b):e) s
unify' r ((a@(RTermVar _), b@(Ref _)):e)       s = unify' r ((a, deref r b):e) s
unify' r ((a@(RTermCons _ _), b@(Ref _)):e)    s = unify' r ((a, deref r b):e) s

--- Substitutes a term for a variable inside the list of equations
--- that have yet to be unified and the right-hand sides of all
--- equations of the result list. Also adds a mapping from that
--- variable to that term to the result list.
elim :: RefTable -> VarIdx -> RTerm -> REqs -> REqs
     -> Either UnificationError (RefTable, REqs)
elim r i t e s
  | dependsOn r (RTermVar i) t
  = Left (OccurCheck i (rTermToTerm r t))
  | otherwise = case t of
    -- Make sure to place a Ref in the RefTable and Subst, not the RTermVar
    -- itself.
    RTermVar i'   -> unify' (addToFM r i (Ref i')) e ((RTermVar i, Ref i'):s)
    RTermCons _ _ -> unify' (addToFM r i t       ) e ((RTermVar i, t)     :s)

--- Checks wether the first term occurs as a subterm of the second term.
--- @param a - the term to search for
--- @param b - the term to search
--- @return whether the first term is found in the second term
dependsOn :: RefTable -> RTerm -> RTerm -> Bool
dependsOn t a b = a /= b && dependsOnRecurse b
 where
  dependsOnRecurse v@(RTermVar   _) = a == v
  dependsOnRecurse (RTermCons _ vs) = or (map dependsOnRecurse vs)
  dependsOnRecurse r@(Ref        _) = deref t r == a
