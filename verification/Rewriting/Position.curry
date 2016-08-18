------------------------------------------------------------------------------
--- Library for representation of positions in first-order terms.
---
--- @author Jan-Hendrik Matthes
--- @version May 2016
--- @category algorithm
------------------------------------------------------------------------------

module Rewriting.Position
  ( Pos
  , eps, showPos, isPosAbove, isPosBelow, isPosLeft, isPosRight
  , isPosDisjunct, positions, (|>), replaceTerm
  ) where

import List (intercalate, isPrefixOf)
import Rewriting.Term (Term (..))

-- ---------------------------------------------------------------------------
-- Representation of positions in first-order terms
-- ---------------------------------------------------------------------------

--- A position in a term represented as a list of integers greater than zero.
type Pos = [Int]

--- The root position of a term.
eps :: Pos
eps = []

-- ---------------------------------------------------------------------------
-- Pretty-printing of positions in first-order terms
-- ---------------------------------------------------------------------------

-- \x00b7 = MIDDLE DOT
-- \x03b5 = GREEK SMALL LETTER EPSILON

--- Transforms a position into a string representation.
showPos :: Pos -> String
showPos []       = "\x03b5"
showPos ps@(_:_) = intercalate "\x00b7" (map show ps)

-- ---------------------------------------------------------------------------
-- Functions for positions in first-order terms
-- ---------------------------------------------------------------------------

--- Checks whether the first position is above the second position.
isPosAbove :: Pos -> Pos -> Bool
isPosAbove = isPrefixOf

--- Checks whether the first position is below the second position.
isPosBelow :: Pos -> Pos -> Bool
isPosBelow = flip isPrefixOf

--- Checks whether the first position is left from the second position.
isPosLeft :: Pos -> Pos -> Bool
isPosLeft []     _      = False
isPosLeft (_:_)  []     = False
isPosLeft (p:ps) (q:qs) = case compare p q of
                            LT -> True
                            EQ -> isPosLeft ps qs
                            GT -> False

--- Checks whether the first position is right from the second position.
isPosRight :: Pos -> Pos -> Bool
isPosRight []     _      = False
isPosRight (_:_)  []     = False
isPosRight (p:ps) (q:qs) = case compare p q of
                             LT -> False
                             EQ -> isPosRight ps qs
                             GT -> True

--- Checks whether the first position is disjunct from the second position.
isPosDisjunct :: Pos -> Pos -> Bool
isPosDisjunct p q = not (isPosAbove p q || isPosAbove q p)

--- Returns a list of all positions in a term.
positions :: Term _ -> [Pos]
positions (TermVar _)     = [eps]
positions (TermCons _ ts) = eps:[i:p | (i, t) <- zip [1..] ts,
                                       p <- positions t]

-- ---------------------------------------------------------------------------
-- Subterms and term replacement
-- ---------------------------------------------------------------------------

--- Selects the subterm at the given position.
(|>) :: Term f -> Pos -> Term f
t               |> []                              = t
(TermCons _ ts) |> (i:p) | i > 0 && i <= length ts = (ts !! (i - 1)) |> p

--- Replaces the subterm at the given position with the given term.
---
--- @param term  - The term with the subterm to replace.
--- @param pos   - The position of the subterm.
--- @param rterm - The new subterm.
--- @return The term with the new subterm.
replaceTerm :: Term f -> Pos -> Term f -> Term f
replaceTerm _               []    s = s
replaceTerm (TermCons c ts) (i:p) s
  | i > 0 && i <= length ts = let (ts1, ti:ts2) = splitAt (i - 1) ts
                               in TermCons c (ts1 ++ (replaceTerm ti p s):ts2)