------------------------------------------------------------------------------
--- RootReplaced analysis:
--- This analysis returns for each function f all functions to which this can
--- be replaced at the root. For instance, if there are the definitions:
---
---     f x = g x
---     g x = h x
---     h x = k x : []
---
--- then the root replacements of f are [g,h].
---
--- This analysis could be useful to detect simple loops, e.g., if
--- a function is in its root replacement. This is the purpose
--- of the analysis `RootCyclic` which assigns `True` to some
--- operation if this operation might cause a cyclic root replacement.
---
--- @author Michael Hanus
--- @version January 2017
------------------------------------------------------------------------------

module RootReplaced
 where

import Analysis
import FlatCurry.Types
import GenericProgInfo
import List
import Sort(sort)

------------------------------------------------------------------------------
--- Data type to represent root replacement information.
--- Basically, it is the set (represented as a sorted list) of
--- all function names to which a function can be root replaced.
type RootReplaced = [QName]

-- Show root-replacement information as a string.
showRootRepl :: AOutFormat -> RootReplaced -> String
showRootRepl AText []     = "no root replacements"
showRootRepl ANote []     = ""
showRootRepl AText xs@(_:_) =
  "root replacements: " ++ intercalate "," (map (\ (mn,fn) -> mn++"."++fn) xs)
showRootRepl ANote xs@(_:_) = "[" ++ intercalate "," (map snd xs) ++ "]"

--- Root replacement analysis.
rootReplAnalysis :: Analysis RootReplaced
rootReplAnalysis = dependencyFuncAnalysis "RootReplaced" [] rrFunc

rrFunc :: FuncDecl -> [(QName,RootReplaced)] -> RootReplaced
rrFunc (Func _ _ _ _ rule) calledFuncs = rrFuncRule calledFuncs rule

rrFuncRule :: [(QName,RootReplaced)] -> Rule -> RootReplaced
rrFuncRule _ (External _) = [] -- nothing known about external functions
rrFuncRule calledFuncs (Rule _ rhs) = rrOfExp rhs
 where
  rrOfExp exp = case exp of
    Var _ -> []
    Lit _ -> []
    Comb ct g _ ->
      if ct == FuncCall
       then maybe (error $ "Abstract value of " ++ show g ++ " not found!")
                  (\grrs -> if g `elem` grrs then grrs
                                             else insertBy (<=) g grrs)
                  (lookup g calledFuncs)
       else []
    Typed e  _  -> rrOfExp e
    Free  _  e  -> rrOfExp e
    Let   _  e  -> rrOfExp e
    Or    e1 e2 -> sort (union (rrOfExp e1) (rrOfExp e2))
    Case _ e bs -> sort (foldr union (rrOfExp e)
                               (map (\ (Branch _ be) -> rrOfExp be) bs))

------------------------------------------------------------------------------
-- Show root-cyclic information as a string.
showRootCyclic :: AOutFormat -> Bool -> String
showRootCyclic AText False = "no cycles at the root"
showRootCyclic ANote False = ""
showRootCyclic AText True  = "possible cyclic root replacement"
showRootCyclic ANote True  = "root-cyclic"

--- Root cyclic analysis.
rootCyclicAnalysis :: Analysis Bool
rootCyclicAnalysis =
  combinedSimpleFuncAnalysis "RootCyclic" rootReplAnalysis rcFunc

rcFunc :: ProgInfo RootReplaced -> FuncDecl -> Bool
-- we assume that external functions are not root cyclic:
rcFunc _ (Func _  _ _ _ (External _)) = False
-- otherwise we check whether the operation is in its set of root replacements:
rcFunc rrinfo (Func qf _ _ _ (Rule _ _)) =
  maybe True -- no information, but this case should not occur
        (\rrfuncs -> qf `elem` rrfuncs -- direct cycle
                     -- or cycle in some root-replacement:
                  || any (\rrf -> maybe True
                                        (rrf  `elem`)
                                        (lookupProgInfo rrf rrinfo))
                         rrfuncs)
        (lookupProgInfo qf rrinfo)

------------------------------------------------------------------------------
