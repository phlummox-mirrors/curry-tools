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
--- This analysis could be useful to detect simple loops, e.g., if
--- a function is in its root replacement.
---
--- @author Michael Hanus
--- @version June 2016
------------------------------------------------------------------------------

module RootReplaced
 where

import Analysis
import FlatCurry.Types
import List
import Sort(sort)

------------------------------------------------------------------------------
--- Data type to represent root replacement information.
--- Basically, it is the set (represented as a sorted list) of
--- all function names to which a function can be root replaced.
type RootReplaced = [QName]

-- Show determinism information as a string.
showRootRepl :: AOutFormat -> RootReplaced -> String
showRootRepl AText []     = "no root replacements"
showRootRepl ANote []     = ""
showRootRepl fmt (x:xs) =
  (if fmt==AText then "root replacements: " else "") ++
  intercalate "," (map (\ (mn,fn) -> mn++"."++fn) (x:xs))

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
