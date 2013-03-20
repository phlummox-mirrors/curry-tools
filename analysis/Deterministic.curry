------------------------------------------------------------------------------
--- Determinism analysis:
--- checks whether functions are deterministic or nondeterministic, i.e.,
--- whether its evaluation on ground argument terms might cause
--- different computation paths.
---
--- @author Michael Hanus
--- @version March 2013
------------------------------------------------------------------------------

module Deterministic(overlapAnalysis,showOverlap,showDet,showSetValued,
                     Deterministic(..),ndAnalysis,setValAnalysis) where

import Analysis
import FlatCurry
import FlatCurryGoodies

------------------------------------------------------------------------------
-- The overlapping analysis can be applied to individual functions.
-- It assigns to a FlatCurry function definition a flag which is True
-- if this function is defined with overlapping left-hand sides.

overlapAnalysis :: Analysis Bool
overlapAnalysis = simpleFuncAnalysis "Overlapping" isOverlappingFunction

isOverlappingFunction :: FuncDecl -> Bool
isOverlappingFunction (Func _ _ _ _ (Rule _ e))   = orInExpr e
isOverlappingFunction (Func f _ _ _ (External _)) = f==("Prelude","?")

-- Check an expression for occurrences of OR:
orInExpr :: Expr -> Bool
orInExpr (Var _) = False
orInExpr (Lit _) = False
orInExpr (Comb _ f es) = f==(pre "?") || any orInExpr es
orInExpr (Free _ e) = orInExpr e
orInExpr (Let bs e) = any orInExpr (map snd bs) || orInExpr e
orInExpr (Or _ _) = True
orInExpr (Case _ e bs) = orInExpr e || any orInBranch bs
                   where orInBranch (Branch _ be) = orInExpr be

-- Show overlapping information as a string.
showOverlap :: Bool -> String
showOverlap True  = "overlapping"
showOverlap False = "non-overlapping" 

------------------------------------------------------------------------------
--- Data type to represent determinism information.
data Deterministic = NDet | Det

--- Determinism analysis.
ndAnalysis :: Analysis Deterministic
ndAnalysis = dependencyFuncAnalysis "Deterministic" Det ndFunc

-- An operation is non-deterministic if it has an overlapping definition.
-- or if it calls a non-deterministic operation.
ndFunc :: FuncDecl -> [(QName,Deterministic)] -> Deterministic
ndFunc fdecl calledFuncs =
  --trace (snd(funcName fdecl)++"...") $
  if isOverlappingFunction fdecl || any (==NDet) (map snd calledFuncs)
  then NDet
  else Det

-- Show determinism information as a string.
showDet :: Deterministic -> String
showDet NDet = "nondeterministic"
showDet Det  = "deterministic" 

------------------------------------------------------------------------------
-- The set-valued analysis is a global function dependency analysis.
-- It assigns to a function a flag which is True if this function
-- might be set-valued, i.e., might reduce to different values
-- for given ground arguments.

setValAnalysis :: Analysis Bool
setValAnalysis = dependencyFuncAnalysis "SetValued" False setValFunc

-- An operation is set-valued if its definition is potentially set-valued
-- or it depends on a set-valued function.
setValFunc :: FuncDecl -> [(QName,Bool)] -> Bool
setValFunc func calledFuncs =
  isSetValuedDefined func || any snd calledFuncs

-- Is a function f defined to be potentially set-valued, i.e., is the rule
-- nondeterministic or does it contain extra variables?
isSetValuedDefined :: FuncDecl -> Bool
isSetValuedDefined (Func f _ _ _ rule) =
  f `notElem` (map pre ["failed","$!!","$##","normalForm","groundNormalForm"])
  -- these operations are internally defined in PAKCS with extra variables
  && isSetValuedRule rule

isSetValuedRule (Rule _ e) = orInExpr e || extraVarInExpr e
isSetValuedRule (External _) = False


-- check an expression for occurrences of extra variables:
extraVarInExpr :: Expr -> Bool
extraVarInExpr (Var _) = False
extraVarInExpr (Lit _) = False
extraVarInExpr (Comb _ _ es) = or (map extraVarInExpr es)
extraVarInExpr (Free vars e) = (not (null vars)) || extraVarInExpr e
extraVarInExpr (Let bs e) = any extraVarInExpr (map snd bs) || extraVarInExpr e
extraVarInExpr (Or e1 e2) = extraVarInExpr e1 || extraVarInExpr e2
extraVarInExpr (Case _  e bs) = extraVarInExpr e || any extraVarInBranch bs
                where extraVarInBranch (Branch _ be) = extraVarInExpr be

-- Show set-valued information as a string.
showSetValued :: Bool -> String
showSetValued True  = "set-valued"
showSetValued False = "single-valued" 


pre n = ("Prelude",n)

------------------------------------------------------------------------------
