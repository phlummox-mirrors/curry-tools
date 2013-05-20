------------------------------------------------------------------------------
--- Determinism analysis:
--- checks whether functions are deterministic or nondeterministic, i.e.,
--- whether its evaluation on ground argument terms might cause
--- different computation paths.
---
--- @author Michael Hanus
--- @version May 2013
------------------------------------------------------------------------------

module Deterministic(overlapAnalysis,showOverlap,showDet,
                     Deterministic(..),nondetAnalysis) where

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
orInExpr (Typed e _) = orInExpr e

-- Show overlapping information as a string.
showOverlap :: AOutFormat -> Bool -> String
showOverlap _     True  = "overlapping"
showOverlap AText False = "non-overlapping" 
showOverlap ANote False = ""

------------------------------------------------------------------------------
-- The determinism analysis is a global function dependency analysis.
-- It assigns to a function a flag which indicates whether is function
-- might be non-deterministic, i.e., might reduce in different ways
-- for given ground arguments.

--- Data type to represent determinism information.
data Deterministic = NDet | Det

-- Show determinism information as a string.
showDet :: AOutFormat -> Deterministic -> String
showDet _     NDet = "nondeterministic"
showDet AText Det  = "deterministic" 
showDet ANote Det  = ""

nondetAnalysis :: Analysis Deterministic
nondetAnalysis = dependencyFuncAnalysis "Deterministic" Det nondetFunc

-- An operation is non-deterministic if its definition is potentially
-- non-deterministic or it depends on a non-deterministic function.
nondetFunc :: FuncDecl -> [(QName,Deterministic)] -> Deterministic
nondetFunc func calledFuncs =
  if isNondetDefined func || any (==NDet) (map snd calledFuncs)
  then NDet
  else Det

-- Is a function f defined to be potentially non-deterministic, i.e.,
-- is the rule non-deterministic or does it contain extra variables?
isNondetDefined :: FuncDecl -> Bool
isNondetDefined (Func f _ _ _ rule) =
  f `notElem` (map pre ["failed","$!!","$##","normalForm","groundNormalForm"])
  -- these operations are internally defined in PAKCS with extra variables
  && isNondetRule rule

isNondetRule (Rule _ e) = orInExpr e || extraVarInExpr e
isNondetRule (External _) = False


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
extraVarInExpr (Typed e _) = extraVarInExpr e

pre n = ("Prelude",n)

------------------------------------------------------------------------------
