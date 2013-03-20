-----------------------------------------------------------------------------
--- Pattern completeness and totally definedness analysis for Curry programs
---
--- This analysis checks for each function in a Curry program  whether
--- this function is completely defined, i.e., reducible on all ground
--- constructor terms
---
--- @author Johannes Koj, Michael Hanus
--- @version March 2013
-----------------------------------------------------------------------------

module TotallyDefined(siblingCons,
                      Completeness(..),showComplete,showTotally,
                      patCompAnalysis,totalAnalysis) where

import Analysis
import FlatCurry
import FlatCurryGoodies
import GenericProgInfo
import List(delete)

-----------------------------------------------------------------------
--- An analysis to compute the sibling constructors (belonging to the
--- same data type) for a data constructor.

siblingCons :: Analysis [QName]
siblingCons =
  simpleConstructorAnalysis "SiblingCons" consNamesOfType
 where
  -- get all constructor names of datatype declaration
  consNamesOfType cdecl (Type _ _ _ consDecls) =
    filter (/= (consName cdecl)) (map consName consDecls)
  consNamesOfType _ (TypeSyn _ _ _ _) = []

------------------------------------------------------------------------------
-- The completeness analysis must be applied to complete programs,
-- i.e., modules together with all their imported modules (although
-- functions are locally checked, the definition of all data types
-- used in the patterns are needed).
-- It assigns to a FlatCurry program the list of all qualified function names
-- together with a flag indicating whether this function is completely
-- defined on its input types (i.e., reducible for all ground data terms).

-- The possible outcomes of the completeness analysis:
data Completeness =
     Complete       -- completely defined
   | InComplete     -- incompletely defined
   | InCompleteOr   -- incompletely defined in each branch of an "Or"

--- A function is totally defined if it is pattern complete and depends
--- only on totally defined functions.
totalAnalysis :: Analysis Bool
totalAnalysis =
  combinedDependencyFuncAnalysis "Total" patCompAnalysis True analyseTotally

analyseTotally :: ProgInfo Completeness -> FuncDecl -> [(QName,Bool)] -> Bool
analyseTotally pcinfo fdecl calledfuncs =
  (maybe False (\c->c==Complete) (lookupProgInfo (funcName fdecl) pcinfo))
  && all snd calledfuncs

-- Shows the result of the totally-defined analysis.
showTotally :: Bool -> String
showTotally True  = "totally defined"
showTotally False = "partially defined"

------------------------------------------------------------------------------
--- Pattern completeness analysis
patCompAnalysis :: Analysis Completeness
patCompAnalysis =
  combinedSimpleFuncAnalysis "PatComplete" siblingCons analysePatComplete

-- Shows the result of the completeness analysis.
showComplete :: Completeness -> String
showComplete Complete     = "complete"
showComplete InComplete   = "incomplete"
showComplete InCompleteOr = "incomplete in each disjunction"


analysePatComplete :: ProgInfo [QName] -> FuncDecl -> Completeness
analysePatComplete consinfo fdecl = anaFun fdecl
 where
  anaFun (Func _ _ _ _ (Rule _ e)) = isComplete consinfo e
  anaFun (Func _ _ _ _ (External _)) = Complete

isComplete :: ProgInfo [QName] -> Expr -> Completeness
isComplete _ (Var _)      = Complete
isComplete _ (Lit _)      = Complete
isComplete consinfo (Comb _ f es) =
  if f==("Prelude","commit") && length es == 1
  then isComplete consinfo (head es)
  else Complete
isComplete _ (Free _ _) = Complete
isComplete _ (Let _ _) = Complete
isComplete consinfo (Or e1 e2) =
   combineOrResults (isComplete consinfo e1) (isComplete consinfo e2)
-- if there is no branch, it is incomplete:
isComplete _ (Case _ _ []) = InComplete
-- for literal branches we assume that not all alternatives are provided:
isComplete _ (Case _ _ (Branch (LPattern _)   _ : _)) = InComplete
isComplete consinfo (Case _ _ (Branch (Pattern cons _) bexp : ces)) =
    combineAndResults
      (checkAllCons (maybe [] id (lookupProgInfo cons consinfo)) ces)
      (isComplete consinfo bexp)
  where
   -- check for occurrences of all constructors in each case branch:
   checkAllCons []    _  = Complete
   checkAllCons (_:_) [] = InComplete
   checkAllCons (_:_) (Branch (LPattern _)   _ : _) = InComplete -- should not occur
   checkAllCons (c:cs) (Branch (Pattern i _) e : ps) =
     combineAndResults (checkAllCons (delete i (c:cs)) ps)
                       (isComplete consinfo e)

-- Combines the completeness results in different Or branches.
combineOrResults Complete     _            = Complete
combineOrResults InComplete   Complete     = Complete
combineOrResults InComplete   InComplete   = InCompleteOr
combineOrResults InComplete   InCompleteOr = InCompleteOr
combineOrResults InCompleteOr Complete     = Complete
combineOrResults InCompleteOr InComplete   = InCompleteOr
combineOrResults InCompleteOr InCompleteOr = InCompleteOr

-- Combines the completeness results in different case branches.
combineAndResults InComplete   _            = InComplete
combineAndResults Complete     Complete     = Complete
combineAndResults Complete     InComplete   = InComplete
combineAndResults Complete     InCompleteOr = InCompleteOr
combineAndResults InCompleteOr Complete     = InCompleteOr
combineAndResults InCompleteOr InComplete   = InComplete
combineAndResults InCompleteOr InCompleteOr = InCompleteOr

