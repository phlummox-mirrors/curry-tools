------------------------------------------------------------------------------
--- Termination analysis:
--- checks whether an operation is terminating, i.e.,
--- whether all evaluations on ground argument terms are finite.
--- The method used here checks whether the arguments in all recursive
--- calls of an operation are smaller than the arguments passed to
--- the operation.
---
--- @author Michael Hanus
--- @version Januar 2017
------------------------------------------------------------------------------

module Termination
  ( terminationAnalysis, showTermination
  , productivityAnalysis, showProductivity, Productivity(..)
  ) where

import Analysis
import Char(isDigit)
import FlatCurry.Types
import FlatCurry.Goodies
import GenericProgInfo
import List
import RootReplaced (rootCyclicAnalysis)
import Sort(sort)
import Unsafe

------------------------------------------------------------------------------
-- The termination analysis is a global function dependency analysis.
-- It assigns to a FlatCurry function definition a flag which is True
-- if this operation is terminating, i.e., whether all evaluations

terminationAnalysis :: Analysis Bool
terminationAnalysis = dependencyFuncAnalysis "Terminating" False isTerminating

-- Show termination information as a string.
showTermination :: AOutFormat -> Bool -> String
showTermination AText True  = "terminating"
showTermination ANote True  = ""
showTermination AText False = "possibly non-terminating" 
showTermination ANote False = "maybe not term."

-- An operation is functionally defined if its definition is not
-- non-deterministic (no overlapping rules, no extra variables) and
-- it depends only on functionally defined operations.
isTerminating :: FuncDecl -> [(QName,Bool)] -> Bool
isTerminating (Func qfunc _ _ _ rule) calledFuncs = hasTermRule rule
 where
  hasTermRule (Rule args e) = hasTermExp (map (\a -> (a,[])) args) e
  -- we assume that all externally defined operations are terminating:
  hasTermRule (External _) = True
  
  hasTermExp _ (Var _)    = True
  hasTermExp _ (Lit _)    = True
  hasTermExp _ (Free _ _) = False -- to be improved!!!
  hasTermExp args (Let bs e) =
    all (hasTermExp args) (map snd bs) && hasTermExp args e -- ???
  hasTermExp args (Or e1 e2) =
    hasTermExp args e1 && hasTermExp args e2
  hasTermExp args (Case _ e bs) =
    hasTermExp args e &&
    all (\ (Branch pt be) -> hasTermExp (addSmallerArgs args e pt) be) bs
  hasTermExp args (Typed e _) = hasTermExp args e
  hasTermExp args (Comb ct qf es) =
    case ct of
      ConsCall       -> all (hasTermExp args) es
      ConsPartCall _ -> all (hasTermExp args) es
      _ -> (if qf == qfunc -- is this a recursive call?
              then any isSmallerArg (zip args es)
              else maybe False id (lookup qf calledFuncs)) &&
           all (hasTermExp args) es

  isSmallerArg ((_,sargs),exp) = case exp of
    Var v -> v `elem` sargs
    _     -> False
    
-- compute smaller args w.r.t. a given discriminating expression and
-- branch pattern
addSmallerArgs :: [(Int, [Int])] -> Expr -> Pattern -> [(Int, [Int])]
addSmallerArgs args de pat =
  case de of
    Var v -> maybe args
                   (\argpos -> let (av,vs) = args!!argpos
                               in replace (av, varsOf pat ++ vs) argpos args)
                   (findIndex (isInArg v) args)
    _     -> args -- other expression, no definite smaller expressions
 where
   varsOf (LPattern _)      = []
   varsOf (Pattern _ pargs) = pargs
   
   isInArg v (argv,svs) = v==argv || v `elem` svs
   
------------------------------------------------------------------------------
-- The productivity analysis is a global function dependency analysis
-- which depends on the termination and root-cyclic analysis.
-- It assigns to a FlatCurry function definition a flag which is True
-- if this operation is terminating, i.e., whether all evaluations

--- Data type to represent productivity status of an operation.
data Productivity = Terminating | Productive | Looping

productivityAnalysis :: Analysis Productivity
productivityAnalysis =
  combined2SimpleFuncAnalysis "Productive"
                              terminationAnalysis
                              rootCyclicAnalysis
                              isProductive

-- Show productivity information as a string.
showProductivity :: AOutFormat -> Productivity -> String
showProductivity _ Looping     = "possibly looping"
showProductivity _ Productive  = "productive"
showProductivity _ Terminating = "terminating" 

lubProd :: Productivity -> Productivity -> Productivity
lubProd Looping     _           = Looping
lubProd Productive  Terminating = Productive
lubProd Productive  Productive  = Productive
lubProd Productive  Looping     = Looping
lubProd Terminating p           = p

-- An operation is functionally defined if its definition is not
-- non-deterministic (no overlapping rules, no extra variables) and
-- it depends only on functionally defined operations.
isProductive :: ProgInfo Bool -> ProgInfo Bool
             -> FuncDecl -> Productivity --[(QName,Bool)] -> Bool
isProductive terminfo rcyclicinfo (Func _ _ _ _ rule) = hasProdRule rule
 where
  hasProdRule (Rule _ e) = hasProdExp e
  -- we assume that all externally defined operations are terminating:
  hasProdRule (External _) = Terminating
  
  hasProdExp (Var _)    = Terminating
  hasProdExp (Lit _)    = Terminating
  hasProdExp (Free _ e) = lubProd Productive (hasProdExp e) -- to be improved!!!
  hasProdExp (Let bs e) =
    lubProd (hasProdExp e)
            (foldr lubProd Terminating (map (\ (_,be) -> hasProdExp be) bs))
  hasProdExp (Or e1 e2) = lubProd (hasProdExp e1) (hasProdExp e2)
  hasProdExp (Case _ e bs) =
    foldr lubProd (hasProdExp e) (map (\ (Branch _ be) -> hasProdExp be) bs)
  hasProdExp (Typed e _) = hasProdExp e
  hasProdExp (Comb ct qf es) =
    case ct of
      ConsCall       -> foldr lubProd Terminating (map hasProdExp es)
      ConsPartCall _ -> foldr lubProd Terminating (map hasProdExp es)
      _ -> foldr lubProd
                 (if maybe False id (lookupProgInfo qf terminfo)
                    then Terminating
                    else if maybe True id (lookupProgInfo qf rcyclicinfo)
                           then Looping
                           else Productive)
                 (map hasProdExp es)

------------------------------------------------------------------------------
