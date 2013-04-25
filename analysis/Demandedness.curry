------------------------------------------------------------------------------
--- Demandedness analysis:
--- checks whether functions demands a particular argument, i.e.,
--- delivers only bottom if some argument is bottom.
---
--- @author Michael Hanus
--- @version April 2013
------------------------------------------------------------------------------

module Demandedness
 where

import Analysis
import FlatCurry
import FlatCurryGoodies
import List((\\))

------------------------------------------------------------------------------
--- Data type to represent determinism information.
type DemandedArgs = [Int]

-- Show determinism information as a string.
showDemand :: DemandedArgs -> String
showDemand = show

-- Abstract demand domain.
data DemandDomain = Bot | Top

-- Least upper bound on abstract demand domain.
lub :: DemandDomain -> DemandDomain -> DemandDomain
lub Bot x = x
lub Top _ = Top

--- Demandedness analysis.
demandAnalysis :: Analysis DemandedArgs
demandAnalysis = dependencyFuncAnalysis "Demand" [1..] daFunc

-- An operation is non-deterministic if it has an overlapping definition.
-- or if it calls a non-deterministic operation.
daFunc :: FuncDecl -> [(QName,DemandedArgs)] -> DemandedArgs
daFunc (Func (m,f) _ _ _ rule) calledFuncs
 | f `elem` prelude2s && m==prelude = [1,2]
 | f `elem` prelude1s && m==prelude = [1]
 | otherwise = daFuncRule calledFuncs rule
 where
  prelude2s = ["==","=:=","compare","<=","$#","$##","$!","$!!",
               "+","-","*","div","mod","divMod","quot","rem","quotRem"]
  prelude1s = ["seq","ensureNotFree","apply","cond","=:<=","negateFloat"]
 -- TODO: >>= catch catchFail


daFuncRule :: [(QName,DemandedArgs)] -> Rule -> DemandedArgs
daFuncRule _ (External _) = [] -- nothing known about other externals
daFuncRule calledFuncs (Rule args rhs) =
  map fst
      (filter ((==Bot) . snd)
              (map (\botarg -> (botarg,absEvalExpr rhs [botarg])) args))
 where
  -- abstract evaluation of an expression w.r.t. variables assumed to be Bot
  absEvalExpr (Var i)        bvs = if i `elem` bvs then Bot else Top
  absEvalExpr (Lit _)        _   = Top
  absEvalExpr (Comb ct g es) bvs =
    if ct == FuncCall
    then maybe (error $ "Abstract value of " ++ show g ++ " not found!")
               (\gdas -> let curargs = map (\ (i,e) -> (i,absEvalExpr e bvs))
                                           (zip [1..] es)
                             cdas = gdas \\
                                    (map fst (filter ((/=Bot) . snd) curargs))
                          in if null cdas then Top else Bot)
               (lookup g calledFuncs)
    else Top
  absEvalExpr (Free _ e)     bvs = absEvalExpr e bvs
  absEvalExpr (Let bs e)     bvs = absEvalExpr e (absEvalBindings bs bvs)
  absEvalExpr (Or e1 e2)     bvs = lub (absEvalExpr e1 bvs) (absEvalExpr e2 bvs)
  absEvalExpr (Case _  e bs) bvs =
    if absEvalExpr e bvs == Bot
    then Bot
    else foldr lub Bot (map absEvalBranch bs)
   where absEvalBranch (Branch _ be) = absEvalExpr be bvs
  absEvalExpr (Typed e _) bvs = absEvalExpr e bvs

    -- could be improved with local fixpoint computation
  absEvalBindings [] bvs = bvs
  absEvalBindings ((i,exp) : bs) bvs =
    let ival = absEvalExpr exp bvs
     in if ival==Bot
        then absEvalBindings bs (i:bvs)
        else absEvalBindings bs bvs

prelude = "Prelude"

------------------------------------------------------------------------------
