------------------------------------------------------------------------
--- Groundness analysis based on the [Brassel/Hanus ICLP 2005]
------------------------------------------------------------------------

module Groundness(Ground(..),showGround,groundAnalysis) where

import FlatCurry
import List
import Analysis

--- Type to represent groundness information.
--- Definitely ground (G), maybe non-ground (A), or maybe non-ground
--- if i-th argument is non-ground (P i).
data Ground = G | A | P [Int]

-- Show groundness information as a string.
showGround :: AOutFormat -> Ground -> String
showGround ANote G      = "G"
showGround AText G      = "always ground result"
showGround ANote A      = "A"
showGround AText A      = "possibly non-ground result"
showGround ANote (P ps) = show ps
showGround AText (P ps) =
  "ground if argument" ++
  (if length ps == 1 then ' ' : show (head ps) ++ " is ground"
                     else "s " ++ show ps ++ " are ground")

lub :: Ground -> Ground -> Ground
lub G      y      = y
lub A      _      = A
lub (P ps) G      = P ps
lub (P _ ) A      = A
lub (P ps) (P qs) = P (union ps qs)

------------------------------------------------------------------------
-- Analyze the groundness information of functions.

groundAnalysis :: Analysis Ground
groundAnalysis = dependencyFuncAnalysis "Groundness" G groundFunc

groundFunc :: FuncDecl -> [(QName,Ground)] -> Ground
groundFunc (Func (m,f) _ _ _ rule) calledFuncs
 | m==prelude && f `elem` preludeGroundFuncs = G
 | m==prelude = maybe anaresult id (lookup f preludeFuncs)
 | otherwise  = anaresult
 where
  anaresult = groundFuncRule calledFuncs rule

  preludeFuncs = [("cond",P [2]),("seq",P [2]),("ensureNotFree",P [1])]

  preludeGroundFuncs =
    ["+","-","*","div","mod","divMod","quot","rem","quotRem","negateFloat",
     "==","=:=","=:<=","compare","<",">","<=",">=","failed","error"]


groundFuncRule :: [(QName,Ground)] -> Rule -> Ground
groundFuncRule _ (External _) = A -- nothing known about other externals
groundFuncRule calledFuncs (Rule args rhs) =
  absEvalExpr (zip args (map (\i->P [i]) [1..])) rhs
 where
  -- abstract evaluation of an expression w.r.t. groundness environment
  absEvalExpr env (Var i)  = maybe A -- occurs in case of recursive lets
                                   id  (lookup i env)
  absEvalExpr _   (Lit _)  = G
  absEvalExpr env (Comb ct g es) =
    if ct == FuncCall
    then maybe (error $ "Abstract value of " ++ show g ++ " not found!")
               (\gd -> let curargs = zip [1..] (map (absEvalExpr env) es)
                        in groundApply gd curargs)
               (lookup g calledFuncs)
    else foldr lub G (map (absEvalExpr env) es)
  absEvalExpr env (Free vs e) = absEvalExpr (zip vs (repeat A) ++ env) e
  absEvalExpr env (Let bs e)  = absEvalExpr (absEvalBindings env bs) e
  absEvalExpr env (Or e1 e2)  = lub (absEvalExpr env e1) (absEvalExpr env e2)
  absEvalExpr env (Typed e _) = absEvalExpr env e
  absEvalExpr env (Case _  e bs) = foldr lub G (map absEvalBranch bs)
   where
    gcase = absEvalExpr env e

    absEvalBranch (Branch (LPattern _) be) = absEvalExpr env be
    absEvalBranch (Branch (Pattern _ pargs) be) =
      absEvalExpr (map (\pi -> (pi,gcase)) pargs ++ env) be

  -- could be improved for recursive lets with local fixpoint computation
  absEvalBindings env [] = env
  absEvalBindings env ((i,exp):bs) =
    absEvalBindings ((i, absEvalExpr env exp) : env) bs

-- compute groundness information for an application
groundApply :: Ground -> [(Int,Ground)] -> Ground
groundApply G _ = G
groundApply A _ = A
groundApply (P ps) gargs =
  foldr lub G (map (\p -> maybe A id (lookup p gargs)) ps)

prelude = "Prelude"

-----------------------------------------------------------------------
