------------------------------------------------------------------------
--- The implementation of the "postcondition" reducer that simplifies
--- postconditions w.r.t. a given list of theorems.
---
--- @author Michael Hanus
--- @version May 2016
------------------------------------------------------------------------

module SimplifyPostConds(simplifyPostConditionWithTheorems) where

import AbstractCurry.Types
import AbstractCurry.Select
import AbstractCurry.Build
import ContractUsage
import List (last, maximum)
import Maybe (maybeToList)
import ReadShowTerm(readQTerm)
import Rewriting.Files
import Rewriting.Term
import Rewriting.Position
import Rewriting.Substitution
import Rewriting.Rules

simplifyPostConditionWithTheorems :: Int -> [CFuncDecl] -> [CFuncDecl]
                                  -> IO [CFuncDecl]
simplifyPostConditionWithTheorems verb theofuncs postconds
 | null theofuncs
 = return postconds
 | otherwise
 = mapIO (simplifyPostCondition verb theofuncs) postconds >>= return . concat

simplifyPostCondition :: Int -> [CFuncDecl] ->  CFuncDecl ->  IO [CFuncDecl]
simplifyPostCondition verb theofuncs (CFunc qn ar vis texp rs) =
  simplifyPostCondition verb theofuncs (CmtFunc "" qn ar vis texp rs)
simplifyPostCondition verb theofuncs (CmtFunc cmt qn ar vis texp rules) = do
  when (verb>0) $ putStr $ unlines
    ["THEOREMS:", showTRS' theoTRS, "SIMPLIFICATION RULES:", showTRS' simprules]
  redrules <- mapIO (simplifyRule verb simprules qn) rules
  return $ if all isTrivial redrules
            then []
            else [CmtFunc cmt qn ar vis texp redrules]
 where
  theoTRS   = concatMap (snd . fromFunc) theofuncs
  simprules = concatMap theoremToSimpRules theoTRS ++ standardSimpRules

theoremToSimpRules :: Rule String -> [Rule String]
theoremToSimpRules rl@(_, TermCons f args)
  | f == "Test.EasyCheck.-=-" || f == "Test.EasyCheck.<~>"
  = [(TermCons "Prelude.==" args, trueTerm),
     (TermCons "Prelude.==" (reverse args), trueTerm)]
  | f == "Test.EasyCheck.always" = [(head args, trueTerm)]
  | otherwise = [rl]

isTrivial :: CRule -> Bool
isTrivial (CRule _ rhs) = case rhs of
  CSimpleRhs exp [] -> exp == constF (pre "True")
  _                 -> False

-- To avoid infinite loops during simplification, we define a maximum number
-- of allowed simplification steps:
maxSimpSteps :: Int
maxSimpSteps = 100

-- Simplify a rule of a postcondition.
simplifyRule :: Int -> TRS String ->  QName -> CRule ->  IO CRule
simplifyRule verb simprules qn crule@(CRule rpats _) = do
  when (verb > 0 ) $ putStrLn $ unlines
    ["POSTCONDITION: " ++ showRule' (lhs,rhs),
     "POSTCONDEXP:   " ++ showTerm' postcondexp,
     "SIMPLIFIEDEXP: " ++ showTerm' simpterm,
     "SIMPPOSTCOND:  " ++ showRule' simppostcond ]     
  return (simpleRule rpats (term2acy (concatMap varsOfPat rpats) simppostrhs))
 where
   (lhs,rhs)   = fromRule (showQN qn) crule
   postcondexp = postCondition2Term lhs rhs
   simpterm = simplifyTerm maxSimpSteps simprules postcondexp
   simppostrhs  = postConditionTermToRule lhs simpterm
   simppostcond = (lhs, simppostrhs)

--- Transform a post-condition rule into a term by substituting
---  the last argument variable by the function call.
postCondition2Term :: Term String -> Term String -> Term String
postCondition2Term (TermCons f args) rhs =
  let TermVar i  = last args
      (qn,fn)    = readQN f
      fcall      = TermCons (showQN (qn, fromPostCondName fn))
                            (take (length args - 1) args)
      fcallsubst = extendSubst emptySubst i fcall
   in applySubst fcallsubst rhs

--- Transform (simplified) post-condition back into rule by replacing
--- function call by the last argument variable. by the function call.
postConditionTermToRule :: Term String -> Term String -> Term String
postConditionTermToRule (TermCons f args) term =
  let TermVar i  = last args
      (qn,fn)    = readQN f
      fcall      = TermCons (showQN (qn, fromPostCondName fn))
                            (take (length args - 1) args)
   in replaceAllTerms (fcall, TermVar i) term

replaceAllTerms :: Rule String -> Term String -> Term String
replaceAllTerms (lhs,rhs) term =
  if null oneStep
   then term
   else replaceAllTerms (lhs,rhs) (head oneStep)
 where
  oneStep = [ replaceTerm term p rhs | p <- positions term, (term |> p) == lhs ]

------------------------------------------------------------------------

simplifyTerm :: Int -> TRS String -> Term String -> Term String
simplifyTerm maxsteps simprules term =
  if null oneStep || maxsteps==0
   then term
   else simplifyTerm (maxsteps-1) simprules (head oneStep)
 where
  oneStep = [ replaceTerm term p (applySubst sub rhs)
            | p <- positions term,
              rule <- simprules,
              let vMax = maximum (0: tVars term) + 1,
              let (lhs,rhs) = renameVarsBy vMax rule,
              sub <- maybeToList (match (term |> p) lhs) ]

-- match t1 t2 = sub  iff  sub(t2) = t1
match :: Term String -> Term String -> Maybe (Subst String)
match = matchTerm emptySubst
 where
  matchTerm sub t1 (TermVar i) =
    maybe (Just (extendSubst sub i t1))
          (\t2 -> matchTerm sub t1 t2)
          (lookupSubst sub i)
  matchTerm sub (TermCons f1 args1) (TermCons f2 args2) =
    if f1 /= f2 then Nothing else matchArgs sub args1 args2
  matchTerm _ (TermVar _) (TermCons _ _) = Nothing

  matchArgs _ (_:_) [] = Nothing
  matchArgs _ [] (_:_) = Nothing
  matchArgs sub []  [] = Just sub
  matchArgs sub (x:xs) (y:ys) = maybe Nothing
                                      (\s -> matchArgs s xs ys)
                                      (matchTerm sub x y)


-- Some additional simplifcation rules (based on Prelude definitions):
standardSimpRules :: TRS String
standardSimpRules =
  [ (TermCons "Prelude.&&" [trueTerm, x1], x1)
  , (TermCons "Prelude.&&" [x1, trueTerm], x1)
  ]
 where
  x1 = TermVar 1

trueTerm :: Term String
trueTerm = TermCons "Prelude.True" []

------------------------------------------------------------------------
--- Translate terms into AbstractCurry expressions

-- to be extended
term2acy :: [CVarIName] -> Term String -> CExpr
term2acy cvars (TermVar i) =
  maybe (error "term2acy: cannot find variable")
        (\s -> CVar (i,s))
        (lookup i cvars)
term2acy cvars (TermCons f args)
 | null args && head f == '%' = CLit (const2literal (tail f))
 | otherwise
 = foldl CApply (CSymbol (readQN f)) (map (term2acy cvars) args)

const2literal :: String -> CLiteral
const2literal sl = case sl of
  ('i':_:s) -> CIntc   (readQTerm s)
  ('f':_:s) -> CFloatc (readQTerm s)
  ('c':_:s) -> CCharc  (head s)
  ('s':_:s) -> CStringc s
  _   -> error "const2literal: unknown literal"

------------------------------------------------------------------------
-- for better readable output: drop module prefix
dropMod :: String -> String
dropMod = snd . readQN

showTerm' :: Term String -> String
showTerm' = showTerm . mapTerm dropMod

showRule' :: Rule String -> String
showRule' (lhs,rhs) = showRule (mapTerm dropMod lhs, mapTerm dropMod rhs)

showTRS' :: TRS String -> String
showTRS' = unlines . map showRule'

------------------------------------------------------------------------
