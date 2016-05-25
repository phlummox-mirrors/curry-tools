-------------------------------------------------------------------------
--- A transformation of Curry programs into Agda programs.
---
--- @author Michael Hanus
--- @version May 2016
-------------------------------------------------------------------------

module ToAgda(theoremToAgda) where

import AbstractCurry.Types
import AbstractCurry.Select
import AbstractCurry.Build
import List
import Rewriting.Files
import Rewriting.Term
import Rewriting.Rules
import Rewriting.CriticalPairs
import TheoremUsage
import VerifyOptions

-- to access the determinism analysis:
import GenericProgInfo   (ProgInfo, lookupProgInfo)
import Deterministic     (Deterministic(..))

-------------------------------------------------------------------------

--- Generate an Agda program file for a given theorem name and the
--- list of all functions involved in this theorem.
theoremToAgda :: Options -> QName -> [CFuncDecl] -> [CTypeDecl] -> IO ()
theoremToAgda opts qtheoname allfuncs alltypes = do
  let (rename, orgtypedrules)  = funcDeclsToTypedRules allfuncs
      typedrules = map (transformRule opts) orgtypedrules
      (theorules,funcrules) =
         partition (\ (fn,_,_) -> isTheoremName (snd (readQN fn))) typedrules
      theoname = fromTheoremName (snd qtheoname)
      mname = "TO-PROVE-" ++ theoname
      agdaprog = agdaHeader mname ++
                 unlines (map typeDeclAsAgda alltypes) ++
                 unlines (map (\ (fn,te,trs) ->
                                  showTypedTRSAsAgda opts rename fn te trs)
                              funcrules) ++
                 unlines (map (theoremAsAgda rename) theorules)
  --putStr agdaprog
  let agdafile = mname ++ ".agda"
  when (optVerb opts > 1 || not (optStore opts)) $ putStr agdaprog
  when (optStore opts) $ do
    writeFile agdafile agdaprog
    when (optVerb opts > 0) $ putStrLn $
     "Agda module '" ++ agdafile ++ "' written.\n" ++
     "If you completed the proof, rename it to 'PROOF-" ++ theoname ++ ".agda'."

agdaHeader :: String -> String
agdaHeader mname = unlines $
  [ "-- Agda program using the Iowa Agda library\n"
  , "open import bool\n"
  , "module " ++ mname
  , "  (Choice : Set)\n  (choose : Choice → 𝔹)"
  , "  (lchoice : Choice → Choice)\n  (rchoice : Choice → Choice)"
  , "  where\n"
  , unlines (map (\im -> "open import " ++ im)
                 ["eq","bool","nat","list","maybe"])
  ]

-------------------------------------------------------------------------
-- Map a list of function declarations into renaming function (for showing
-- identifiers in the the Agda code)
--  and a list of function names, types, and TRS.
funcDeclsToTypedRules :: [CFuncDecl]
                      -> (String -> String, [(String, CTypeExpr, TRS String)])
funcDeclsToTypedRules fdecls = (rename, typedrules)
 where
  typedrules  = map funcDeclToTypedRule fdecls
  allrules    = concatMap (\ (_,_,rules) -> rules) typedrules
  rename      = rename4agda (map readQN (allNamesOfTRS allrules))

allNamesOfTRS :: TRS String -> [String]
allNamesOfTRS =
  nub . concatMap allNamesOfTerm . concatMap (\ (lhs,rhs) -> [lhs,rhs])
 where
  allNamesOfTerm (TermVar _) = []
  allNamesOfTerm (TermCons f ts) = f : concatMap allNamesOfTerm ts

-- Perform some renamings of qualified names for Agda.
-- In particular, drop the module qualifier from a name if it is still unique
-- to improve the readability of the Agda code.
rename4agda :: [QName] -> String -> String
rename4agda allnames s
  | (qn,fn) == pre ":"     = "::"
  | (qn,fn) == pre "True"  = "tt"
  | (qn,fn) == pre "False" = "ff"
  | (qn,fn) == nat "Z"     = "zero"
  | (qn,fn) == nat "S"     = "suc"
  | otherwise
  = maybe fn (const s) (find (\ (q,f) -> f==fn && q /= qn) allnames)
 where
  (qn,fn) = readQN s

-- Map a function declaration into the function name, type, and TRS.
funcDeclToTypedRule :: CFuncDecl -> (String, CTypeExpr, TRS String)
funcDeclToTypedRule (CFunc qn ar vis texp rules) =
  funcDeclToTypedRule (CmtFunc "" qn ar vis texp rules) 
funcDeclToTypedRule fdecl@(CmtFunc _ _ _ _ texp _) =
  let (fn, trs) = fromFunc fdecl
   in (fn, texp, trs)

-------------------------------------------------------------------------
-- Transform a TRS for a function according to transformation method
-- based in determinism information:
-- TODO: check overlapping rules and implement partial functions 

transformRule :: Options
              -> (String, CTypeExpr, TRS String)
              -> (String, CTypeExpr, TRS String)
transformRule opts (fn,texp,trs)
  | lookupProgInfo (readQN fn) detinfo == Just Det
  = (fn,texp,trs)
  | otherwise
  = (fn, CFuncType (baseType (pre "Choice")) texp,
     map addChoices (elimOverlaps trs))
 where
  detinfo   = detInfos opts
  critPairs = cPairs trs
  choicevar = TermVar 46

  elimOverlaps rs = if null critPairs then rs else
    let arity = case head rs of (TermCons _ args,_) -> length args
                                _ -> error "transformRule: no arity"
        newargs = map TermVar [1 .. arity]
        ruleName i = fn ++ "-rule" ++ show i
     in [(TermCons fn newargs,
          foldr1 (\x y -> TermCons "Prelude.?" [x,y])
                 (map (\i -> TermCons (ruleName i) newargs)
                      [1 .. length rs]))]
        ++ map (\(i,(TermCons _ args, rhs)) ->
                   (TermCons (ruleName i ++ "#") args,rhs))
               (zip [1 .. length rs] rs)

  addChoices (lhs,rhs) = (addLhsChoice choicevar lhs,
                          snd (addChoiceInTerm (Right choicevar) rhs))

  addLhsChoice _ v@(TermVar _) = v
  addLhsChoice ch (TermCons f args) = TermCons f (ch : args)

  addChoiceInTerm ch v@(TermVar _) = (ch,v)
  addChoiceInTerm ch (TermCons f args)
   | f == "Prelude.?" && length args == 2
   = let (ch1,cargs) = addChoiceInTerms (nextChoice ch) args
      in (ch1, TermCons "Prelude.if_then_else"
                        (TermCons "Prelude.choose" [currChoice ch] : cargs))
   | lookupProgInfo (readQN f) detinfo == Just NDet
   = let (ch1,cargs) = addChoiceInTerms (nextChoice ch) args
      in (ch1, TermCons f (currChoice ch : cargs))
   | otherwise
   = let (ch1,cargs) = addChoiceInTerms ch args
      in (ch1, TermCons f cargs)

  addChoiceInTerms ch [] = (ch,[])
  addChoiceInTerms ch (t:ts) = let (ch1,ct ) = addChoiceInTerm  ch t
                                   (ch2,cts) = addChoiceInTerms ch1 ts
                                in (ch2,ct:cts)

  currChoice (Left ch) = TermCons "Prelude.lchoice" [ch]
  currChoice (Right ch) = ch

  nextChoice (Left ch) = Right (TermCons "Prelude.rchoice" [ch])
  nextChoice (Right ch) = Left ch

-------------------------------------------------------------------------

-- Show typed rules (properties) as theorems in Agda.
theoremAsAgda :: (String -> String) -> (String, CTypeExpr, TRS String) -> String
theoremAsAgda _ (_,_,[]) = ""
theoremAsAgda _ (fn,_,(TermVar _ ,_) : _) =
  error $ "Theorem '" ++ fn ++ "': variable in left-hand side"
theoremAsAgda rn (fn, texp, (TermCons _ largs,rhs) : rules) =
  rn fn ++ " : " ++ (if null tvars then "" else showForAll tvars) ++
  showTypeWOResult largs texp ++
  showTermAsAgda False (term2theorem (mapTerm rn rhs)) ++ "\n" ++
  showTermAsAgda False (mapTerm rn (TermCons fn largs)) ++ " = ?\n" ++
  theoremAsAgda rn (fn,texp,rules)
 where
  tvars = nub (tvarsOfType texp)

  showTypeWOResult [] _ = ""
  showTypeWOResult (arg:args) te = case te of
    CFuncType t1 t2 -> "(" ++ showTermAsAgda False (mapTerm rn arg) ++ " : " ++
                       showTypeAsAgda False t1 ++
                       ") \x2192 " ++ showTypeWOResult args t2
    _ -> error "Inconsistent type in theorem " ++ fn

  term2theorem t = case t of
    TermCons "-=-"    args -> TermCons "\x2261" args
    TermCons "<~>"    args -> TermCons "\x2261" args
    TermCons "always" args -> TermCons "\x2261" (args ++ [agdaTrue])
    _ -> t

-- Show a TRS for an operation whose type is given as Agda definitions.
showTypedTRSAsAgda :: Options -> (String -> String) -> String -> CTypeExpr
                   -> TRS String -> String
showTypedTRSAsAgda opts rn fn texp trs =
  (if lookupProgInfo (readQN fn) (totInfos opts) == Just True
    then ""
    else "-- WARNING: function '" ++ fn ++ "' is partial!\n") ++
  (if any isLocalRule trs
    then "-- WARNING: function '" ++ fn ++ "' has overlapping rules!\n"
    else "") ++
  showTypeSignatureAsAgda (rn fn) texp ++"\n"++
  unlines (showRulesAsAgda trs)
 where
  showRulesAsAgda [] = []
  showRulesAsAgda [rule] = [showRuleAsAgda rn rule]
  showRulesAsAgda (rule1 : rule2 : rules) =
    [showRuleAsAgda rn rule1] ++
    (if not (isLocalRule rule1) && isLocalRule rule2 then [" where"] else []) ++
    (if ruleFunc rule1 /= ruleFunc rule2
     then ["","   " ++ showTypeSignatureAsAgda
                         (rn (unLocalName (ruleFunc rule2))) texp]
     else []) ++
    showRulesAsAgda (rule2 : rules)

isLocalRule :: Rule String -> Bool
isLocalRule rule = last (ruleFunc rule) == '#'

ruleFunc :: Rule String -> String
ruleFunc rl@(TermVar _,_) = error $ "Rule with variable lhs: " ++ showRule rl
ruleFunc (TermCons f _,_) = f

unLocalLhs :: Term String -> Term String
unLocalLhs (TermVar _) = error $ "LHS with variable"
unLocalLhs (TermCons f args) = TermCons (unLocalName f) args

unLocalName :: String -> String
unLocalName s = if last s == '#' then take (length s - 1) s else s

showRuleAsAgda :: (String -> String) -> Rule String -> String
showRuleAsAgda rn rl@(lhs,rhs) =
  (if isLocalRule rl then "   " else "") ++
  showTermAsAgda False (mapTerm rn (unLocalLhs lhs)) ++ " = " ++
  showTermAsAgda False (mapTerm rn rhs)

showTermAsAgda :: Bool -> Term String -> String
showTermAsAgda _ (TermVar v) = showVarAsAgda v
showTermAsAgda withbrackets (TermCons c args) = inBrackets $
  if c == "if_then_else" && length args == 3
   then let is = map (showTermAsAgda False) args
        in unwords ["if", is!!0, "then", is!!1, "else", is!!2]
   else
    if c `elem` knownInfixOps
     then if length ts == 2
           then unwords [head ts, c, head (tail ts)]
           else unwords (("_"++c++"_") : ts)
     else unwords $ (c : ts)
 where
  ts = map (showTermAsAgda True) args
  inBrackets s = if not (null ts) && withbrackets then "(" ++ s ++ ")" else s

-- Infix operators known in Agda:
knownInfixOps :: [String]
knownInfixOps = ["::","==","+","*","\x2261"]

showVarAsAgda :: Int -> String
showVarAsAgda v | v >= 0 = if q == 0
                          then [c]
                          else c:(show q)
  where
    (q, r) = divMod v 26
    c = "xyzuvwrstijklmnopqabcdefgh" !! r

-------------------------------------------------------------------------
typeDeclAsAgda :: CTypeDecl -> String
typeDeclAsAgda (CTypeSyn tc _ _ _) =
  error $ "Type synonyms not supported: " ++ showQN tc
typeDeclAsAgda (CNewType tc vis tvars consdecl) =
  typeDeclAsAgda (CType tc vis tvars [consdecl])
typeDeclAsAgda (CType tc _ tvars constrs) = unlines $
  (unwords $
     ["data", snd tc] ++
     map (\tv -> "("++ showTypeAsAgda False (CTVar tv) ++ " : Set)") tvars ++
     [": Set where"]) : map typeConsDeclAsAgda constrs
 where
  typeConsDeclAsAgda (CCons qc _ texps) =
    "   " ++ snd qc ++ " : " ++
    showTypeAsAgda False (foldr CFuncType (CTCons tc (map CTVar tvars)) texps)
  typeConsDeclAsAgda (CRecord qc _ _) =
    error $ "Records not yet supported: " ++ showQN qc

-------------------------------------------------------------------------
showTypeSignatureAsAgda :: String -> CTypeExpr -> String
showTypeSignatureAsAgda fn texp =
   fn ++ " : " ++ (if null tvars then "" else showForAll tvars) ++
   showTypeAsAgda False texp
 where
  tvars = nub (tvarsOfType texp)

showForAll :: [CTVarIName] -> String
showForAll tvars = "{" ++ unwords (map (showTypeAsAgda False . CTVar) tvars) ++
                   " : Set} \x2192 "

showTypeAsAgda :: Bool -> CTypeExpr -> String
showTypeAsAgda withbrackets texp = case texp of
  CFuncType t1 t2 -> inBrackets $ showTypeAsAgda False t1 ++ " \x2192 " ++
                                  showTypeAsAgda False t2
  CTVar (i,_) -> showVarAsAgda (i + 18) -- to get 'a' for var index 0...
  CTCons tc targs ->
    if null targs
     then showTCon tc
     else inBrackets $ unwords (showTCon tc : map (showTypeAsAgda True) targs)
 where
  inBrackets s = if withbrackets then "(" ++ s ++ ")" else s

showTCon :: QName -> String
showTCon tc
 | tc == pre "[]"     = "\x1d543"
 | tc == pre "Int"    = "\x2115"
 | tc == pre "Bool"   = "\x1d539"
 | tc == pre "Maybe"  = "maybe"
 | tc == pre "Choice" = "Choice"
 | tc == nat "Nat"    = "\x2115"
 | otherwise = snd tc

nat :: String -> QName
nat s = ("Nat",s)

agdaTrue :: Term String
agdaTrue = TermCons "tt" []

-------------------------------------------------------------------------
