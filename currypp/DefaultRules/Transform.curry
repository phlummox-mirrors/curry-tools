-----------------------------------------------------------------------------
--- Translator for Curry programs to implement default rules
--- and deterministic functions.
---
--- @author Michael Hanus
--- @version September 2015
-----------------------------------------------------------------------------

import AbstractCurry.Types
import AbstractCurry.Files
import AbstractCurry.Select
import AbstractCurry.Build
import AbstractCurry.Pretty
import Char(isDigit,digitToInt)
import Directory
import Distribution
import List(isPrefixOf,isSuffixOf,partition)
import System

--------------------------------------------------------------------

banner :: String
banner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText =
     "Transformation Tool for Curry with Default Rules (Version of 03/09/15)"
   bannerLine = take (length bannerText) (repeat '=')

------------------------------------------------------------------------
-- Data type for transformation parameters
data TParam = TParam Bool -- work quietly?
                     Bool -- compile the transformed program?
                     Bool -- load and execute transformed program?

defaultTParam :: TParam
defaultTParam = TParam False False False

setRunQuiet :: TParam -> TParam
setRunQuiet (TParam _ cmp ep) = TParam True cmp ep

setCompile :: TParam -> TParam
setCompile (TParam wq _ ep) = TParam wq True ep

setExec :: TParam -> TParam
setExec  (TParam wq _ _) = TParam wq True True

------------------------------------------------------------------------

maintest :: String -> IO ()
maintest progname = do
  prog <- readUntypedCurry progname
  let newprog = showCProg (snd (translateProg prog))
  putStrLn newprog
  writeFile "Test.curry" newprog

main :: IO ()
main = do
  args <- getArgs
  processArgs defaultTParam args
 where
  processArgs tparam args = case args of
     ["-h"]          -> putStrLn $ banner ++ usageInfo
     ("-q":moreargs) -> processArgs (setRunQuiet  tparam) moreargs
     ("-c":moreargs) -> processArgs (setCompile   tparam) moreargs
     ("-r":moreargs) -> processArgs (setExec      tparam) moreargs
     [mname]         -> transMain tparam (stripCurrySuffix mname)
     (orgfile:infile:outfile:opts) ->
       maybe (printError args)
             (\vl -> transPP vl orgfile infile outfile)
	     (processOptions opts)
     _ -> printError args

  processOptions optargs = case optargs of
    []             -> Just 0
    ["-v"]         -> Just 1
    [['-','v',vl]] -> if isDigit vl then Just (digitToInt vl) else Nothing

  printError args =
    putStrLn $ banner ++
               "ERROR: Illegal arguments for transformation:\n" ++
               unwords args ++ "\n" ++ usageInfo

usageInfo :: String
usageInfo =
  "Usage: ... [-q|-c|-r] <module_name>\n"++
  "-q : work quietly\n"++
  "-c : compile the transformed program\n"++
  "-r : load the transformed program into "++curryCompiler++" (implies -c)\n"

------------------------------------------------------------------------
-- Transformation in "batch" mode:
transMain :: TParam -> String -> IO ()
transMain (TParam quiet compile execprog) progname = do
  let progfname = progname ++ ".curry"
      saveprogfname = progname++"_ORG.curry"
      transprogfname = progname++"_TRANS.curry"
      putStrNQ s = if quiet then done else putStr s
      putStrLnNQ s = if quiet then done else putStrLn s
  putStrNQ banner
  prog <- readUntypedCurry progname
  system $ "cleancurry " ++ progname
  let transprog = showCProg (snd (translateProg prog))
  putStrLnNQ "Transformed module:"
  putStrLnNQ transprog
  if not compile then done else do
    renameFile progfname saveprogfname
    writeFile progfname transprog
    compileAcyFcy quiet progname
    renameFile progfname transprogfname
    renameFile saveprogfname progfname
    putStrLnNQ $ "Transformed program written into '"++transprogfname++"'"
    if not execprog then done else do
      system $ "mate-terminal -x "++installDir++"/bin/curry :l "++progname
      done

compileAcyFcy :: Bool -> String -> IO ()
compileAcyFcy quiet progname = do
  params <- rcParams >>= return . setQuiet quiet
  callFrontendWithParams ACY params progname
  callFrontendWithParams FCY params progname

------------------------------------------------------------------------
-- Start sequentializer in "preprocessor mode":
transPP :: Int -> String -> String -> String -> IO ()
transPP verb orgfile infile outfile = do
  when (verb>0) $ putStr banner
  let savefile = orgfile++".SAVE"
      modname = stripCurrySuffix orgfile
  renameFile orgfile savefile
  starttime <- getCPUTime
  readFile infile >>= writeFile orgfile . replaceOptionsLine
  inputProg <- tryReadUntypedCurry modname savefile
  renameFile savefile orgfile
  let (detfuncnames,newprog) = translateProg inputProg
  writeFile outfile (showCProg newprog)
  stoptime <- getCPUTime
  when (verb>0) $ putStrLn
    ("Total transformation time: " ++ show (stoptime-starttime) ++ " ms")
  printProofObligation detfuncnames
 where
   tryReadUntypedCurry mn savefile =
     catch (readUntypedCurry mn)
           (\_ -> renameFile savefile orgfile >> exitWith 1)

-- Replace OPTIONS_CYMAKE line in a source text by blank line:
replaceOptionsLine :: String -> String
replaceOptionsLine = unlines . map replOptLine . lines
 where
  replOptLine s = if "{-# OPTIONS_CYMAKE " `isPrefixOf` s -- -}
                  then " "
                  else s

printProofObligation :: [QName] -> IO ()
printProofObligation qfs = unless (null qfs) $ do
  putStrLn line
  putStrLn "PROOF OBLIGATIONS:"
  mapIO_ (\ (q,f) -> putStrLn (q++"."++f++" is a deterministic operation.")) qfs
  putStrLn line
 where
  line = take 70 (repeat '=')

------------------------------------------------------------------------
-- Main transformation: transform a Curry program with default rules
-- and deterministic functions into a new Curry program where these
-- features are implemented by standard Curry features.
-- Moreover, the list of deterministic functions is returned
-- (to show the proof obligations to ensure completeness of the
-- transformation).

translateProg :: CurryProg -> ([QName],CurryProg)
translateProg prog@(CurryProg mn imps tdecls fdecls ops) =
  if null deffuncs && null detfuncnames
  then ([],prog)
  else (detfuncnames, CurryProg mn newimps tdecls newfdecls ops)
 where
  newimps          = if setFunMod `elem` imps then imps else setFunMod:imps
  detfuncnames     = map funcName (filter isDetFun fdecls)
  undetfuncs       = concatMap (transDetFun detfuncnames) fdecls
  (deffuncs,funcs) = partition isDefault undetfuncs
  defrules         = map (func2rule funcs) deffuncs
  newfdecls        = concatMap (transFDecl defrules) funcs

------------------------------------------------------------------------
-- implementation of deterministic function transformation:

-- Is the function declaration marked as a deterministic function?
isDetFun :: CFuncDecl -> Bool
isDetFun (CmtFunc _ qf ar vis texp rules) =
  isDetFun (CFunc qf ar vis texp rules)
isDetFun (CFunc _ _ _ texp _) = hasDetResultType texp
  where
   hasDetResultType (CTVar _) = False
   hasDetResultType (CFuncType _ rt) = hasDetResultType rt
   hasDetResultType (CTCons tc _) = tc == pre "DET"

-- translate a function (where the names of all deterministic functions
-- is provided as a first argument):
transDetFun :: [QName] -> CFuncDecl -> [CFuncDecl]
transDetFun detfnames (CmtFunc _ qf ar vis texp rules) =
  transDetFun detfnames (CFunc qf ar vis texp rules)
transDetFun detfnames fdecl@(CFunc qf@(mn,fn) ar vis texp rules)
 | qf `elem` detfnames
 = [CFunc qf ar vis (removeDetResultType texp) [newdetrule],
    CFunc neworgname ar Private (removeDetResultType texp) rules]
 | isDefault fdecl && (mn, default2orgname fn) `elem` detfnames
  -- rename default rule of a deterministic function:
 = [CFunc (mn, default2orgname fn ++ orgsuffix ++ "'default") ar vis texp rules]
 | otherwise = [fdecl]
 where
  -- new name for original function (TODO: check for unused name)
  neworgname = (mn,fn++orgsuffix)
  orgsuffix = "_ORGNDFUN"

  newdetrule =
    CRule (map CPVar argvars)
          (CSimpleRhs (applyF (setFunMod, "selectValue")
                              [applyF (setFunMod, "set"++show ar)
                                      (CSymbol neworgname : map CVar argvars)])
                      [])

  argvars = map (\i->(i,"x"++show i)) [1..ar]

removeDetResultType :: CTypeExpr -> CTypeExpr
removeDetResultType tv@(CTVar _) = tv
removeDetResultType (CFuncType t1 t2) =
  CFuncType (removeDetResultType t1) (removeDetResultType t2)
removeDetResultType (CTCons tc texps) =
  if tc == pre "DET"
  then head texps
  else CTCons tc (map removeDetResultType texps)


------------------------------------------------------------------------
-- implementation of default rule transformation:

isDefault :: CFuncDecl -> Bool
isDefault (CmtFunc _ qf ar vis texp rules) =
  isDefault (CFunc qf ar vis texp rules)
isDefault (CFunc (_,fname) _ _ _ _) = "'default" `isSuffixOf` fname

-- translate default name (i.e., with suffix 'default) into standard name:
default2orgname :: String -> String
default2orgname fname = reverse . drop 8 . reverse $ fname

-- Extract the arity and default rule for a default function definition:
func2rule :: [CFuncDecl] -> CFuncDecl -> (QName,(Int,CRule))
func2rule funcs (CFunc (mn,fn) ar _ _ rules)
  | (mn,defname) `notElem` map funcName funcs
   = error $
      "Default rule given for '"++defname++"' but no such function defined!"
  | null rules
   = error $ "Default rule for '"++defname++"' without right-hand side!"
  | length rules > 1
   = error $ "More than one default rule for function '"++defname++"'!"
  | otherwise = ((mn, defname), (ar, head rules))
 where defname = default2orgname fn
func2rule funcs (CmtFunc _ qf ar vis texp rules) =
  func2rule funcs (CFunc qf ar vis texp rules)

-- Translates a function declaration into a new one that respects
-- the potential default rule (which are provided as the first argument).
transFDecl :: [(QName,(Int,CRule))] -> CFuncDecl -> [CFuncDecl]
transFDecl defrules (CmtFunc _ qf ar vis texp rules) =
  transFDecl defrules (CFunc qf ar vis texp rules)
transFDecl defrules fdecl@(CFunc qf@(mn,fn) ar vis texp rules) =
  maybe [fdecl]
        (\ (dar,defrule) ->
           if dar /= ar
           then error $ "Default rule for '"++fn++"' has different arity!"
           else
              [CFunc neworgname ar Private texp rules,
               transFDecl2ApplyCond applyname fdecl,
               CFunc deffunname ar Private texp
                     [transDefaultRule applyname ar defrule],
               CFunc qf ar vis texp [neworgrule]])
        (lookup qf defrules)
 where
  -- new names for auxiliary functions (TODO: check for unused name)
  neworgname = (mn,fn++"_ORGRULES")
  applyname  = (mn,fn++"_APPLICABLE")
  deffunname = (mn,fn++"_DEFAULT")

  neworgrule =
    CRule (map CPVar argvars)
          (CSimpleRhs (applyF (pre "?")
                              [applyF neworgname (map CVar argvars),
                               applyF deffunname (map CVar argvars)])
                      [])

  argvars = map (\i->(i,"x"++show i)) [1..ar]

-- Translates a function declaration into one where the right-hand side
-- is always Prelude.(), i.e., it just checks for applicability.
-- The first argument is the new name of the translated function.
transFDecl2ApplyCond :: QName -> CFuncDecl -> CFuncDecl
transFDecl2ApplyCond nqf (CmtFunc _ qf ar vis texp rules) =
  transFDecl2ApplyCond nqf (CFunc qf ar vis texp rules)
transFDecl2ApplyCond nqf (CFunc _ ar _ texp rules) =
  CFunc nqf ar Private (adjustResultType texp) (map rule2cond rules)
 where
  rule2cond (CRule rpats (CSimpleRhs _ rlocals)) =
    let singlepatvars = extractSingles (concatMap varsOfPat rpats ++
                                        concatMap varsOfLDecl rlocals)
     in CRule (map (anonymPat singlepatvars) rpats)
              (CSimpleRhs preUnit rlocals)
  rule2cond (CRule rpats (CGuardedRhs gds rlocals)) =
    let singlepatvars = extractSingles (concatMap varsOfPat rpats ++
                                        concatMap (varsOfExp . fst) gds ++
                                        concatMap varsOfLDecl rlocals)
     in CRule (map (anonymPat singlepatvars) rpats)
              (CGuardedRhs (map (\gd -> (fst gd,preUnit)) gds) rlocals)

-- Adjust the result type of a type by setting the result type to ():
adjustResultType :: CTypeExpr -> CTypeExpr
adjustResultType texp =
  if texp == preUntyped
  then texp
  else case texp of
         CFuncType te1 te2 -> CFuncType te1 (adjustResultType te2)
         _                 -> unitType

transDefaultRule :: QName -> Int -> CRule -> CRule
transDefaultRule _ _ (CRule _ (CGuardedRhs _ _)) =
  error "Cannot yet transform guarded default rules!"
transDefaultRule condfunname ar (CRule pats (CSimpleRhs exp locals)) =
  CRule newpats (CGuardedRhs [(checkCond,exp)] locals)
 where
  checkCond = applyF (setFunMod,"isEmpty")
                     [applyF (setFunMod,"set"++show ar)
                             (CSymbol condfunname : args)]

  (newpats,args) = unzip (map arg2patexp (zip [1001..] pats))
  
  arg2patexp (i,pat) = case pat of
    CPVar v     -> if snd v=="_"
                     then let newvar = (i,"patvar_"++show i)
                           in (CPVar newvar, CVar newvar)
                     else (pat, CVar v)
    CPAs asv _  -> (pat, CVar asv)
    _           -> let newvar = (i,"patvar_"++show i)
                    in (CPAs newvar pat, CVar newvar)
  
preUnit :: CExpr
preUnit = CSymbol (pre "()")

preUntyped :: CTypeExpr
preUntyped = CTCons (pre "untyped") []

setFunMod :: String
setFunMod = "SetFunctions"

--- Extracts all elements with a single occurrence in a given list.
extractSingles :: [a] -> [a]
extractSingles [] = []
extractSingles (x:xs) =
  if null (filter (==x) xs)
  then x : extractSingles xs
  else extractSingles (filter (/=x) xs)

--- Replaces all variables occurring in the first argument by
--- anonymous variables in a pattern.
anonymPat :: [(Int,String)] -> CPattern -> CPattern
anonymPat vs (CPVar v) = CPVar (if v `elem` vs then (fst v,"_") else v)
anonymPat _  (CPLit l) = CPLit l
anonymPat vs (CPComb qc pats) = CPComb qc (map (anonymPat vs) pats)
anonymPat vs (CPAs v pat) =
  if v `elem` vs then anonymPat vs pat
                 else CPAs v (anonymPat vs pat)
anonymPat vs (CPFuncComb qf pats) = CPFuncComb qf (map (anonymPat vs) pats)
anonymPat vs (CPLazy pat) = CPLazy (anonymPat vs pat)
anonymPat vs (CPRecord qc recpats) =
  CPRecord qc (map (\ (n,p) -> (n, anonymPat vs p)) recpats)

------------------------------------------------------------------------
