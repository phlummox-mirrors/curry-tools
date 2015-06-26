-----------------------------------------------------------------------------
--- Translator to implement default rules
-----------------------------------------------------------------------------

import AbstractCurry
import AbstractCurryGoodies
import Directory
import Distribution
import List(isPrefixOf,partition)
import PrettyAbstract(showCProg)
import System

--------------------------------------------------------------------

banner :: String
banner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText = "Transformation Tool for Curry with Default Rules (Version of 25/06/15)"
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
  let newprog = showCProg (translateProg prog)
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
     [orgfile,infile,outfile] -> transPP orgfile infile outfile
     _ -> putStrLn $ banner ++
           "\nERROR: Illegal arguments for transformation: " ++
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
  putStrLnNQ banner
  prog <- readUntypedCurry progname
  system $ "cleancurry " ++ progname
  let transprog = showCProg (translateProg prog)
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
transPP :: String -> String -> String -> IO ()
transPP orgfile infile outfile = do
  let savefile = orgfile++".SAVE"
      modname = stripCurrySuffix orgfile
  renameFile orgfile savefile
  readFile infile >>= writeFile orgfile . replaceOptionsLine
  inputProg <- tryReadUntypedCurry modname savefile
  renameFile savefile orgfile
  writeFile outfile (showCProg (translateProg inputProg))
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

------------------------------------------------------------------------
-- Main transformation:

translateProg :: CurryProg -> CurryProg
translateProg (CurryProg mn imps tdecls fdecls ops) =
  CurryProg mn newimps tdecls newfdecls ops
 where
  newimps = if "SetFunctions" `elem` imps then imps else "SetFunctions":imps
  (deffuncs,funcs) = partition isDefault fdecls
  defrules = map (func2rule funcs) deffuncs
  newfdecls = concatMap (transFDecl defrules) funcs

isDefault :: CFuncDecl -> Bool
isDefault (CFunc (_,fname) _ _ _ _)     = "default_" `isPrefixOf` fname
isDefault (CmtFunc _ (_,fname) _ _ _ _) = "default_" `isPrefixOf` fname

func2rule :: [CFuncDecl] -> CFuncDecl -> (QName,CRule)
func2rule funcs (CFunc (mn,fn) _ _ _ rules)
  | (mn,defname) `notElem` map funcName funcs
   = error $
      "Default rule given for '"++defname++"' but no such function defined!"
  | null rules
   = error $ "Default rule for '"++defname++"' without right-hand side!"
  | length rules > 1
   = error $ "More than one default rule for function '"++defname++"'!"
  | otherwise = ((mn, defname), head rules)
 where defname = drop 8 fn
func2rule funcs (CmtFunc _ qf ar vis texp rules) =
  func2rule funcs (CFunc qf ar vis texp rules)

transFDecl :: [(QName,CRule)] -> CFuncDecl -> [CFuncDecl]
transFDecl defrules (CmtFunc _ qf ar vis texp rules) =
  transFDecl defrules (CFunc qf ar vis texp rules)
transFDecl defrules fdecl@(CFunc qf@(mn,fn) ar vis texp rules) =
  maybe [CFunc qf ar vis texp rules]
        (\defrule ->
	      [transFDecl2ApplyCond applyname fdecl,
               CFunc neworgname ar Private texp rules,
	       CFunc deffunname ar Private texp
                     [transDefaultRule applyname ar defrule],
	       CFunc qf ar vis texp [neworgrule]])
        (lookup qf defrules)
 where
  -- new names for auxiliary functions (TODO: check for unused name)
  applyname  = (mn,fn++"_ORGFUN")
  neworgname = (mn,fn++"_DEFAULT")
  deffunname = (mn,fn++"_APPLYCOND")

  neworgrule =
    CRule (map CPVar argvars)
          (CSimpleRhs (applyF (pre "?")
  	                      [applyF neworgname (map CVar argvars),
		   	       applyF deffunname (map CVar argvars)])
	              [])

  argvars = map (\i->(i,"x"++show i)) [1..ar]


transFDecl2ApplyCond :: QName -> CFuncDecl -> CFuncDecl
transFDecl2ApplyCond nqf (CmtFunc _ qf ar vis texp rules) =
  transFDecl2ApplyCond nqf (CFunc qf ar vis texp rules)
transFDecl2ApplyCond nqf (CFunc _ ar _ texp rules) =
  CFunc nqf ar Private (adjustResultType texp) (map rule2cond rules)
 where
  rule2cond (CRule rpats (CSimpleRhs _ rlocals)) =
    CRule rpats (CSimpleRhs preUnit rlocals)
  rule2cond (CRule rpats (CGuardedRhs gds rlocals)) =
    CRule rpats (CGuardedRhs (map (\gd -> (fst gd,preUnit)) gds) rlocals)

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
  checkCond =applyF ("SetFunctions","isEmpty")
                    [applyF ("SetFunctions","set"++show ar)
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
