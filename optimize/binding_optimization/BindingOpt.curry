-------------------------------------------------------------------------
--- Implementation of a transformation to replace Boolean equalities
--- by constrained equalities (which binds variables).
---
--- @author Michael Hanus
--- @version September 2014
-------------------------------------------------------------------------

module BindingOpt(main,transformFlatProg) where

import AnalysisServer(analyzeGeneric,analyzePublic,analyzeInterface)
import Analysis
import GenericProgInfo
import RequiredValues

import List
import System(getArgs,system,exitWith,getCPUTime)
import FileGoodies
import FlatCurry hiding (Cons)
import FlatCurryGoodies
import Distribution(installDir,curryCompiler,inCurrySubdir,currySubdir)
import FilePath(pathSeparator,(</>))

systemBanner =
  let bannerText = "Curry Binding Optimizer (version of 05/09/2014)"
      bannerLine = take (length bannerText) (repeat '=')
   in bannerLine ++ "\n" ++ bannerText ++ "\n" ++ bannerLine

usageComment =
  "Usage: bindingopt [-v<n>] [-l] <module or FlatCurry file names>\n" ++
  "       -v<n> : verbosity level (n=0|1|2|3)\n" ++
  "       -f    : fast transformation without analysis\n" ++
  "               (uses only infos about the standard prelude)\n" ++
  "       -l    : load optimized module into Curry system\n"

-- main function to call the optimizer:
main = do
  args <- getArgs
  checkArgs (1,True,False,"") args

mainCallError args = do
  putStrLn $
    systemBanner ++
    "\nIllegal arguments: " ++ concat (intersperse " " args) ++ "\n" ++
    usageComment
  exitWith 1

checkArgs :: (Int,Bool,Bool,String) -> [String] -> IO ()
checkArgs (verbosity,withana,load,prog) args = case args of
  [] -> mainCallError []
  (('-':'v':d:[]):margs) -> let v = ord d - ord '0'
                             in if v>=0 && v<=4
                                then checkArgs (v,withana,load,prog) margs
                                else mainCallError args
  ("-f":margs) -> checkArgs (verbosity,False,load,prog) margs
  ("-l":margs) -> checkArgs (verbosity,withana,True,prog) margs
  ("-h":_)     -> putStr (systemBanner++'\n':usageComment)
  ("-?":_)     -> putStr (systemBanner++'\n':usageComment)
  modnames     -> if null modnames
                  then mainCallError args
                  else do
                    printVerbose verbosity 1 systemBanner
                    mapIO_ (\mn -> transformBoolEq verbosity withana load mn)
                           modnames

-- Verbosity level:
-- 0 : show nothing
-- 1 : show summary of optimizations performed
-- 2 : show analysis infos and details of optimizations including timings
-- 3 : show analysis infos also of imported modules
-- 4 : show intermediate data (not yet used)

-- Output a string w.r.t. verbosity level
printVerbose :: Int -> Int -> String -> IO ()
printVerbose verbosity printlevel message =
  if null message || verbosity < printlevel then done else putStrLn message

transformBoolEq :: Int -> Bool -> Bool -> String -> IO ()
transformBoolEq verb withanalysis load name = do
  let isfcyname = fileSuffix name == "fcy"
      modname   = if isfcyname then stripCurrySubdir (stripSuffix name)
                               else stripSuffix name
  printVerbose verb 1 $ "Reading and analyzing module '"++modname++"'..."
  flatprog <- if isfcyname then readFlatCurryFile name
                           else readFlatCurry modname
  transformAndStoreFlatProg verb withanalysis load modname flatprog

stripCurrySubdir :: String -> String
stripCurrySubdir s =
 let (dirname,basename) = splitDirectoryBaseName s
     lcsd = length currySubdir
     rs   = reverse dirname
     swoc = reverse (drop lcsd rs)
  in if take lcsd rs == reverse currySubdir
     then if swoc==['.',pathSeparator] then basename else swoc </> basename
     else s

transformAndStoreFlatProg :: Int -> Bool -> Bool -> String -> Prog -> IO ()
transformAndStoreFlatProg verb withanalysis load modname prog = do
  let oldprogfile = inCurrySubdir (modname++".fcy")
      newprogfile = inCurrySubdir (modname++"_O.fcy")
  starttime <- getCPUTime
  (newprog,transformed) <- transformFlatProg verb withanalysis modname prog
  when transformed $ writeFCY newprogfile newprog
  stoptime <- getCPUTime
  printVerbose verb 2 $ "Transformation time for " ++ modname ++ ": " ++
                        show (stoptime-starttime) ++ " msecs"
  when transformed $ do
    printVerbose verb 2 $ "Transformed program stored in "++newprogfile
    system $ "mv "++newprogfile++" "++oldprogfile
    printVerbose verb 2 $ " ...and moved to "++oldprogfile
  if load
   then system (installDir++"/bin/"++curryCompiler++" :l "++modname) >> done
   else done

-- Perform the binding optimization on a FlatCurry program.
-- Return the new FlatCurry program and a flag indicating whether
-- something has been changed.
transformFlatProg :: Int -> Bool -> String -> Prog -> IO (Prog,Bool)
transformFlatProg verb withanalysis modname
                  (Prog mname imports tdecls fdecls opdecls)= do
  lookupreqinfo <-
    if withanalysis
    then do (mreqinfo,reqinfo) <- loadAnalysisWithImports reqValueAnalysis
                                                          modname imports
            printVerbose verb 2 $ "\nResult of \"RequiredValue\" analysis:\n"++
                                  showInfos (showAFType AText)
                                     (if verb==3 then reqinfo else mreqinfo)
            return (flip lookupProgInfo reqinfo)
    else return (flip lookup preludeBoolReqValues)
  let (ons,cmts,newfdecls) = unzip3 (map (transformFuncDecl verb lookupreqinfo)
                                         fdecls)
      numtrans = foldr (+) 0 ons
  printVerbose verb 1 $ unlines (filter (not . null) cmts)
  printVerbose verb 1 $ "Total number of transformations: " ++ show numtrans
  return (Prog mname imports tdecls newfdecls opdecls, numtrans>0)

loadAnalysisWithImports :: Analysis a -> String -> [String]
                        -> IO (ProgInfo a,ProgInfo a)
loadAnalysisWithImports analysis modname imports = do
  maininfo <- analyzeGeneric analysis modname >>= return . either id error
  impinfos <- mapIO (\m -> analyzePublic analysis m >>=
                                                     return . either id error)
                    imports
  return $ (maininfo, foldr1 combineProgInfo (maininfo:impinfos))

showInfos :: (a -> String) -> ProgInfo a -> String
showInfos showi =
  unlines . map (\ (qn,i) -> snd qn ++ ": " ++ showi i)
          . (\p -> fst p ++ snd p) . progInfo2Lists

-- Transform a function declaration.
-- The number of transformed occurrences of (==), a comment, and
-- the new function declaration are returned.
transformFuncDecl :: Int -> (QName -> Maybe AFType) -> FuncDecl
                  -> (Int,String,FuncDecl)
transformFuncDecl verb lookupreqinfo fdecl@(Func qf ar vis texp rule) =
  if containsBeqRule rule
  then
    let (tst,trule) = transformRule lookupreqinfo (initTState qf) rule
        on = occNumber tst
     in (on,
         if verb <= 1 then "" else
           if on==0 then "" else
             "Function "++snd qf++": "++
             (if on==1 then "one occurrence" else show on++" occurrences") ++
             " of (==) transformed into (=:=)",
         Func qf ar vis texp trule)
  else (0,"",fdecl)

-------------------------------------------------------------------------
-- State threaded thorugh the program transformer:
-- * name of current function
-- * number of occurrences of (==) that are replaced by (=:=)
data TState = TState QName Int

initTState qf = TState qf 0

occNumber (TState _ on) = on

incOccNumber (TState qf on) = TState qf (on+1)

-------------------------------------------------------------------------
--- Transform a FlatCurry program rule w.r.t. information about required
--- values. If there is an occurrence of (e1==e2) where the value `True`
--- is required, then this occurrence is replaced by
---
---     ((e1=:=e2) &> True)
---
--- Similarly, (e1/=e2) with required value `False` is replaced by
---
---     ((e1=:=e2) &> False)

transformRule :: (QName -> Maybe AFType) -> TState -> Rule -> (TState,Rule)
transformRule _ tst (External s) = (tst, External s)
transformRule lookupreqinfo tstr (Rule args rhs) =
  let (te,tste) = transformExp tstr rhs Any
   in (tste, Rule args te)
 where
  -- transform an expression w.r.t. a required value
  transformExp tst (Var i) _ = (Var i, tst)
  transformExp tst (Lit v) _ = (Lit v, tst)
  transformExp tst0 (Comb ct qf es) reqval =
    let reqtype     = maybe AnyFunc id (lookupreqinfo qf)
        reqargtypes = argumentTypesFor reqtype reqval
        (tes,tst1)  = transformExps tst0 (zip es reqargtypes)
     in if (qf == pre "==" && reqval == RequiredValues.Cons (pre "True")) ||
           (qf == pre "/=" && reqval == RequiredValues.Cons (pre "False"))
        then (Comb FuncCall (pre "&>")
               [Comb FuncCall (pre "=:=") tes,
                let RequiredValues.Cons qcons = reqval
                 in Comb ConsCall qcons []],
              incOccNumber tst1)
        else if qf == pre "$" && length es == 2 &&
                (isFuncPartCall (head es) || isConsPartCall (head es))
             then transformExp tst0 (reduceDollar es) reqval
             else (Comb ct qf tes, tst1)
  transformExp tst0 (Free vars e) reqval =
    let (te,tst1) = transformExp tst0 e reqval
     in (Free vars te, tst1)
  transformExp tst0 (Or e1 e2) reqval =
    let (te1,tst1) = transformExp tst0 e1 reqval
        (te2,tst2) = transformExp tst1 e2 reqval
     in (Or te1 te2, tst2)
  transformExp tst0 (Typed e t) reqval =
    let (te,tst1) = transformExp tst0 e reqval
     in (Typed te t, tst1)
  transformExp tst0 (Case ct e bs) reqval =
    let (te ,tst1) = transformExp tst0 e (caseArgType bs)
        (tbs,tst2) = transformBranches tst1 bs reqval
     in (Case ct te tbs, tst2)
  transformExp tst0 (Let bs e) reqval =
    let (tbes,tst1) = transformExps tst0 (zip (map snd bs) (repeat Any))
        (te,tst2) = transformExp tst1 e reqval
     in (Let (zip (map fst bs) tbes) te, tst2)

  transformExps tst [] = ([],tst)
  transformExps tst ((exp,rv):exps) =
    let (te, tste ) = transformExp tst exp rv
        (tes,tstes) = transformExps tste exps
     in (te:tes, tstes)

  transformBranches tst [] _ = ([],tst)
  transformBranches tst (br:brs) reqval =
    let (tbr,tst1) = transformBranch tst br reqval
        (tbrs,tst2) = transformBranches tst1 brs reqval
     in (tbr:tbrs, tst2)

  transformBranch tst (Branch pat be) reqval =
    let (tbe,tstb) = transformExp tst be reqval
     in (Branch pat tbe, tstb)

--- Reduce an application of Prelude.$ to a combination:
reduceDollar [Comb (FuncPartCall n) qf es, arg2] =
  Comb (if n==1 then FuncCall else (FuncPartCall (n-1))) qf (es++[arg2])
reduceDollar [Comb (ConsPartCall n) qf es, arg2] =
  Comb (if n==1 then ConsCall else (ConsPartCall (n-1))) qf (es++[arg2])

--- Try to compute the required value of a case argument expression.
--- If the case expression has one non-failing branch, the branch
--- constructor is chosen, otherwise it is `Any`.
caseArgType branches =
  let nfbranches = filter (\ (Branch _ be) ->
                                   be /= Comb FuncCall (pre "failed") [])
                          branches
   in if length nfbranches /= 1 then Any else getPatCons (head nfbranches)
 where
   getPatCons (Branch (Pattern qc _) _) = Cons qc --RequiredValues.Cons
   getPatCons (Branch (LPattern _)   _) = Any

--- Compute the argument types for a given abstract function type
--- and required value.
argumentTypesFor :: AFType -> AType -> [AType]
argumentTypesFor AnyFunc _ = repeat Any
argumentTypesFor (AFType rtypes) reqval =
  maybe (-- no exactly matching type, look for Any type:
         maybe (-- no Any type: if reqtype==Any, try lub of all other types:
                if reqval==Any && not (null rtypes)
                then foldr1 lubArgs (map fst rtypes)
                else repeat Any)
               fst
               (find ((==Any) . snd) rtypes))
        fst
        (find ((==reqval) . snd) rtypes)
 where
  lubArgs xs ys = map (uncurry lubAType) (zip xs ys)


-- Does Prelude.== occur in a rule?
containsBeqRule :: Rule -> Bool
containsBeqRule (External _) = False
containsBeqRule (Rule _ rhs) = containsBeqExp rhs
 where
  -- containsBeq an expression w.r.t. a required value
  containsBeqExp exp = case exp of
    Var _ -> False
    Lit _ -> False
    Comb _ qf es -> qf == pre "==" || qf == pre "/=" || any containsBeqExp es
    Free _ e -> containsBeqExp e
    Or e1 e2 -> containsBeqExp e1 || containsBeqExp e2
    Typed e _ -> containsBeqExp e
    Case _ e bs -> containsBeqExp e || any containsBeqBranch bs
    Let bs e -> containsBeqExp e || any containsBeqExp (map snd bs)

  containsBeqBranch (Branch _ be) = containsBeqExp be

pre n = ("Prelude",n)

-------------------------------------------------------------------------
-- Loading prelude analysis result:
loadPreludeBoolReqValues = do
  maininfo <- analyzeInterface reqValueAnalysis "Prelude" >>=
                                                return . either id error
  return (filter (hasBoolReqValue . snd) maininfo)
 where
  hasBoolReqValue AnyFunc = False
  hasBoolReqValue (AFType rtypes) =
    maybe False (const True) (find (isBoolReqValue . snd) rtypes)

  isBoolReqValue rt = case rt of
    Cons qc -> qc `elem` [pre "True", pre "False"]
    _       -> False

-- Current relevant Boolean functions of the prelude:
preludeBoolReqValues =
 [((pre "&&"),(AFType [([Any,Any],(Cons (pre "False"))),
           ([(Cons (pre "True")),(Cons (pre "True"))],(Cons (pre "True")))]))
 ,((pre "not"),(AFType [([(Cons (pre "True"))],(Cons (pre "False"))),
                        ([(Cons (pre "False"))],(Cons (pre "True")))]))
 ,((pre "||"),
    (AFType [([(Cons (pre "False")),(Cons (pre "False"))],(Cons (pre "False"))),
             ([Any,Any],(Cons (pre "True")))]))
 ,((pre "solve"),(AFType [([(Cons (pre "True"))],(Cons (pre "True")))]))
 ,((pre "&&>"),(AFType [([(Cons (pre "True")),Any],Any)]))
 ]
