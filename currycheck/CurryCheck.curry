-------------------------------------------------------------------------
--- This is the implementation of the currycheck tool.
--- This tool extracts all EasyCheck tests contained in a Curry module
--- and transforms them such that they are automatically tested
--- and the test results are reported.
--- Moreover, for all functions declared as deterministic,
--- determinism properties are generated and checked.
---
--- @author Michael Hanus, Jan-Patrick Baye
--- @version February 2016
-------------------------------------------------------------------------

import AbstractCurry.Types
import AbstractCurry.Files
import AbstractCurry.Select
import AbstractCurry.Build
import AbstractCurry.Pretty    (showCProg)
import AbstractCurry.Transform (renameCurryModule)
import UsageCheck              (checkBlacklistUse, checkSetUse)
import Distribution
import qualified FlatCurry.Types as FC
import FlatCurry.Files
import qualified FlatCurry.Goodies as FCG
import GetOpt
import IO
import List
import Maybe                   (fromJust, isJust)
import ReadNumeric             (readNat)
import System                  (system, exitWith, getArgs, getPID)

--- Maximal arity of check functions and tuples currently supported:
maxArity :: Int
maxArity = 5

-- Banner of this tool:
ccBanner :: String
ccBanner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText =
     "CurryCheck: a tool for testing Curry programs (version of 25/02/2016)"
   bannerLine = take (length bannerText) (repeat '-')

-- Help text
usageText :: String
usageText = usageInfo ("Usage: currycheck [options] <module names>\n") options
  
-------------------------------------------------------------------------
-- Representation of command line options.
data Options = Options
  { optHelp    :: Bool
  , optVerb    :: Int
  , optMaxTest :: Int
  , optMaxFail :: Int
  , optDefType :: String
  , optSource  :: Bool
  , optProp    :: Bool
  , optSpec    :: Bool
  , optDet     :: Bool
  }

-- Default command line options.
defaultOptions :: Options
defaultOptions = Options
  { optHelp    = False
  , optVerb    = 1
  , optMaxTest = 0
  , optMaxFail = 0
  , optDefType = "Bool"
  , optSource  = True
  , optProp    = True
  , optSpec    = True
  , optDet     = True
  }

-- Definition of actual command line options.
options :: [OptDescr (Options -> Options)]
options =
  [ Option "h?" ["help"]  (NoArg (\opts -> opts { optHelp = True }))
           "print help and exit"
  , Option "q" ["quiet"] (NoArg (\opts -> opts { optVerb = 0 }))
           "run quietly (no output, only exit code)"
  , Option "v" ["verbosity"]
            (OptArg (maybe (checkVerb 2) (safeReadNat checkVerb)) "<n>")
            "verbosity level:\n0: quiet (same as `-q')\n1: show test names (default)\n2: show test data (same as `-v')\n3: keep intermediate program files"
  , Option "m" ["maxtests"]
           (ReqArg (safeReadNat (\n opts -> opts { optMaxTest = n })) "<n>")
           "maximal number of tests (default: 100)"
  , Option "f" ["maxfails"]
           (ReqArg (safeReadNat (\n opts -> opts { optMaxFail = n })) "<n>")
           "maximal number of condition failures\n(default: 10000)"
  , Option "d" ["deftype"]
            (ReqArg checkDefType "<t>")
           "type for defaulting polymorphic tests:\nBool (default) | Int | Char"
  , Option "" ["nosource"]
           (NoArg (\opts -> opts { optSource = False }))
           "do not perform source code checks"
  , Option "" ["noprop"]
           (NoArg (\opts -> opts { optProp = False }))
           "do not perform any property tests"
  , Option "" ["nospec"]
           (NoArg (\opts -> opts { optSpec = False }))
           "do not perform specification/postcondition tests"
  , Option "" ["nodet"]
           (NoArg (\opts -> opts { optDet = False }))
           "do not perform determinism tests"
  ]
 where
  safeReadNat opttrans s opts =
   let numError = error "Illegal number argument (try `-h' for help)" in
    maybe numError
          (\ (n,rs) -> if null rs then opttrans n opts else numError)
          (readNat s)

  checkVerb n opts = if n>=0 && n<4
                     then opts { optVerb = n }
                     else error "Illegal verbosity level (try `-h' for help)"

  checkDefType s opts = if s `elem` ["Bool","Int","Char"]
                        then opts { optDefType = s }
                        else error "Illegal default type (try `-h' for help)"

isQuiet :: Options -> Bool
isQuiet opts = optVerb opts == 0

--- Print second argument if verbosity level is not quiet:
putStrIfNormal :: Options -> String -> IO ()
putStrIfNormal opts s = unless (isQuiet opts) (putStr s >> hFlush stdout)

--- Print second argument if verbosity level > 1:
putStrIfVerbose :: Options -> String -> IO ()
putStrIfVerbose opts s = when (optVerb opts > 1) (putStr s >> hFlush stdout)

-------------------------------------------------------------------------
-- The names of suffixes added to specific tests.

defTypeSuffix :: String
defTypeSuffix = "_ON_BASETYPE"

postCondSuffix :: String
postCondSuffix = "SatisfiesPostCondition"

satSpecSuffix :: String
satSpecSuffix = "SatisfiesSpecification"

isDetSuffix :: String
isDetSuffix = "IsDeterministic"

-------------------------------------------------------------------------
-- Internal representation of tests extracted from a Curry module.
-- A test is either a property test (with a name, type, source line number)
-- or an IO test (with a name and source line number).
data Test = PropTest QName CTypeExpr Int | AssertTest QName Int

-- The name of a test:
getTestName :: Test -> QName
getTestName (PropTest   n _ _) = n
getTestName (AssertTest n   _) = n

-- The line number of a test:
getTestLine :: Test -> Int
getTestLine (PropTest   _ _ n) = n
getTestLine (AssertTest _   n) = n

-- Generates a useful error message for tests (with module and line number)
genTestMsg :: String -> Test -> String
genTestMsg file test =
  snd (getTestName test) ++
  " (module " ++ file ++ ", line " ++ show (getTestLine test) ++ ")"

-------------------------------------------------------------------------
-- Representation of the information about a module to be tested:
-- * the original name of the module to be tested
-- * the name of the transformed (public) test module
-- * static errors (e.g., illegal uses of set functions)
-- * test operations
-- * name of generators defined in this module (i.e., starting with "gen"
--   and of appropriate result type)
data TestModule = TestModule
  { orgModuleName  :: String
  , testModuleName :: String
  , staticErrors   :: [String]
  , propTests      :: [Test]
  , generators     :: [QName]
  }

-- A test module with only static errors.
staticErrorTestMod :: String -> [String] -> TestModule
staticErrorTestMod modname staterrs =
 TestModule modname modname staterrs [] []

-- Is this a test module that should be tested?
testThisModule :: TestModule -> Bool
testThisModule tm = null (staticErrors tm) && not (null (propTests tm))

-- Extracts all user data types used as test data generators.
userTestDataOfModule :: TestModule -> [QName]
userTestDataOfModule testmod = concatMap testDataOf (propTests testmod)
 where
  testDataOf (AssertTest _ _) = []
  testDataOf (PropTest _ texp _) = unionOn userTypesOf (argTypes texp)

  userTypesOf (CTVar _) = []
  userTypesOf (CFuncType from to) = union (userTypesOf from) (userTypesOf to)
  userTypesOf (CTCons (mn,tc) argtypes) =
    union (if mn == preludeName then [] else [(mn,tc)])
          (unionOn userTypesOf argtypes)

  unionOn f = foldr union [] . map f

-------------------------------------------------------------------------
-- Transform all tests of a module into an appropriate call of EasyCheck:
createTests :: Options -> String -> TestModule -> [CFuncDecl]
createTests opts mainmodname tm = map createTest (propTests tm)
 where
  createTest test =
    cfunc (mainmodname, (genTestName $ getTestName test)) 0 Public
          (ioType (maybeType stringType))
          (case test of PropTest   name t _ -> propBody name (argTypes t) test
                        AssertTest name   _ -> assertBody name test)

  msgOf test = string2ac $ genTestMsg (orgModuleName tm) test

  testmname = testModuleName tm
  
  genTestName (modName, fName) = fName ++ "_" ++ modNameToId modName

  easyCheckFuncName arity =
    if arity>maxArity
    then error $ "Properties with more than " ++ show maxArity ++
                 " parameters are currently not supported!"
    else (easyCheckModule,"checkWithValues" ++ show arity)

  propBody (_, name) argtypes test =
    [simpleRule [] $
       CLetDecl [CLocalPat (CPVar msgvar) (CSimpleRhs (msgOf test) [])]
                (applyF (easyCheckModule,"checkPropWithMsg")
                  [CVar msgvar
                  ,applyF (easyCheckFuncName (length argtypes)) $
                     [configOpWithMaxFail, CVar msgvar] ++
                     (map (\t ->
                           applyF (easyCheckModule,"valuesOfSearchTree")
                             [if isPAKCS || useUserDefinedGen t
                              then type2genop mainmodname tm t
                              else applyF (searchTreeModule,"someSearchTree")
                                          [constF (pre "unknown")]])
                          argtypes) ++
                     [CSymbol (testmname,name)]
                  ])]
   where
    useUserDefinedGen texp = case texp of
      CTVar _         -> error "No polymorphic generator!"
      CFuncType _ _   -> error "No generator for functional types!"
      CTCons (_,tc) _ -> isJust
                           (find (\qn -> "gen"++tc == snd qn) (generators tm))

    configOpWithMaxTest =
      let n = optMaxTest opts
       in if n==0 then stdConfigOp
                  else applyF (easyCheckModule,"setMaxTest")
                              [cInt n, stdConfigOp]
                            
    configOpWithMaxFail =
      let n = optMaxFail opts
       in if n==0 then configOpWithMaxTest
                  else applyF (easyCheckModule,"setMaxFail")
                              [cInt n, configOpWithMaxTest]
                            
    msgvar = (0,"msg")
    
  stdConfigOp = constF (easyCheckConfig opts)
    
  assertBody (_, name) test =
    [simpleRule [] $ applyF (easyCheckModule, "checkPropIOWithMsg")
                            [stdConfigOp, msgOf test, CSymbol (testmname,name)]]

-- The configuration option for EasyCheck
easyCheckConfig :: Options -> QName
easyCheckConfig opts =
  (easyCheckModule, if isQuiet opts     then "quietConfig"   else
                    if optVerb opts > 1 then "verboseConfig"
                                        else "easyConfig")

-- Translates a type expression into calls to generator operations.
type2genop :: String -> TestModule -> CTypeExpr -> CExpr
type2genop _ _ (CTVar _)       = error "No polymorphic generator!"
type2genop _ _ (CFuncType _ _) = error "No generator for functional types!"
type2genop mainmod tm (CTCons qt targs) =
  applyF (typename2genopname mainmod (generators tm) qt)
         (map (type2genop mainmod tm) targs)

typename2genopname :: String -> [QName] -> QName -> QName
typename2genopname mainmod definedgenops (mn,tc)
  | isJust maybeuserdefined -- take user-defined generator:
  = fromJust maybeuserdefined
  | mn==preludeName &&
    tc `elem` ["Bool","Int","Char","Maybe","Either","Ordering"]
  = (generatorModule, "gen" ++ tc)
  | mn==preludeName && tc `elem` ["[]","()","(,)","(,,)","(,,,)","(,,,,)"]
  = (generatorModule, "gen" ++ transTC tc)
  | otherwise -- we use our own generator:
  = (mainmod, "gen_" ++ modNameToId mn ++ "_" ++ tc)
 where
  maybeuserdefined = find (\qn -> "gen"++tc == snd qn) definedgenops
  
  transTC tcons | tcons == "[]"     = "List"
                | tcons == "()"     = "Unit"
                | tcons == "(,)"    = "Pair"
                | tcons == "(,,)"   = "Triple"
                | tcons == "(,,,)"  = "Tuple4"
                | tcons == "(,,,,)" = "Tuple5"

-------------------------------------------------------------------------
-- Turn all functions into public ones.
-- This ensures that all tests can be executed.
makeAllPublic :: CurryProg -> CurryProg
makeAllPublic (CurryProg modname imports typedecls functions opdecls) =
  CurryProg modname stimports typedecls publicFunctions opdecls
 where
  stimports = if generatorModule `elem` imports &&
                 searchTreeModule `notElem` imports
              then searchTreeModule : imports -- just to be safe if module
                                              -- contains generator definitions
              else imports

  publicFunctions = map makePublic $ map ignoreComment functions

  -- since we create a copy of the module, we can ignore unnecessary data
  ignoreComment :: CFuncDecl -> CFuncDecl
  ignoreComment (CmtFunc _ name arity visibility typeExpr rules) =
    CFunc name arity visibility typeExpr rules
  ignoreComment x@(CFunc      _     _          _        _     _) = x

  makePublic :: CFuncDecl -> CFuncDecl
  makePublic (CFunc name arity _      typeExpr rules) =
              CFunc name arity Public typeExpr rules
  makePublic (CmtFunc cmt name arity _      typeExpr rules) =
              CmtFunc cmt name arity Public typeExpr rules

-- Check if a function definition is a property that should be tested,
-- i.e., if the result type is Prop (= [Test]) or PropIO.
isTest :: CFuncDecl -> Bool
isTest = isTestType . funcType
 where
  isTestType :: CTypeExpr -> Bool
  isTestType ct = isPropIOType ct || resultType ct == propType

-- The type of EasyCheck properties.
propType :: CTypeExpr
propType = listType (baseType (easyCheckModule, "Test"))
   
isPropIOType :: CTypeExpr -> Bool
isPropIOType texp = case texp of
    CTCons tcons [] -> tcons == (easyCheckModule,"PropIO")
    _               -> False

-- classify the tests as either PropTest or AssertTest
classifyTests :: [CFuncDecl] -> [Test]
classifyTests = map makeProperty
 where
  makeProperty test = if isPropIOType (funcType test)
                        then assertion test
                        else property test

  property  f = PropTest (funcName f) (funcType f) 0

  assertion f = AssertTest (funcName f) 0

-- Extracts all tests and transforms all polymorphic tests into tests on a
-- base type.
-- The result contains a pair consisting of all actual tests and all
-- ignored tests.
transformTests :: Options -> CurryProg -> ([CFuncDecl],[CFuncDecl],CurryProg)
transformTests opts (CurryProg mname imports typeDecls functions opDecls) =
  (map snd realtests, map snd ignoredtests,
   CurryProg mname
             (nub (easyCheckModule:imports))
             typeDecls
             (funcs ++ map snd (realtests ++ ignoredtests))
             opDecls)
 where
  (usertests, funcs) = partition isTest functions

  preCondOps = preCondOperations functions
  
  postCondOps = map ((\ (mn,fn) -> (mn,stripSuffix fn "'post")) . funcName)
                    (filter (\fd -> "'post" `isSuffixOf` snd (funcName fd))
                            functions)

  specOps    = map ((\ (mn,fn) -> (mn,stripSuffix fn "'spec")) . funcName)
                   (filter (\fd -> "'spec" `isSuffixOf` snd (funcName fd))
                           functions)

  -- generate post condition tests:
  postCondTests = concatMap (genPostCondTest preCondOps postCondOps) functions

  -- generate specification tests:
  specOpTests = concatMap (genSpecTest preCondOps specOps) functions

  (realtests,ignoredtests) = partition fst $
    if not (optProp opts)
    then []
    else concatMap (poly2default (optDefType opts)) $
           usertests ++
           (if optSpec opts then postCondTests ++ specOpTests else [])

-- Extracts all determinism tests and transforms deterministic operations
-- backinto non-deterministic operations.
-- The result contains a pair consisting of all actual determinism tests
-- and all ignored tests (since they are polymorphic).
transformDetTests :: Options -> CurryProg -> ([CFuncDecl],[CFuncDecl],CurryProg)
transformDetTests opts (CurryProg mname imports typeDecls functions opDecls) =
  (map snd realtests, map snd ignoredtests,
   CurryProg mname
             (nub (easyCheckModule:imports))
             typeDecls
             (map (revertDetOpTrans detOpNames) functions ++
              map snd (realtests ++ ignoredtests))
             opDecls)
 where
  preCondOps = preCondOperations functions
  
  -- generate determinism tests:
  detOpTests = genDetOpTests preCondOps functions

  -- names of deterministic operations:
  detOpNames = map (stripIsDet . funcName) detOpTests

  stripIsDet (mn,fn) = (mn, take (length fn -15) fn)

  (realtests,ignoredtests) = partition fst $
    if not (optProp opts)
    then []
    else concatMap (poly2default (optDefType opts))
                   (if optDet opts then detOpTests else [])

-- Get all operations with a defined precondition from a list of functions.
preCondOperations :: [CFuncDecl] -> [QName]
preCondOperations fdecls =
  map ((\ (mn,fn) -> (mn,stripSuffix fn "'pre")) . funcName)
      (filter (\fd -> "'pre" `isSuffixOf` snd (funcName fd))
              fdecls)

-- Transforms a function type into a property type, i.e.,
-- t1 -> ... -> tn -> t  is transformed into  t1 -> ... -> tn -> Prop
propResultType :: CTypeExpr -> CTypeExpr
propResultType (CTVar _) = propType
propResultType (CTCons _ _) = propType
propResultType (CFuncType from to) = CFuncType from (propResultType to)


-- Transforms a function declaration into a post condition test if
-- there is a post condition for this function (i.e., a relation named
-- f'post). The specification test is of the form
-- fSatisfiesPostCondition x1...xn y =
--   let r = f x1...xn
--    in f'pre x1...xn && r==r  ==> always (f'post x1...xn r)
genPostCondTest :: [QName] -> [QName] -> CFuncDecl -> [CFuncDecl]
genPostCondTest prefuns postops (CmtFunc _ qf ar vis texp rules) =
  genSpecTest prefuns postops (CFunc qf ar vis texp rules)
genPostCondTest prefuns postops (CFunc qf@(mn,fn) ar _ texp _) =
 if qf `notElem` postops then [] else
  [CFunc (mn, fn ++ postCondSuffix) ar Public
    (propResultType texp)
    [simpleRule (map CPVar cvars) $
      CLetDecl [CLocalPat (CPVar rvar)
                          (CSimpleRhs (applyF qf (map CVar cvars)) [])]
               (applyF (easyCheckModule,"==>")
                       [preCond,
                        applyF (easyCheckModule,"always")
                               [applyF (mn,fn++"'post")
                                       (map CVar (cvars ++ [rvar]))]])]]
 where
  cvars = map (\i -> (i,"x"++show i)) [1 .. ar]
  rvar  = (0,"r")

  preCond =
   let eqResult = applyF (pre "==") [CVar rvar, CVar rvar]
    in if qf `elem` prefuns
       then applyF (pre "&&")
                   [applyF (mn,fn++"'pre") (map CVar cvars), eqResult]
       else eqResult

-- Transforms a function declaration into a specification test if
-- there is a specification for this function (i.e., an operation named
-- f'spec). The specification test is of the form
-- fSatisfiesSpecification x1...xn =
--   f'pre x1...xn  ==> (f x1...xn <~> f'spec x1...xn)
genSpecTest :: [QName] -> [QName] -> CFuncDecl -> [CFuncDecl]
genSpecTest prefuns specops (CmtFunc _ qf ar vis texp rules) =
  genSpecTest prefuns specops (CFunc qf ar vis texp rules)
genSpecTest prefuns specops (CFunc qf@(mn,fn) ar _ texp _) =
 if qf `notElem` specops then [] else
  [CFunc (mn, fn ++ satSpecSuffix) ar Public
    (propResultType texp)
    [simpleRule (map CPVar cvars) $
       addPreCond (applyF (pre "<~>")
                          [applyF qf (map CVar cvars),
                           applyF (mn,fn++"'spec") (map CVar cvars)])]]
 where
  cvars = map (\i -> (i,"x"++show i)) [1 .. ar]

  addPreCond exp = if qf `elem` prefuns
                   then applyF (easyCheckModule,"==>")
                               [applyF (mn,fn++"'pre") (map CVar cvars), exp]
                   else exp

-- Revert the transformation for deterministic operations performed
-- by currypp, i.e., replace rule "f x = selectValue (set f_ORGNDFUN x)"
-- with "f = f_ORGNDFUN".
revertDetOpTrans :: [QName] -> CFuncDecl -> CFuncDecl
revertDetOpTrans  detops (CmtFunc _ qf ar vis texp rules) =
  revertDetOpTrans detops (CFunc qf ar vis texp rules)
revertDetOpTrans detops fdecl@(CFunc qf@(mn,fn) ar vis texp _) =
  if qf `elem` detops
  then CFunc qf ar vis texp [simpleRule [] (constF (mn,fn++"_ORGNDFUN"))]
  else fdecl

-- Look for operations named f_ORGNDFUN and create a determinism property
-- for f.
genDetOpTests :: [QName] -> [CFuncDecl] -> [CFuncDecl]
genDetOpTests prefuns fdecls =
  map (genDetProp prefuns) (filter isDetOrgOp fdecls)
 where
  isDetOrgOp fdecl = "_ORGNDFUN" `isSuffixOf` snd (funcName fdecl)

-- Transforms a declaration of a deterministic operation f_ORGNDFUN
-- into a determinisim property test of the form
-- fIsDeterministic x1...xn = let r = f x1...xn
--                             in r==r ==> always (f x1...xn == r)
genDetProp :: [QName] -> CFuncDecl -> CFuncDecl
genDetProp prefuns (CmtFunc _ qf ar vis texp rules) =
  genDetProp prefuns (CFunc qf ar vis texp rules)
genDetProp prefuns (CFunc (mn,fn) ar _ texp _) =
  CFunc (mn, forg ++ isDetSuffix) ar Public
   (propResultType texp)
   [simpleRule (map CPVar cvars) $
      CLetDecl [CLocalPat (CPVar rvar) (CSimpleRhs forgcall [])]
               (applyF (easyCheckModule,"==>")
                       [preCond,
                        applyF (easyCheckModule,"always")
                               [applyF (pre "==") [forgcall, CVar rvar]]])]
 where
  forg  = take (length fn - 9) fn
  forgcall = applyF (mn,forg) (map CVar cvars)
  cvars = map (\i -> (i,"x"++show i)) [1 .. ar]
  rvar  = (0,"r")

  preCond =
   let eqResult = applyF (pre "==") [CVar rvar, CVar rvar]
    in if (mn,forg) `elem` prefuns
       then applyF (pre "&&")
                   [applyF (mn,forg++"'pre") (map CVar cvars), eqResult]
       else eqResult


-- Generates auxiliary (Boolean-instantiated) test functions for
-- polymorphically typed test function.
-- The flag indicates whether the test function should be actually passed
-- to the test tool.
poly2default :: String -> CFuncDecl -> [(Bool,CFuncDecl)]
poly2default dt (CmtFunc _ name arity vis ftype rules) =
  poly2default dt (CFunc name arity vis ftype rules)
poly2default dt fdecl@(CFunc (mn,fname) arity vis ftype _)
  | isPolyType ftype
  = [(False,fdecl)
    ,(True, CFunc (mn,fname++defTypeSuffix) arity vis (p2dt ftype)
                     [simpleRule [] (applyF (mn,fname) [])])
    ]
  | otherwise
  = [(True,fdecl)]
 where
  p2dt (CTVar _) = baseType (pre dt)
  p2dt (CFuncType t1 t2) = CFuncType (p2dt t1) (p2dt t2)
  p2dt (CTCons ct ts) = CTCons ct (map p2dt ts)

-- Transforms a possibly changed test name (like "test_ON_BASETYPE")
-- back to its original name.
orgTestName :: QName -> QName
orgTestName (mn,tname)
  | defTypeSuffix `isSuffixOf` tname
  = orgTestName (mn, stripSuffix tname defTypeSuffix)
  | isDetSuffix `isSuffixOf` tname
  = orgTestName (mn, take (length tname - 15) tname)
  | postCondSuffix `isSuffixOf` tname
  = orgTestName (mn, stripSuffix tname postCondSuffix)
  | satSpecSuffix `isSuffixOf` tname
  = orgTestName (mn, stripSuffix tname satSpecSuffix)
  | otherwise = (mn,tname)

-- This function implements the first phase of CurryCheck: it analyses
-- a module to be checked, i.e., it finds the tests, creates new tests
-- (e.g., for polymorphic properties, deterministic functions, post
-- conditions, specifications)
-- and generates a copy of the module appropriate for the main operation
-- of CurryCheck (e.g., all operations are made public).
-- If there are determinism tests, it generates also a second copy
-- where all deterministic functions are defined as non-deterministic
-- so that these definitions are tested.
analyseModule :: Options -> String -> IO [TestModule]
analyseModule opts modname = do
  putStrIfNormal opts $ "Analyzing module '" ++ modname ++ "'...\n"
  catch (readCurryWithParseOptions modname (setQuiet True defaultParams) >>=
         analyseCurryProg opts modname)
        (\_ -> return [staticErrorTestMod modname
                         ["Module '"++modname++"': incorrect source program"]])

analyseCurryProg :: Options -> String -> CurryProg -> IO [TestModule]
analyseCurryProg opts modname prog = do
  progtxt <- readFile (modNameToPath modname ++ ".curry")
  putStrIfVerbose opts "Checking source code for illegal uses of primitives...\n"
  useerrs <- if optSource opts then checkBlacklistUse prog else return []
  seterrs <- if optSource opts then readFlatCurry modname >>= checkSetUse
                               else return []
  putStrIfVerbose opts "Generating property tests...\n"
  let words                   = map firstWord (lines progtxt)
      (rawTests,ignoredTests,pubmod) =
        transformTests opts . renameCurryModule (modname++"_PUBLIC")
                            . makeAllPublic $ prog
      (rawDetTests,ignoredDetTests,pubdetmod) =
        transformDetTests opts . renameCurryModule (modname++"_PUBLICDET")
                               . makeAllPublic $ prog
  unless (null rawTests && null rawDetTests) $ putStrIfNormal opts $
    "Properties to be tested:\n" ++
    unwords (map (snd . funcName) (rawTests++rawDetTests)) ++ "\n"
  unless (null ignoredTests && null ignoredDetTests) $ putStrIfNormal opts $
    "Properties ignored for testing:\n" ++
    unwords (map (snd . funcName) (ignoredTests++ignoredDetTests)) ++ "\n"
  let tm    = TestModule modname
                         (progName pubmod)
                         (map (showOpError words) (seterrs ++ useerrs))
                         (addLinesNumbers words (classifyTests rawTests))
                         (generatorsOfProg pubmod)
      dettm = TestModule modname
                         (progName pubdetmod)
                         []
                         (addLinesNumbers words (classifyTests rawDetTests))
                         (generatorsOfProg pubmod)
  when (testThisModule tm) $ writeCurryProgram pubmod
  when (testThisModule dettm) $ writeCurryProgram pubdetmod
  return (if testThisModule dettm then [tm,dettm] else [tm])
 where
  showOpError words (qf,err) =
    snd qf ++ " (module " ++ modname ++ ", line " ++
    show (getLineNumber words qf) ++"): " ++ err

  addLinesNumbers words = map (addLineNumber words)

  addLineNumber :: [String] -> Test -> Test
  addLineNumber words (PropTest   name texp _) =
    PropTest   name texp $ getLineNumber words (orgTestName name)
  addLineNumber words (AssertTest name _) =
    AssertTest name $ getLineNumber words (orgTestName name)

  getLineNumber :: [String] -> QName -> Int
  getLineNumber words (_, name) = maybe 0 (+1) (elemIndex name words)

-- Extracts all user-defined defined generators defined in a module.
generatorsOfProg :: CurryProg -> [QName]
generatorsOfProg = map funcName . filter isGen . functions
 where
   isGen fdecl = "gen" `isPrefixOf` snd (funcName fdecl) &&
                 isSearchTreeType (resultType (funcType fdecl))

   isSearchTreeType (CTVar _) = False
   isSearchTreeType (CFuncType _ _) = False
   isSearchTreeType (CTCons tc _) = tc == searchTreeTC

-------------------------------------------------------------------------
-- Create the main test module containing all tests of all test modules as
-- a Curry program with name `mainmodname`.
-- The main test module contains a wrapper operation for each test
-- and a main function to execute these tests.
-- Furthermore, if PAKCS is used, test data generators
-- for user-defined types are automatically generated.
genMainTestModule :: Options -> String -> [TestModule] -> IO ()
genMainTestModule opts mainmodname modules = do
  let testtypes = nub (concatMap userTestDataOfModule modules)
  generators <- mapIO (createTestDataGenerator mainmodname) testtypes
  let funcs        = concatMap (createTests opts mainmodname) modules ++
                               generators
      mainFunction = genMainFunction opts mainmodname
                                     (concatMap propTests modules)
      imports      = nub $ [easyCheckModule, searchTreeModule, generatorModule,
                            "System"] ++
                           map fst testtypes ++ map testModuleName modules
  writeCurryProgram (CurryProg mainmodname imports [] (mainFunction : funcs) [])


-- Generates the main function which executes all property tests
-- of all test modules.
genMainFunction :: Options -> String -> [Test] -> CFuncDecl
genMainFunction opts testModule tests =
  CFunc (testModule, "main") 0 Public (ioType unitType) [simpleRule [] body]
 where
  body = CDoExpr $
     (if isQuiet opts
        then []
        else [CSExpr (applyF (pre "putStrLn")
                             [string2ac "Executing all tests..."])]) ++
     [ CSPat (cpvar "x1") $ -- run all tests:
          applyF (easyCheckModule, "runPropertyTests") [easyCheckExprs]
     , CSExpr $ applyF ("System", "exitWith") [cvar "x1"]
     ]

  easyCheckExprs = list2ac $ map makeExpr tests

  makeExpr :: Test -> CExpr
  makeExpr (PropTest (mn, name) _ _) =
    constF (testModule, name ++ "_" ++ modNameToId mn)
  makeExpr (AssertTest (mn, name) _) =
    constF (testModule, name ++ "_" ++modNameToId  mn)


-------------------------------------------------------------------------
-- Creates a test data generator for a given type.
createTestDataGenerator :: String -> QName -> IO CFuncDecl
createTestDataGenerator mainmodname qt@(mn,_) = do
  fprog <- readFlatCurry mn
  maybe (error $ "Definition of type '" ++ qtString ++ "' not found!")
        (return . type2genData)
        (find (\t -> FCG.typeName t == qt) (FCG.progTypes fprog))
 where
  qtString = FC.showQNameInModule "" qt

  type2genData (FC.TypeSyn _ _ _ _) =
    error $ "Cannot create generator for type synonym " ++ qtString
  type2genData (FC.Type _ _ tvars cdecls) =
    if null cdecls
    then error $ "Cannot create value generator for type '" ++ qtString ++
                 "' without constructors!"
    else CFunc (typename2genopname mainmodname [] qt) (length tvars) Public
               (foldr (~>) (CTCons searchTreeTC [CTCons qt ctvars])
                           (map (\v -> CTCons searchTreeTC [v]) ctvars))
               [simpleRule (map CPVar cvars)
                  (foldr1 (\e1 e2 -> applyF (generatorModule,"|||") [e1,e2])
                          (map cons2gen cdecls))]
   where
    cons2gen (FC.Cons qn ar _ ctypes) =
      if ar>maxArity
      then error $ "Test data constructors with more than " ++ show maxArity ++
                   " arguments are currently not supported!"
      else applyF (generatorModule, "genCons" ++ show ar)
                  ([CSymbol qn] ++ map type2gen ctypes)

    type2gen (FC.TVar i) = CVar (i,"a"++show i)
    type2gen (FC.FuncType _ _) =
      error $ "Type '" ++ qtString ++
              "': cannot create value generators for functions!"
    type2gen (FC.TCons qtc argtypes) =
      applyF (typename2genopname mainmodname [] qtc) (map type2gen argtypes)

    ctvars = map (\i -> CTVar (i,"a"++show i)) tvars
    cvars  = map (\i -> (i,"a"++show i)) tvars

-- remove the generated files (except in Verbose-mode)
cleanup :: Options -> String -> [TestModule] -> IO ()
cleanup opts mainmodname modules =
  unless (optVerb opts > 2) $ do
    removeCurryModule mainmodname
    mapIO_ removeCurryModule (map testModuleName modules)
 where
  removeCurryModule modname = do
    system $ installDir ++ "/bin/cleancurry " ++ modname
    system $ "rm -f " ++ modname ++ ".curry"

main :: IO ()
main = do
  argv <- getArgs
  pid  <- getPID
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts = foldl (flip id) defaultOptions funopts
      mainmodname = "TEST" ++ show pid
  unless (null opterrors)
         (putStr (unlines opterrors) >> putStrLn usageText >> exitWith 1)
  putStrIfNormal opts ccBanner 
  when (null args || optHelp opts) (putStrLn usageText >> exitWith 1)
  testModules <- mapIO (analyseModule opts) (map stripCurrySuffix args)
  let staticerrs       = concatMap staticErrors (concat testModules)
      finaltestmodules = filter testThisModule (concat testModules)
  if not (null staticerrs)
   then do showStaticErrors staticerrs
           putStrLn "Testing aborted!"
           cleanup opts mainmodname finaltestmodules
           exitWith 1
   else if null finaltestmodules then exitWith 0 else do
    putStrIfNormal opts $ "Generating main test module '"++mainmodname++"'..."
    genMainTestModule opts mainmodname finaltestmodules
    putStrIfNormal opts $ "and compiling it...\n"
    ret <- system $ unwords $
                     [installDir++"/bin/curry",":set v0",":set parser -Wnone",
                      ":l "++mainmodname,":eval main :q"]
    cleanup opts mainmodname finaltestmodules
    exitWith ret
 where
  showStaticErrors errs = putStrLn $
    unlines (line : "STATIC ERRORS IN PROGRAMS:" : errs) ++ line
  line = take 78 (repeat '=')

-------------------------------------------------------------------------
-- Auxiliaries

-- Extracts the first word of a string
firstWord :: String -> String
firstWord = head . splitOn "\t" . head . splitOn " "

-- Strips a suffix from a string.
stripSuffix :: String -> String -> String
stripSuffix str suf = if suf `isSuffixOf` str
                      then take (length str - length suf) str
                      else str

-- Translate a module name to an identifier, i.e., replace '.' by '_':
modNameToId :: String -> String
modNameToId = intercalate "_" . split (=='.')

--- Name of the EasyCheck module.
easyCheckModule :: String
easyCheckModule = "Test.EasyCheck" 

--- Name of the SearchTree module.
searchTreeModule :: String
searchTreeModule = "SearchTree"

--- Name of SearchTree type constructor.
searchTreeTC :: QName
searchTreeTC = (searchTreeModule,"SearchTree")
    
--- Name of the SearchTreeGenerator module.
generatorModule :: String
generatorModule = "SearchTreeGenerators"

-- Writes a Curry module to its file.
writeCurryProgram :: CurryProg -> IO ()
writeCurryProgram p =
  writeFile (modNameToPath (progName p) ++ ".curry") (showCProg p ++ "\n")

isPAKCS :: Bool
isPAKCS = curryCompiler == "pakcs"

-------------------------------------------------------------------------
