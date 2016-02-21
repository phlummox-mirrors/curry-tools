-------------------------------------------------------------------------
--- This is the implementation of the currycheck tool.
--- This tool extracts all EasyCheck tests contained in a Curry module
--- and transforms them such that they are automatically tested
--- and the test results are reported.
--- Moreover, for all functions declared as deterministic,
--- determinism properties are generated and checked.
---
--- @author Jan-Patrick Baye, Michael Hanus
--- @version February 2016
-------------------------------------------------------------------------

import AbstractCurry.Types
import AbstractCurry.Files
import AbstractCurry.Select
import AbstractCurry.Build
import AbstractCurry.Pretty    (showCProg)
import AbstractCurry.Transform (renameCurryModule)
import Distribution
import qualified FlatCurry.Types as FC
import FlatCurry.Files
import qualified FlatCurry.Goodies as FCG
import GetOpt
import IO
import List
import Maybe                   (fromJust, isJust)
import Read                    (readNat)
import System                  (system, exitWith, getArgs, getPID)

--- Maximal arity of check functions and tuples currently supported:
maxArity :: Int
maxArity = 5

-- Banner of this tool:
ccBanner :: String
ccBanner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText =
     "CurryCheck: a tool for testing Curry programs (version of 21/02/2016)"
   bannerLine = take (length bannerText) (repeat '=')

-- print the help
showUsage :: IO ()
showUsage = do
  putStr $ usageInfo ("Usage: currycheck [options] <ModuleName[s]>\n")
                     options
  exitWith 1
  
-------------------------------------------------------------------------
-- command line options
data CmdFlag = FQuiet | FVerbose | FMaxTest Int | FMaxFail Int

options :: [OptDescr CmdFlag]
options =
  [ Option "q" ["quiet"]    (NoArg FQuiet)
           "run quietly (no output, only exit code)"
  , Option "v" ["verbose"]  (NoArg FVerbose)
           "run in verbose mode (ignored if 'quiet' is set)"
  , Option "m" ["maxtests"] (ReqArg (\s -> FMaxTest (readNat s)) "<n>")
           "maximal number of tests (default: 100)"
  , Option "f" ["maxfails"] (ReqArg (\s -> FMaxFail (readNat s)) "<n>")
           "maximal number of condition failures (default: 10000)"
  ]

isQuiet :: [CmdFlag] -> Bool
isQuiet = (FQuiet `elem`)

isVerbose :: [CmdFlag] -> Bool
isVerbose = (FVerbose `elem`)

maxTestOfOpts :: [CmdFlag] -> Maybe Int
maxTestOfOpts opts =
  maybe Nothing
        (\ (FMaxTest n) -> if n==0 then Nothing else Just n)
        (find isMaxTestFlag opts)
 where
   isMaxTestFlag opt = case opt of FMaxTest _ -> True
                                   _          -> False
                                   
maxFailOfOpts :: [CmdFlag] -> Maybe Int
maxFailOfOpts opts =
  maybe Nothing
        (\ (FMaxFail n) -> if n==0 then Nothing else Just n)
        (find isMaxFailFlag opts)
 where
   isMaxFailFlag opt = case opt of FMaxFail _ -> True
                                   _          -> False
                                   
--- Print second argument if verbosity level is not quiet:
putStrIfNormal :: [CmdFlag] -> String -> IO ()
putStrIfNormal opts s = unless (isQuiet opts) $ do
  putStr s
  hFlush stdout

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

-- Representation of the information about a module to be tested:
-- * module name
-- * test operations
-- * name of generators defined in this module (i.e., starting with "gen"
--   and of appropriate result type)
data TestModule = TestModule
  { moduleName :: String
  , tests      :: [Test]
  , generators :: [QName]
  }

-- Extracts the first word of a string
firstWord :: String -> String
firstWord = head . splitOn "\t" . head . splitOn " "

-- The configuration option for EasyCheck
easyCheckConfig :: [CmdFlag] -> QName
easyCheckConfig opts =
  (easyCheckModule, if isQuiet opts   then "quietConfig"   else
                    if isVerbose opts then "verboseConfig" else "easyConfig")

-- Extracts all user data types used as test data generators.
userTestDataOfModule :: TestModule -> [QName]
userTestDataOfModule testmod = concatMap testDataOf (tests testmod)
 where
  testDataOf (AssertTest _ _) = []
  testDataOf (PropTest _ texp _) = unionOn userTypesOf (argTypes texp)

  userTypesOf (CTVar _) = []
  userTypesOf (CFuncType from to) = union (userTypesOf from) (userTypesOf to)
  userTypesOf (CTCons (mn,tc) argtypes) =
    union (if mn == "Prelude" then [] else [(mn,tc)])
          (unionOn userTypesOf argtypes)

  unionOn f = foldr union [] . map f
  
-- Transform all tests to an appropriate call of EasyCheck:
createTests :: [CmdFlag] -> String -> TestModule -> [CFuncDecl]
createTests opts mainmodname tm = map createTest (tests tm)
 where
  testmodname = moduleName tm
  
  createTest test =
    cfunc (mainmodname, (genTestName $ getTestName test)) 0 Public
          (ioType (maybeType stringType))
          (case test of PropTest   name t _ -> propBody name (argTypes t) test
                        AssertTest name   _ -> assertBody name test)

  msgOf test = string2ac $ genTestMsg testmodname test

  pubmname = publicModuleName testmodname
  
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
                     [CSymbol (pubmname,name)]
                  ])]
   where
    useUserDefinedGen texp = case texp of
      CTVar _         -> error "No polymorphic generator!"
      CFuncType _ _   -> error "No generator for functional types!"
      CTCons (_,tc) _ -> isJust
                           (find (\qn -> "gen"++tc == snd qn) (generators tm))

    configOpWithMaxTest = maybe stdConfigOp
                                (\n -> applyF (easyCheckModule,"setMaxTest")
                                              [cInt n, stdConfigOp])
                                (maxTestOfOpts opts)
                            
    configOpWithMaxFail = maybe configOpWithMaxTest
                                (\n -> applyF (easyCheckModule,"setMaxFail")
                                              [cInt n, configOpWithMaxTest])
                                (maxFailOfOpts opts)
                            
    msgvar = (0,"msg")
    
  stdConfigOp = constF (easyCheckConfig opts)
    
  assertBody (_, name) test =
    [simpleRule [] $ applyF (easyCheckModule, "checkPropIOWithMsg")
                            [stdConfigOp, msgOf test, CSymbol (pubmname, name)]]

type2genop :: String -> TestModule -> CTypeExpr -> CExpr
type2genop _ _ (CTVar _)       = error "No polymorphic generator!"
type2genop _ _ (CFuncType _ _) = error "No generator for functional types!"
type2genop mainmod tm (CTCons qt targs) =
  applyF (typename2genopname mainmod (generators tm) qt)
         (map (type2genop mainmod tm) targs)

typename2genopname :: String -> [QName] -> QName -> QName
typename2genopname mainmod definedgenops qtc@(mn,tc)
  | isJust maybeuserdefined -- take user-defined generator:
  = fromJust maybeuserdefined
  | qtc `elem` map pre ["Bool","Int","Char","Maybe","Either","Ordering"]
  = (generatorModule, "gen" ++ tc)
  | qtc `elem` map pre ["[]","()","(,)","(,,)","(,,,)","(,,,,)"]
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

-- generates the main function of the new executable which executes all tests
-- testModule: name of new module
-- tests:      list of tests to execute
genMainFunction :: [CmdFlag] -> String -> [Test] -> CFuncDecl
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


-- generate the name of the modified module
publicModuleName :: String -> String
publicModuleName = (++ "_PUBLIC")

-- extract the original name of a possibly modified module
orgModuleName :: String -> String
orgModuleName s = if "_PUBLIC" `isSuffixOf` s then take (length s - 7) s else s

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

-- Extracts all tests and transforms all polymorphic tests into Boolean tests.
transformTests :: CurryProg -> ([(Bool,CFuncDecl)], CurryProg)
transformTests (CurryProg modName imports typeDecls functions opDecls) =
  (alltests,
   CurryProg modName
             (nub (easyCheckModule:imports))
             typeDecls
             (map (revertDetOpTrans detOpNames) funcs ++ map snd alltests)
             opDecls)
 where
  (rawTests, funcs) = partition isTest functions

  preCondFuns = map ((\ (mn,fn) -> (mn,stripSuffix fn "'pre")) . funcName)
                    (filter (\fd -> "'pre" `isSuffixOf` snd (funcName fd))
                            functions)

  detOpTests = genDetOpTests preCondFuns functions -- determinism tests

  -- names of deterministic operations:
  detOpNames = map (stripIsDet . funcName) detOpTests

  stripIsDet (mn,fn) = (mn, take (length fn -15) fn)

  alltests = concatMap poly2bool (rawTests ++ detOpTests)


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
  CFunc (mn, forg ++ "IsDeterministic") ar Public
   (propResultType texp)
   [simpleRule (map CPVar cvars) $
      CLetDecl [CLocalPat (CPVar rvar)
                          (CSimpleRhs forgcall [])]
               (applyF (easyCheckModule,"==>")
                       [preCond,
                        applyF (easyCheckModule,"always")
                               [applyF (pre "==") [forgcall, CVar rvar]]])]
 where
  forg  = take (length fn - 9) fn
  forgcall = applyF (mn,forg) (map CVar cvars)
  cvars = map (\i -> (i,"x"++show i)) [1 .. ar]
  rvar  = (0,"r")

  eqResult = applyF (pre "==") [CVar rvar, CVar rvar]

  preCond = if (mn,forg) `elem` prefuns
            then applyF (pre "&&")
                        [applyF (mn,forg++"'pre") (map CVar cvars), eqResult]
            else eqResult

  propResultType (CTVar _) = propType
  propResultType (CTCons _ _) = propType
  propResultType (CFuncType from to) = CFuncType from (propResultType to)

-- Generates auxiliary (Boolean-instantiated) test functions for
-- polymorphically typed test function.
-- The flag indicates whether the test function should be actually passed
-- to the test tool.
poly2bool :: CFuncDecl -> [(Bool,CFuncDecl)]
poly2bool (CmtFunc _ name arity vis ftype rules) =
  poly2bool (CFunc name arity vis ftype rules)
poly2bool fdecl@(CFunc (mn,fname) arity vis ftype _)
  | isPolyType ftype
  = [(False,fdecl)
    ,(True, CFunc (mn,fname++"_ON_BOOL") arity vis (p2b ftype)
                     [simpleRule [] (applyF (mn,fname) [])])
    ]
  | otherwise
  = [(True,fdecl)]
 where
  p2b (CTVar _) = boolType
  p2b (CFuncType t1 t2) = CFuncType (p2b t1) (p2b t2)
  p2b (CTCons ct ts) = CTCons ct (map p2b ts)

-- Transforms a possibly changed test name (like "test_ON_BOOL")
-- back to its original name.
orgTestName :: QName -> QName
orgTestName (mn,tname)
  | "_ON_BOOL" `isSuffixOf` tname
  = orgTestName (mn, take (length tname - 8) tname)
  | "IsDeterministic" `isSuffixOf` tname
  = orgTestName (mn, take (length tname - 15) tname)
  | otherwise = (mn,tname)

-- This function implement the first phase of CurryCheck: it analyses
-- a module to be checked, i.e., it finds the tests,
-- creates new tests (e.g., for polymorphic properties, deterministic functions)
-- and generates a copy of the module appropriate for the main operation
-- of CurryCheck (e.g., all operations are made public).
genAndAnalyseModule :: [CmdFlag] -> String -> IO TestModule
genAndAnalyseModule opts modname = do
  putStrIfNormal opts $ "Analyzing tests in module '" ++ modname ++ "'...\n"
  prog    <- readCurryWithParseOptions modname (setQuiet True defaultParams)
  progtxt <- readFile (modNameToPath modname ++ ".curry")
  let words                   = map firstWord (lines progtxt)
      (alltests, pubmod)      = transformModule prog
      (rawTests,ignoredTests) = partition fst alltests
  putStrIfNormal opts $
    if null rawTests
    then "No properties found for testing!\n"
    else "Properties to be tested:\n" ++
         unwords (map (snd . funcName . snd) rawTests) ++ "\n"
  unless (null ignoredTests) $ putStrIfNormal opts $
    "Properties ignored for testing:\n" ++
    unwords (map (snd . funcName . snd) ignoredTests) ++ "\n"
  writeCurryProgram pubmod
  return $ TestModule modname
                      (addLinesNumbers words (classifyTests (map snd rawTests)))
                      (generatorsOfProg pubmod)
 where
  transformModule :: CurryProg -> ([(Bool,CFuncDecl)], CurryProg)
  transformModule =
    transformTests . renameCurryModule (publicModuleName modname)
                   . makeAllPublic

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

-- Create the main test module containing all tests of all test modules as
-- a Curry program with name `mainmodname`.
-- The main test module contains a wrapper operation for each test
-- and a main function to execute these tests.
-- Furthermore, if PAKCS is used, test data generators
-- for user-defined types are automatically generated.
genTestModule :: [CmdFlag] -> String -> [TestModule] -> IO ()
genTestModule opts mainmodname modules = do
  let testtypes = nub (concatMap userTestDataOfModule modules)
  generators <- mapIO (createTestDataGenerator mainmodname) testtypes
  let funcs        = concatMap (createTests opts mainmodname) modules ++
                               generators
      mainFunction = genMainFunction opts mainmodname $ concatMap tests modules
      imports      = nub $ [easyCheckModule, searchTreeModule, generatorModule,
                            "System"] ++
                           map fst testtypes ++
                           map (publicModuleName . moduleName) modules
  writeCurryProgram (CurryProg mainmodname imports [] (mainFunction : funcs) [])

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

-- Executes the main operation of the generated test module.
execTests :: String -> IO Int
execTests mainmodname = system $
  installDir ++ "/bin/curry :set v0 :l " ++ mainmodname ++ " :eval main :q"

-- remove the generated files (except in Verbose-mode)
cleanup :: [CmdFlag] -> String -> [TestModule] -> IO ()
cleanup opts mainmodname modules =
  unless (isVerbose opts) $ do
    removeCurryModule mainmodname
    mapIO_ removeCurryModule (map (publicModuleName . moduleName) modules)
 where
  removeCurryModule modname = do
    system $ installDir ++ "/bin/cleancurry " ++ modname
    system $ "rm -f " ++ modname ++ ".curry"

main :: IO ()
main = do
  argv <- getArgs
  pid  <- getPID
  let (opts, args, opterrors) = getOpt RequireOrder options argv
      mainmodname = "TEST" ++ show pid
  unless (null opterrors) (putStr (unlines opterrors) >> showUsage)
  putStrIfNormal opts ccBanner 
  when (null args) showUsage
  testModules <- mapIO (genAndAnalyseModule opts) (map stripCurrySuffix args)
  putStrIfNormal opts $ "Generating main test module '"++mainmodname++"'..."
  genTestModule opts mainmodname testModules
  putStrIfNormal opts $ "and compiling it...\n"
  ret <- execTests mainmodname
  cleanup opts mainmodname testModules
  exitWith ret

-------------------------------------------------------------------------
-- Auxiliaries

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
