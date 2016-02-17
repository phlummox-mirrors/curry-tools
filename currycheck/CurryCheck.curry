-------------------------------------------------------------------------
--- This is the implementation of the currycheck tool.
--- This tool extracts all EasyCheck tests contained in a Curry module
--- and transforms them such that they are automatically tested
--- and the test results are reported.
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
import Read                    (readNat)
import System                  (system, exitWith, getArgs, getPID)

-- Banner of this tool:
ccBanner :: String
ccBanner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText =
     "CurryCheck: a tool for testing Curry programs (version of 17/02/2016)"
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

data TestModule = TestModule
  { moduleName :: String
  , tests      :: [Test]
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
createTests opts mainmodname (TestModule testmod tests) = map createTest tests
 where
  createTest test =
    cfunc (mainmodname, (genTestName $ getTestName test)) 0 Public
          (ioType (maybeType stringType))
          (case test of PropTest   name t _ -> propBody name (argTypes t) test
                        AssertTest name   _ -> assertBody name test)

  msgOf test = string2ac $ genTestMsg testmod test

  pubmname = publicModuleName testmod
  
  genTestName (modName, fName) = fName ++ "_" ++ modNameToId modName

  easyCheckFuncName arity =
    (easyCheckModule,
     (if isPAKCS then "checkWithValues" else "check") ++ show arity)

  propBody (_, name) argtypes test =
    [simpleRule [] $
       CLetDecl [CLocalPat (CPVar msgvar) (CSimpleRhs (msgOf test) [])]
                (applyF (easyCheckModule,"checkPropWithMsg")
                  [CVar msgvar
                  ,applyF (easyCheckFuncName (length argtypes)) $
                     [configOpWithMaxFail, CVar msgvar] ++
                     (if isPAKCS
                      then map (\t -> applyF
                                        (easyCheckModule,"valuesOfSearchTree")
                                        [type2genop mainmodname t])
                                   argtypes
                      else []) ++
                     [CSymbol (pubmname,name)]
                  ])]
   where
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

type2genop :: String -> CTypeExpr -> CExpr
type2genop _ (CTVar _)       = error "No polymorphic generator!"
type2genop _ (CFuncType _ _) = error "No generator for functional types!"
type2genop testmod (CTCons qt targs) =
  applyF (typename2genopname testmod qt) (map (type2genop testmod) targs)

typename2genopname :: String -> QName -> QName
typename2genopname testmod qtc@(mn,tc)
  | qtc `elem` map pre ["Bool","Int","Char","Maybe","Either","Ordering"]
  = (generatorModule, "gen" ++ tc)
  | qtc `elem` map pre ["[]","()","(,)","(,,)","(,,,)","(,,,,)"]
  = (generatorModule, "gen" ++ transTC tc)
  | otherwise -- we use our own generator:
  = (testmod, "gen_" ++ modNameToId mn ++ "_" ++ tc)
 where
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
  ignoreComment (CmtFunc _ name arity visibility typeExpr rules) = CFunc name arity visibility typeExpr rules
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
  makeExpr (PropTest (mn, name) _ _) = constF (testModule, name ++ "_" ++ modNameToId mn)
  makeExpr (AssertTest (mn, name) _) = constF (testModule, name ++ "_" ++modNameToId  mn)


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

-- Extracts all tests and transforms all polymoprhic tests into Boolean tests.
transformTests :: CurryProg -> ([(Bool,CFuncDecl)], CurryProg)
transformTests (CurryProg modName imports typeDecls functions opDecls) =
  (tests, CurryProg modName imports typeDecls newFunctions opDecls)
 where
  (rawTests, funcs) = partition isTest functions

  tests = concatMap poly2bool rawTests

  newFunctions = funcs ++ map snd tests


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
orgTestName (mn,tname) =
  (mn, if "_ON_BOOL" `isSuffixOf` tname
       then take (length tname - 8) tname
       else tname)

-- this function and genTestEnvironment implement the first phase of CurryCheck
-- analysing the module, i.e. finding tests,
-- and transforming a copy of the module for CurryChecks usage
genAndAnalyseModule :: [CmdFlag] -> String -> IO TestModule
genAndAnalyseModule opts moduleName = do
  putStrIfNormal opts $ "Analyzing tests in module '" ++ moduleName ++ "'...\n"
  prog    <- readCurryWithParseOptions moduleName (setQuiet True defaultParams)
  progtxt <- readFile (modNameToPath moduleName ++ ".curry")
  let words = map firstWord (lines progtxt)
      (alltests, newMod) = transformModule prog
      (rawTests,ignoredTests) = partition fst alltests
  putStrIfNormal opts $
    if null rawTests
    then "No properties found for testing!\n"
    else "Properties to be tested:\n" ++
         unwords (map (snd . funcName . snd) rawTests) ++ "\n"
  unless (null ignoredTests) $ putStrIfNormal opts $
    "Properties ignored for testing:\n" ++
    unwords (map (snd . funcName . snd) ignoredTests) ++ "\n"
  writeCurryProgram newMod
  return $ TestModule moduleName
                      (addLinesNumbers words (classifyTests (map snd rawTests)))
 where
  transformModule :: CurryProg -> ([(Bool,CFuncDecl)], CurryProg)
  transformModule =
    transformTests . renameCurryModule (publicModuleName moduleName)
                   . makeAllPublic

  addLinesNumbers words = map (addLineNumber words)

  addLineNumber :: [String] -> Test -> Test
  addLineNumber words (PropTest   name texp _) =
    PropTest   name texp $ getLineNumber words (orgTestName name)
  addLineNumber words (AssertTest name _) =
    AssertTest name $ getLineNumber words (orgTestName name)

  getLineNumber :: [String] -> QName -> Int
  getLineNumber words (_, name) = lineNumber + 1
   where Just lineNumber = elemIndex name words

genTestEnvironment :: [CmdFlag] -> [String] -> IO [TestModule]
genTestEnvironment opts = mapIO (genAndAnalyseModule opts)

-- Create the main test module containing all tests of all test modules as
-- a Curry program with name `mainmodname`.
-- The main test module contains a wrapper operation for each test
-- and a main function to execute these tests.
-- Furthermore, if PAKCS is used, test data generators
-- for user-defined types are automatically generated.
genTestModule :: [CmdFlag] -> String -> [TestModule] -> IO ()
genTestModule opts mainmodname modules = do
  let testtypes = if isPAKCS
                  then nub (concatMap userTestDataOfModule modules)
                  else []
  generators <- mapIO (createTestDataGenerator mainmodname) testtypes
  let funcs        = concatMap (createTests opts mainmodname) modules ++
                               generators
      mainFunction = genMainFunction opts mainmodname $ concatMap tests modules
      imports      = nub $ [easyCheckModule, "System"] ++
                           (if isPAKCS
                            then [searchTreeModule, generatorModule] ++
                                  map fst testtypes
                            else []) ++
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
    else CFunc (typename2genopname mainmodname qt) (length tvars) Public
               (foldr (~>) (CTCons searchTreeTC [CTCons qt ctvars])
                           (map (\v -> CTCons searchTreeTC [v]) ctvars))
               [simpleRule (map CPVar cvars)
                  (foldr1 (\e1 e2 -> applyF (generatorModule,"|||") [e1,e2])
                          (map cons2gen cdecls))]
   where
    searchTreeTC = (searchTreeModule,"SearchTree")
    
    cons2gen (FC.Cons qn ar _ ctypes) =
      applyF (generatorModule, "genCons" ++ show ar)
             ([CSymbol qn] ++ map type2gen ctypes)

    type2gen (FC.TVar i) = CVar (i,"a"++show i)
    type2gen (FC.FuncType _ _) =
      error $ "Type '" ++ qtString ++
              "': cannot create value generators for functions!"
    type2gen (FC.TCons qtc argtypes) =
      applyF (typename2genopname mainmodname qtc) (map type2gen argtypes)

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
  testModules <- genTestEnvironment opts (map stripCurrySuffix args)
  putStrIfNormal opts $ "Generating main test module '"++mainmodname++"'..."
  genTestModule opts mainmodname testModules
  putStrIfNormal opts $ "and compiling it...\n"
  ret <- execTests mainmodname
  cleanup opts mainmodname testModules
  exitWith ret

-------------------------------------------------------------------------
-- Auxiliaries

-- Translate a module name to an identifier, i.e., replace '.' by '_':
modNameToId :: String -> String
modNameToId = intercalate "_" . split (=='.')

--- Name of the EasyCheck module.
easyCheckModule :: String
easyCheckModule = "Test.EasyCheck" 

--- Name of the SearchTree module.
searchTreeModule :: String
searchTreeModule = "SearchTree"

--- Name of the SearchTreeGenerator module.
generatorModule :: String
generatorModule = "SearchTreeGenerators"

-- Writes a Curry module to its file.
writeCurryProgram :: CurryProg -> IO ()
writeCurryProgram p = writeFile (modNameToPath (progName p) ++ ".curry") (showCProg p)

isPAKCS :: Bool
isPAKCS = curryCompiler == "pakcs"

-------------------------------------------------------------------------
