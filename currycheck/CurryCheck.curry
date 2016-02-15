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
import GetOpt
import IO
import IOExts
import List
import Read                    (readNat)
import System                  (system, exitWith, getArgs, getPID)

-------------------------------------------------------------------------
-- command line options
data CmdFlag = FQuiet | FVerbose | FMaxTest Int

options :: [OptDescr CmdFlag]
options =
  [ Option "q" ["quiet"]    (NoArg FQuiet)
           "run quietly (no output, only exit code)"
  , Option "v" ["verbose"]  (NoArg FVerbose)
           "run in verbose mode (ignored if 'quiet' is set)"
  , Option "m" ["maxtests"] (ReqArg (\s -> FMaxTest (readNat s)) "<n>")
           "maximal number of tests (default: 100)"
  ]

maxTestOfOpts :: [CmdFlag] -> Maybe Int
maxTestOfOpts opts =
  maybe Nothing
        (\ (FMaxTest n) -> if n==0 then Nothing else Just n)
        (find isMaxTestFlag opts)
 where
   isMaxTestFlag opt = case opt of FMaxTest _ -> True
                                   _          -> False
                                   
-------------------------------------------------------------------------
-- represents the verbosity level used
data VerbosityMode = Quiet | Verbose | Normal

--- Print second argument if verbosity level is at least Normal:
putStrIfNormal :: VerbosityMode -> String -> IO ()
putStrIfNormal md s = when (md /= Quiet) $ do
  putStr s
  hFlush stdout

-------------------------------------------------------------------------

-- Internal representation of tests extracted from a Curry module.
-- A test is either a property test (with a name, type, source line number)
-- passed to EasyCheck, or an IO test (with a name, source line number)
-- which is directly executed.
data Test = PropTest QName CTypeExpr Int | AssertTest QName Int

-- The name of a test:
getTestName :: Test -> QName
getTestName (PropTest   n _ _) = n
getTestName (AssertTest n   _) = n

-- The line number of a test:
getTestLine :: Test -> Int
getTestLine (PropTest   _ _ n) = n
getTestLine (AssertTest _   n) = n

data TestModule = TestModule
  { moduleName :: String
  , newName    :: String
  , tests      :: [Test]
  }

-- get all lines from file `filename`
getLines :: String -> IO [String]
getLines filename = readFile filename >>= return . lines

-- extracts the first word of each string
firstWordsOfLines :: [String] -> [String]
firstWordsOfLines = map firstWord
 where
  firstWord = head . splitOn "\t" . head . splitOn " "

-- generate a useful error message for failed tests (module and line number)
genMsg :: Int -> String -> QName -> String
genMsg lineNumber file testfun =
  snd (testfun) ++
  " (module " ++ file ++ ", line " ++ show lineNumber ++ ")"

easyCheckConfig :: VerbosityMode -> QName
easyCheckConfig m =
  (easyCheckModule, case m of Verbose -> "verboseConfig"
                              Quiet   -> "quietConfig"
                              _       -> "easyConfig"   )

-- Transform all tests to an appropriate call of EasyCheck:
createTests :: [CmdFlag] -> VerbosityMode -> String -> TestModule -> [CFuncDecl]
createTests opts m testmodname (TestModule moduleName newName tests) =
  map (createTest opts m testmodname moduleName newName) tests

createTest :: [CmdFlag] -> VerbosityMode -> String -> String -> String -> Test
           -> CFuncDecl
createTest opts m testmodname origName modname test =
  uncurry (cfunc (testmodname, (genTestName $ getTestName test)) 0 Public)
          createTest'
 where
  createTest' = case test of
    (PropTest   name t _) -> (checkPropResultType, propBody name (argTypes t))
    (AssertTest name   _) -> (checkPropResultType, assertBody name)

  checkPropResultType = ioType (maybeType stringType)
  
  genTestName (modName, fName) = fName ++ "_" ++ modName

  msg = string2ac $ genMsg (getTestLine test) origName (getTestName test)

  easyCheckFuncName arity =
    (easyCheckModule, ifPAKCS "checkWithValues" "check" ++ show arity)

  propBody :: QName -> [CTypeExpr] -> [CRule]
  propBody (_, name) argtypes =
    [simpleRule [] $
       CLetDecl [CLocalPat (CPVar msgvar) (CSimpleRhs msg [])]
                (applyF (easyCheckModule,"checkPropWithMsg")
                  [CVar msgvar
                  ,applyF (easyCheckFuncName (length argtypes)) $
                     [maybe (constF (easyCheckConfig m))
                            (\mx -> applyF (easyCheckModule,"setMaxTest")
                                           [cInt mx,
                                            constF (easyCheckConfig m)])
                            (maxTestOfOpts opts)
                     ,CVar msgvar] ++
                     (ifPAKCS (map (\t -> applyF
                                          (easyCheckModule,"valuesOfSearchTree")
                                          [type2genop testmodname t])
                                   argtypes)
                              []) ++
                     [CSymbol (modname,name)]
                  ])]
   where msgvar = (0,"msg")
    
  assertBody :: QName -> [CRule]
  assertBody (_, name) =
    [simpleRule [] $ applyF (easyCheckModule, "checkPropIOWithMsg")
                            [constF (easyCheckConfig m)
                            ,msg
                            ,CSymbol (modname, name)]]

type2genop :: String -> CTypeExpr -> CExpr
type2genop _ (CTVar _)       = error "No polymorphic generator!"
type2genop _ (CFuncType _ _) = error "No generator for functional types!"
type2genop testmod (CTCons qtc@(_,tc) targs)
  | qtc `elem` map pre ["Bool","Int","Char","Maybe","Either","Ordering"]
  = applyF (generatorModule, "gen" ++ tc) arggens
  | qtc `elem` map pre ["[]","()","(,)","(,,)","(,,,)","(,,,,)"]
  = applyF (generatorModule, "gen" ++ transTC tc) arggens
  | otherwise
   -- let's hope that the programmer has defined an appropriate generator:
  = applyF (testmod, "gen" ++ tc) arggens
 where
  arggens = map (type2genop testmod) targs

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
genMainFunction :: VerbosityMode -> String -> [Test] -> CFuncDecl
genMainFunction vm testModule tests =
  CFunc (testModule, "main") 0 Public typeExpr body
 where
  typeExpr = ioType unitType -- IO ()

  body = [simpleRule [] expr]

  expr = CDoExpr $
     (if vm == Quiet
        then []
        else [CSExpr (applyF (pre "putStrLn")
                             [string2ac "Executing all tests..."])]) ++
     [ CSPat (cpvar "x1") $ -- run all tests:
          applyF (easyCheckModule, "runPropertyTests") [easyCheckExprs]
     , CSExpr $ applyF ("System", "exitWith") [cvar "x1"]
     ]

  easyCheckExprs = list2ac $ map makeExpr tests

  makeExpr :: Test -> CExpr
  makeExpr (PropTest (mn, name) _ _) = constF (testModule, name ++ "_" ++ mn)
  makeExpr (AssertTest (mn, name) _) = constF (testModule, name ++ "_" ++ mn)


-- generate the name of the modified module
publicModuleName :: String -> String
publicModuleName = (++ "_PUBLIC")

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
genAndAnalyseModule :: VerbosityMode -> String -> IO TestModule
genAndAnalyseModule m moduleName = do
  putStrIfNormal m $ "Analyzing tests in module '" ++ moduleName ++ "'...\n"
  prog <- readCurryWithParseOptions moduleName (setQuiet True defaultParams)
  lines <- getLines $ moduleName ++ ".curry"
  let words = firstWordsOfLines lines
      (alltests, newMod) = transformModule prog
      (rawTests,ignoredTests) = partition fst alltests
  putStrIfNormal m $
    if null rawTests
    then "No properties found for testing!\n"
    else "Properties to be tested: " ++
         unwords (map (snd . funcName . snd) rawTests) ++ "\n"
  unless (null ignoredTests) $ putStrIfNormal m $
    "Properties ignored for testing: " ++
    unwords (map (snd . funcName . snd) ignoredTests) ++ "\n"
  saveCurryProgram newMod
  return $ TestModule moduleName newModName
                      (addLinesNumbers words (classifyTests (map snd rawTests)))
 where
  transformModule :: CurryProg -> ([(Bool,CFuncDecl)], CurryProg)
  transformModule =
    transformTests . renameCurryModule newModName . makeAllPublic

  newModName = publicModuleName moduleName

  addLinesNumbers words = map (addLineNumber words)

  addLineNumber :: [String] -> Test -> Test
  addLineNumber words (PropTest   name texp _) =
    PropTest   name texp $ getLineNumber words (orgTestName name)
  addLineNumber words (AssertTest name _) =
    AssertTest name $ getLineNumber words (orgTestName name)

  getLineNumber :: [String] -> QName -> Int
  getLineNumber words (_, name) = lineNumber + 1
   where Just lineNumber = elemIndex name words

genTestEnvironment :: VerbosityMode -> [String] -> IO [TestModule]
genTestEnvironment m = mapIO (genAndAnalyseModule m)

-- this creates the auxiliary test module containing all modules' tests
-- and the main function to run the tests
genTestModule :: [CmdFlag] -> VerbosityMode -> String -> [TestModule] -> IO ()
genTestModule opts m testmodname modules = saveCurryProgram testProg
 where
  funcs = concatMap (createTests opts m testmodname) modules
  mainFunction = genMainFunction m testmodname $ concatMap tests modules
  testProg = CurryProg testmodname imports [] (mainFunction : funcs) []
  imports = [easyCheckModule, "System"] ++
            (ifPAKCS [generatorModule] []) ++
            map newName modules

-- Executes the main operation of the generated test module.
execTests :: String -> IO Int
execTests testmodname = system $
  curryBin ++ " :set v0 :l " ++ testmodname ++ " :eval main :q"

-- print the help
showUsage :: IO ()
showUsage = do
  putStr $ usageInfo ("Usage: currycheck [OPTIONS] ModuleName[s]") options
  exitWith 1
  
--- Name of the Curry REPL binary:
curryBin :: String
curryBin = installDir ++ "/bin/curry"

-- remove the generated files (except in Verbose-mode)
cleanup :: VerbosityMode -> String -> [TestModule] -> IO ()
cleanup mode testmodname modules =
  case mode of
    Verbose -> return ()
    _       -> do removeCurryModule testmodname
                  mapIO_ removeCurryModule (map newName modules)
 where
  removeCurryModule modname = do
    system $ installDir ++ "/bin/cleancurry " ++ modname
    system $ "rm -f " ++ modname ++ ".curry"

main :: IO ()
main = do
  argv <- getArgs
  pid  <- getPID
  let (opts, args, opterrors) = getOpt RequireOrder options argv
      vmode = getVerbosityMode opts
      testmodname = "TEST" ++ show pid
  unless (null opterrors) (putStr (unlines opterrors) >> showUsage)
  when (null args) showUsage
  testModules <- genTestEnvironment vmode (map stripCurrySuffix args)
  putStrIfNormal vmode $
    "Generating auxiliary test module '"++testmodname++"'...\n"
  genTestModule opts vmode testmodname testModules
  putStrIfNormal vmode $
    "Compiling auxiliary test module '"++testmodname++"'...\n"
  ret <- execTests testmodname
  cleanup vmode testmodname testModules
  exitWith ret
 where
  getVerbosityMode :: [CmdFlag] -> VerbosityMode
  getVerbosityMode fs | FQuiet   `elem` fs = Quiet
                      | FVerbose `elem` fs = Verbose
                      | otherwise          = Normal

-------------------------------------------------------------------------
-- Auxiliaries

--- Name of the EasyCheck module.
easyCheckModule :: String
easyCheckModule = "Test.EasyCheck" 

--- Name of the SearchTree module.
searchTreeModule :: String
searchTreeModule = "SearchTree"

--- Name of the SearchTreeGenerator module.
generatorModule :: String
generatorModule = "SearchTreeGenerators"

-- save a Curry program as 'ModuleName'.curry
saveCurryProgram :: CurryProg -> IO ()
saveCurryProgram p = do
  file <- openFile (progName p ++ ".curry") WriteMode
  hPutStrLn file $ showCProg p
  hClose file

ifPAKCS :: a -> a -> a
ifPAKCS x y = if curryCompiler == "pakcs" then x else y

-------------------------------------------------------------------------
