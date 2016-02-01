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
import AbstractCurry.Pretty (showCProg)
import Distribution
import GetOpt
import IO
import IOExts
import List
import Maybe                (fromJust)
import System               (system, exitWith, getArgs, getProgName, getPID)

-- command line options
data CmdFlag = FQuiet | FVerbose

options :: [OptDescr CmdFlag]
options =
  [ Option "q" ["quiet"]   (NoArg FQuiet)   "run quietly (no output, only exit code)"
  , Option "v" ["verbose"] (NoArg FVerbose) "run in verbose mode (ignored if 'quiet' is set)"
  ]

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
-- A test is either a property test (with a name, arity, source line number)
-- passed to EasyCheck, or an IO test (with a name, source line number)
-- which is directly executed.
data Test = PropTest QName Int Int | AssertTest QName Int

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
  snd (orgTestName testfun) ++
  " (module " ++ file ++ ", line " ++ show lineNumber ++ ")"

-- createTests and createTest transform the tests
-- for execTests (cf. chapter 5)
createTests :: VerbosityMode -> String -> TestModule -> [CFuncDecl]
createTests m testmodname (TestModule moduleName newName tests)
  = map (createTest m testmodname moduleName newName) tests

createTest :: VerbosityMode -> String -> String -> String -> Test -> CFuncDecl
createTest m testmodname origName modname test
  = uncurry (cfunc (testmodname, (genTestName $ getTestName test)) 0 Public)
            createTest'
 where
  createTest' = case test of
    (PropTest   name arity _) -> (execPropResultType, propBody name arity)
    (AssertTest name       _) -> (execPropResultType, assertBody name)

  execPropResultType = ioType (maybeType stringType)
  
  genTestName (modName, fName) = fName ++ "_" ++ modName

  msg = string2ac $ genMsg (getTestLine test) origName (getTestName test)

  easyCheckFuncName :: String
  easyCheckFuncName = case m of Verbose -> "verboseCheck"
                                _       -> "easyCheck"

  easyCheck arity = (easyCheckModule, easyCheckFuncName ++ show arity)

  propBody :: QName -> Int -> [CRule]
  propBody (_, name) arity =
    [simpleRule [] $
       CLetDecl [CLocalPat (CPVar msgvar) (CSimpleRhs msg [])]
                (applyF (easyCheckModule,"execPropWithMsg")
                  [CVar msgvar
                  ,applyF (easyCheck arity) [CVar msgvar,CSymbol (modname,name)]
                  ])]
   where msgvar = (0,"msg")
    
  assertBody :: QName -> [CRule]
  assertBody (_, name)
    = [simpleRule [] $ applyF (easyCheckModule, "execAssertIO")
                              [CSymbol (modname, name), msg]]

-- the module has to be renamed, this happens in two steps
-- part one: changing the module name in the module header
renameModule1 :: String -> CurryProg -> CurryProg
renameModule1 newName (CurryProg _ imports typedecls functions opdecls)
  = CurryProg newName imports typedecls functions opdecls

-- part two: remove all references to the old module name in the code
-- Problem (TODO): simple string replacement does not for hierarchical
-- module names
renameModule2 :: String -> String -> IO ()
renameModule2 oldName newName =
  updateFile (replaceString (oldName ++ ".") "") filename
 where
  filename = newName ++ ".curry"

-- replace all occurrences of old with new in str
replaceString :: String -> String -> String -> String
replaceString _   _   ""         = ""
replaceString old new str@(s:ss)
  | old == take (length old) str = new ++ replaceString old new (drop (length old) str)
  | otherwise                    = s : replaceString old new ss

-- make all functions public
-- this ensures that all tests can be executed
makeAllPublic :: CurryProg -> CurryProg
makeAllPublic (CurryProg modname imports typedecls functions opdecls)
  = CurryProg modname imports typedecls publicFunctions opdecls
 where
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
          applyF (easyCheckModule, "execProps") [easyCheckExprs]
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
-- Since parameterized Prop tests are supported only by kics2,
-- we add these only if kics2 is used.
isTest :: CFuncDecl -> Bool
isTest = isTestType . funcType
 where
  isTestType :: CTypeExpr -> Bool
  isTestType ct =
    isPropIOType ct || ct == propType ||
    (curryCompiler == "kics2" && resultType ct == propType)

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

  property f = PropTest (funcName f) (calcArity (funcType f)) 0

  assertion f = AssertTest (funcName f) 0

-- Extracts all tests and transforms all polymoprhic tests into Boolean tests.
transformTests :: CurryProg -> ([CFuncDecl], CurryProg)
transformTests (CurryProg modName imports typeDecls functions opDecls) =
  (map snd (filter fst tests),
   CurryProg modName imports typeDecls newFunctions opDecls)
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
poly2bool fdecl@(CFunc (mn,fname) arity vis ftype _) =
  if isPolyType ftype
  then [(False,fdecl)
       ,(True, CFunc (mn,fname++"_ON_BOOL") arity vis (p2b ftype)
                     [simpleRule [] (applyF (mn,fname) [])])
       ]
  else [(True,fdecl)]
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
  let (rawTests, newMod) = transformModule prog
  putStrIfNormal m $
    "Test operations found: " ++
    unwords (map (snd . orgTestName . funcName) rawTests) ++ "\n"
  saveCurryProgram newMod
  renameModule2 moduleName newModName
  let tests = classifyTests rawTests
  return $ TestModule moduleName newModName (addLinesNumbers words tests)
 where
  transformModule :: CurryProg -> ([CFuncDecl], CurryProg)
  transformModule = transformTests . renameModule1 newModName . makeAllPublic

  newModName = publicModuleName moduleName

  addLinesNumbers words = map (addLineNumber words)

  addLineNumber :: [String] -> Test -> Test
  addLineNumber words (PropTest   name a _) =
    PropTest   name a $ getLineNumber words (orgTestName name)
  addLineNumber words (AssertTest name _) =
    AssertTest name $ getLineNumber words (orgTestName name)

  getLineNumber :: [String] -> QName -> Int
  getLineNumber words (_, name) = lineNumber + 1
   where Just lineNumber = elemIndex name words

genTestEnvironment :: VerbosityMode -> [String] -> IO [TestModule]
genTestEnvironment m = mapIO (genAndAnalyseModule m)

-- this creates the auxiliary test module containing all modules' tests
-- and the main function to run the tests
genTestModule :: VerbosityMode -> String -> [TestModule] -> IO ()
genTestModule m testmodname modules = saveCurryProgram testProg
 where
  funcs = concatMap (createTests m testmodname) modules
  mainFunction = genMainFunction m testmodname $ concatMap tests modules
  testProg = CurryProg testmodname imports [] (mainFunction : funcs) []
  imports = [easyCheckModule, "System"] ++ map newName modules

-- executes execTests.main
execTests :: VerbosityMode -> String -> IO Int
execTests m testmodname = system $
  makeCmdQuiet (curryBin ++ " :set v0 :l " ++ testmodname ++ " :eval main :q") m

-- depending on the verbosity mode this
-- adds a redirection of all output to /dev/null
makeCmdQuiet :: String -> VerbosityMode -> String
makeCmdQuiet cmd m = case m of
  Quiet -> cmd ++ " 2>&1 > /dev/null"
  _     -> cmd

-- print the help
showUsage :: IO ()
showUsage = do
  progName <- getProgName
  error $ usageInfo ("usage: " ++ progName ++ " [OPTIONS] ModuleName[s]") options

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
  let (opts, args, _) = getOpt RequireOrder options argv
      mode = getVerbosityMode opts
      testmodname = "TEST" ++ show pid
  when (null args) showUsage
  testModules <- genTestEnvironment mode (map stripCurrySuffix args)
  putStrIfNormal mode $
    "Generating auxiliary test module '"++testmodname++"'...\n"
  genTestModule mode testmodname testModules
  putStrIfNormal mode $
    "Compiling auxiliary test module '"++testmodname++"'...\n"
  ret <- execTests mode testmodname
  cleanup mode testmodname testModules
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

-- Calculates the arity of a function from its actual type.
calcArity :: CTypeExpr -> Int
calcArity = length . argTypes

-- save a Curry program as 'ModuleName'.curry
saveCurryProgram :: CurryProg -> IO ()
saveCurryProgram p = do
  file <- openFile (progName p ++ ".curry") WriteMode
  hPutStrLn file $ showCProg p
  hClose file

-------------------------------------------------------------------------
