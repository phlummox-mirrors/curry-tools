-------------------------------------------------------------------------
--- This module contains the datatypes, constructors, and other
--- operations to create and process analyses used in the
--- generic analysis system.
---
--- Each analysis has a name which is used to identify the analysis
--- stored in files, when passing analysis information between workers etc.
---
--- **Important:** Use the constructor operations to define new analyses
---                (instead of the data constructors).
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version May 2013
-------------------------------------------------------------------------

module Analysis(Analysis(..),
                simpleFuncAnalysis,simpleTypeAnalysis,
                simpleConstructorAnalysis,
                dependencyFuncAnalysis,dependencyTypeAnalysis,
                combinedSimpleFuncAnalysis,combinedSimpleTypeAnalysis,
                combinedDependencyFuncAnalysis,combinedDependencyTypeAnalysis,
                isSimpleAnalysis,isCombinedAnalysis,isFunctionAnalysis,
                analysisName,baseAnalysisName,startValue,
                AOutFormat(..))
  where

import FlatCurry(ConsDecl,FuncDecl,TypeDecl,QName)
import FlatCurryGoodies(progImports)
import GenericProgInfo(ProgInfo,combineProgInfo,lookupProgInfo)
import LoadAnalysis(loadCompleteAnalysis,getInterfaceInfos)
import CurryFiles(getImports)

--- Datatype representing a program analysis to be used in the
--- generic analysis system. The datatype is abstract so that
--- one has to use one of the constructor operations to create
--- an analysis.
data Analysis a = 
   SimpleFuncAnalysis String (FuncDecl -> a)
 | SimpleTypeAnalysis String (TypeDecl -> a)
 | SimpleConstructorAnalysis String (ConsDecl -> TypeDecl -> a)
 | DependencyFuncAnalysis String a (FuncDecl -> [(QName,a)] -> a)
 | DependencyTypeAnalysis String a (TypeDecl -> [(QName,a)] -> a)
 | CombinedSimpleFuncAnalysis String String Bool
                              (String -> IO (FuncDecl -> a))
 | CombinedSimpleTypeAnalysis String String Bool
                              (String -> IO (TypeDecl -> a))
 | CombinedDependencyFuncAnalysis String String Bool a
                                  (String -> IO (FuncDecl -> [(QName,a)] -> a))
 | CombinedDependencyTypeAnalysis String String Bool a
                                  (String -> IO (TypeDecl -> [(QName,a)] -> a))


--- A simple analysis for functions takes an operation that computes
--- some information from a given function declaration.
simpleFuncAnalysis :: String -> (FuncDecl -> a) -> Analysis a
simpleFuncAnalysis anaName anaFunc =
  SimpleFuncAnalysis anaName anaFunc 

--- A simple analysis for types takes an operation that computes
--- some information from a given type declaration.
simpleTypeAnalysis :: String -> (TypeDecl -> a) -> Analysis a
simpleTypeAnalysis anaName anaFunc =
  SimpleTypeAnalysis anaName anaFunc 

--- A simple analysis for data constructors takes an operation that computes
--- some information for a constructor declaration and its type declaration
--- to which it belongs.
simpleConstructorAnalysis :: String -> (ConsDecl -> TypeDecl -> a) -> Analysis a
simpleConstructorAnalysis anaName anaFunc =
  SimpleConstructorAnalysis anaName anaFunc 

--- Construct a function analysis with dependencies.
--- The analysis has a name, a start value (representing "no initial
--- information") and an operation to process a function declaration
--- with analysis information
--- for the operations directly called in this function declaration.
--- The analysis will be performed by a fixpoint iteration
--- starting with the given start value.
dependencyFuncAnalysis :: String -> a -> (FuncDecl -> [(QName,a)] -> a)
                       -> Analysis a
dependencyFuncAnalysis anaName startval anaFunc =
  DependencyFuncAnalysis anaName startval anaFunc

--- Construct a type analysis with dependencies.
--- The analysis has a name, a start value (representing "no initial
--- information") and an operation to process a type declaration
--- with analysis information
--- for the type constructors occurring in the type declaration.
--- The analysis will be performed by a fixpoint iteration
--- starting with the given start value.
dependencyTypeAnalysis :: String -> a -> (TypeDecl -> [(QName,a)] -> a)
                       -> Analysis a
dependencyTypeAnalysis anaName startval anaType =
  DependencyTypeAnalysis anaName startval anaType

--- A simple combined analysis for functions.
--- The analysis is based on an operation that computes
--- some information from a given function declaration
--- and information provided by some base analysis.
--- The base analysis is provided as the second argument.
combinedSimpleFuncAnalysis :: String -> Analysis b
                           -> (ProgInfo  b -> FuncDecl -> a) -> Analysis a
combinedSimpleFuncAnalysis ananame baseAnalysis anaFunc =
  CombinedSimpleFuncAnalysis analysisAName ananame True
                             (runWithBaseAnalysis baseAnalysis anaFunc)
 where analysisAName = analysisName baseAnalysis

--- A simple combined analysis for types.
--- The analysis is based on an operation that computes
--- some information from a given type declaration
--- and information provided by some base analysis.
--- The base analysis is provided as the second argument.
combinedSimpleTypeAnalysis :: String -> Analysis b
                           -> (ProgInfo  b -> TypeDecl -> a) -> Analysis a
combinedSimpleTypeAnalysis ananame baseAnalysis anaFunc =
  CombinedSimpleTypeAnalysis analysisAName ananame True
                             (runWithBaseAnalysis baseAnalysis anaFunc)
 where analysisAName = analysisName baseAnalysis

--- A combined analysis for functions with dependencies.
--- The analysis is based on an operation that computes
--- from information provided by some base analysis
--- for each function declaration and information about its
--- directly called operation some information for the declared function.
--- The analysis will be performed by a fixpoint iteration
--- starting with the given start value (fourth argument).
--- The base analysis is provided as the second argument.
combinedDependencyFuncAnalysis :: String -> Analysis b -> a
             -> (ProgInfo b -> FuncDecl -> [(QName,a)] -> a) -> Analysis a
combinedDependencyFuncAnalysis ananame baseAnalysis startval anaFunc =
  CombinedDependencyFuncAnalysis baseAnaName ananame True startval
                                 (runWithBaseAnalysis baseAnalysis anaFunc)
 where
  baseAnaName = analysisName baseAnalysis

--- A combined analysis for types with dependencies.
--- The analysis is based on an operation that computes
--- from information provided by some base analysis
--- for each type declaration and information about its
--- directly used types some information for the declared type.
--- The analysis will be performed by a fixpoint iteration
--- starting with the given start value (fourth argument).
--- The base analysis is provided as the second argument.
combinedDependencyTypeAnalysis :: String -> Analysis b -> a
   -> (ProgInfo b -> TypeDecl -> [(QName,a)] -> a) -> Analysis a
combinedDependencyTypeAnalysis ananame baseAnalysis startval anaType =
  CombinedDependencyTypeAnalysis baseAnaName ananame True startval
                                 (runWithBaseAnalysis baseAnalysis anaType)
 where
  baseAnaName = analysisName baseAnalysis


--- Loads the results of the base analysis and put it as the first
--- argument of the main analysis operation which is returned.
runWithBaseAnalysis :: Analysis a -> (ProgInfo a -> (input -> b)) -> String
                    -> IO (input -> b)
runWithBaseAnalysis baseAnalysis analysisFunction moduleName = do
  importedModules <- getImports moduleName
  let baseananame = analysisName baseAnalysis
  impbaseinfos  <- getInterfaceInfos baseananame importedModules
  mainbaseinfos <- loadCompleteAnalysis baseananame moduleName
  let baseinfos = combineProgInfo impbaseinfos mainbaseinfos
  return (analysisFunction baseinfos)

--- Is the analysis a simple analysis?
--- Otherwise, it is a dependency analysis which requires a fixpoint
--- computation to compute the results.
isSimpleAnalysis :: Analysis a -> Bool
isSimpleAnalysis analysis = case analysis of
  SimpleFuncAnalysis _ _ -> True
  SimpleTypeAnalysis _ _ -> True
  SimpleConstructorAnalysis _ _ -> True
  CombinedSimpleFuncAnalysis _ _ _ _ -> True
  CombinedSimpleTypeAnalysis _ _ _ _ -> True
  _ -> False

--- Is the analysis a combined analysis?
isCombinedAnalysis :: Analysis a -> Bool
isCombinedAnalysis analysis = case analysis of
  CombinedSimpleFuncAnalysis     _ _ _ _   -> True
  CombinedSimpleTypeAnalysis     _ _ _ _   -> True
  CombinedDependencyFuncAnalysis _ _ _ _ _ -> True
  CombinedDependencyTypeAnalysis _ _ _ _ _ -> True
  _ -> False

--- Is the analysis a function analysis?
--- Otherwise, it is a type or constructor analysis.
isFunctionAnalysis :: Analysis a -> Bool
isFunctionAnalysis analysis = case analysis of
  SimpleFuncAnalysis _ _ -> True
  DependencyFuncAnalysis _ _ _ -> True
  CombinedSimpleFuncAnalysis _ _ _ _ -> True
  CombinedDependencyFuncAnalysis _ _ _ _ _ -> True
  _ -> False

--- Name of the analysis to be used in server communication and
--- analysis files.
analysisName :: Analysis a -> String
analysisName (SimpleFuncAnalysis name _) = name
analysisName (SimpleTypeAnalysis name _) = name
analysisName (SimpleConstructorAnalysis name _) = name
analysisName (DependencyFuncAnalysis name _ _) = name
analysisName (DependencyTypeAnalysis name _ _) = name
analysisName (CombinedSimpleFuncAnalysis _ nameB _ _) = nameB
analysisName (CombinedSimpleTypeAnalysis _ nameB _ _) = nameB
analysisName (CombinedDependencyFuncAnalysis _ nameB _ _ _) = nameB
analysisName (CombinedDependencyTypeAnalysis _ nameB _ _ _) = nameB

--- Name of the base analysis of a combined analysis.
baseAnalysisName (CombinedSimpleFuncAnalysis     bName _ _ _) = bName
baseAnalysisName (CombinedSimpleTypeAnalysis     bName _ _ _) = bName
baseAnalysisName (CombinedDependencyFuncAnalysis bName _ _ _ _) = bName
baseAnalysisName (CombinedDependencyTypeAnalysis bName _ _ _ _) = bName

--- Start value of a dependency analysis.
startValue :: Analysis a -> a
startValue (DependencyFuncAnalysis _ startval _) = startval
startValue (DependencyTypeAnalysis _ startval _) = startval 
startValue (CombinedDependencyFuncAnalysis _ _ _ startval _) = startval
startValue (CombinedDependencyTypeAnalysis _ _ _ startval _) = startval

-------------------------------------------------------------------------
--- The desired kind of output of an analysis result.
--- `AText` denotes a standard textual representation.
--- `ANote` denotes a short note that is empty in case of irrelevant
--- information. For instance, this is used in the CurryBrowser
--- to get a quick overview of the analysis results of all operations
--- in a module.
data AOutFormat = AText | ANote

-------------------------------------------------------------------------