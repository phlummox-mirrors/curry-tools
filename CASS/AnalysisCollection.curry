--------------------------------------------------------------------
--- This module collects all analyses in the analysis system.
---
--- Each analysis available in the analysis system must be
--- registered in the top part of this module.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version March 2013
--------------------------------------------------------------------

module AnalysisCollection(
  analysisInfos,functionAnalysisInfos,registeredAnalysisNames,
  lookupRegAnaWorker,runAnalysisWithWorkers,analyzeMain) where

import FlatCurry
import FlatCurryGoodies(progImports)
import IO
import IOExts
import XML

import Analysis
import Configuration(debugMessageLevel,numberOfWorkers)
import CurryFiles(getImports)
import GenericProgInfo
import AnalysisDependencies(getModulesToAnalyze)
import ServerFunctions(workerLoop)
import WorkerFunctions(analysisClient)
import LoadAnalysis(loadCompleteAnalysis)

--------------------------------------------------------------------
-- Configurable part of this module.
--------------------------------------------------------------------

import Deterministic
import HigherOrder
import RightLinearity
import SolutionCompleteness
import TotallyDefined
import Indeterministic

--- Each analysis name should be added here together with a short explanation.
--- The first component is the registered analysis name.
--- These names will be visible by the server message `GetAnalysis`.
--- The second and third components, which might be used in interactive tools
--- like the CurryBrowser, are a longer analysis name and some explanation
--- of the analysis and their result values.
analysisInfos = functionAnalysisInfos ++ typeAnalysisInfos

functionAnalysisInfos =
  [("Overlapping",  "Overlapping rules", "Overlapping function analysis"),
   ("Deterministic","Deterministic operations",
    "(Non-)determinism function analysis"),
   ("SetValued",    "Set-valued operations", "Set-valued function analysis"),
   ("PatComplete",  "Pattern completeness", "Pattern completeness analysis"),
   ("Total",        "Totally defined operations",
    "Totally definedness analysis"),
   ("SolComplete",  "Solution completeness","Solution completeness analysis"),
   ("Indeterministic","Indeterministic operations",
    "Indeterminism function analysis"),
   ("RightLinear",  "Right-linear operations","Right-linear function analysis"),
   ("HiOrderFunc",  "Higher-order functions","Higher-order function analysis")]

typeAnalysisInfos =
  [("HiOrderType",  "Higher-order datatypes", "Higher-order datatype analysis"),
   ("HiOrderConstr","Higher-order constructors",
    "Higher-order constructor analysis"),
   ("SiblingCons",  "Sibling constructors","Sibling constructor analysis")]

--------------------------------------------------------------------
--- Each analysis used in our tool must be registered in this list
--- together with an operation to show the analysis result as a string.
registeredAnalysis :: [RegisteredAnalysis]
registeredAnalysis =
  [scAnalysis overlapAnalysis showOverlap
  ,scAnalysis ndAnalysis      showDet
  ,scAnalysis setValAnalysis  showSetValued
  ,scAnalysis rlinAnalysis    showRightLinear
  ,scAnalysis solcompAnalysis showSolComplete
  ,scAnalysis patCompAnalysis showComplete
  ,scAnalysis totalAnalysis   showTotally
  ,scAnalysis indetAnalysis   showIndet
  ,scAnalysis hiOrdType       showOrder
  ,scAnalysis hiOrdCons       showOrder
  ,scAnalysis hiOrdFunc       show
  ,scAnalysis siblingCons     show
  ]



--------------------------------------------------------------------
-- Static part of this module follows below
--------------------------------------------------------------------

--- The type of all registered analysis.
--- The first component is the name of the analysis.
--- The second component is the operation used by the server
--- to distribute analysis work to the clients.
--- The third component is the worker operation to analyze a list of modules.
data RegisteredAnalysis =
  RegAna String
         (String -> [Handle] -> Bool -> IO (Either (ProgInfo String) String))
         ([String] -> IO ())

regAnaName (RegAna n _ _) = n

regAnaServer (RegAna _ a _) = a

regAnaWorker (RegAna _ _ a) = a

--- Names of all registered analyses.
registeredAnalysisNames = map regAnaName registeredAnalysis

lookupRegAna :: String -> [RegisteredAnalysis] -> Maybe RegisteredAnalysis
lookupRegAna _ [] = Nothing
lookupRegAna aname (ra@(RegAna raname _ _) : ras) =
  if aname==raname then Just ra else lookupRegAna aname ras

-- Look up a registered analysis server with a given analysis name.
lookupRegAnaServer :: String
        -> (String -> [Handle] -> Bool -> IO (Either (ProgInfo String) String))
lookupRegAnaServer aname =
  maybe (\_ _ _ -> return (Right ("unknown analysis: "++aname)))
        regAnaServer
        (lookupRegAna aname registeredAnalysis)

-- Look up a registered analysis worker with a given analysis name.
lookupRegAnaWorker :: String -> ([String] -> IO ())
lookupRegAnaWorker aname =
  maybe (const done) regAnaWorker (lookupRegAna aname registeredAnalysis)

--- This auxiliary operation creates new analysis operations to be used
--- by the server/client analysis tool from a given analysis and
--- analysis show function.
scAnalysis :: Analysis a -> (a->String) -> RegisteredAnalysis
scAnalysis analysis showres =
  RegAna (analysisName analysis)
         (analyzeAsString analysis showres)
         (analysisClient analysis)

--------------------------------------------------------------------

debugMessage dl message =
  debugMessageLevel dl ("AnalysisCollection: "++message)

--------------------------------------------------------------------
-- Run an analysis with a given name on a given module with a list
-- of workers identified by their handles and return the analysis results.
runAnalysisWithWorkers :: String -> [Handle] -> String
                       -> IO (Either (ProgInfo String) String)
runAnalysisWithWorkers ananame handles moduleName =
  (lookupRegAnaServer ananame) moduleName handles True

-- Run an analysis with a given name on a given module with a list
-- of workers identified by their handles but do not load analysis results.
runAnalysisWithWorkersNoLoad :: String -> [Handle] -> String -> IO ()
runAnalysisWithWorkersNoLoad ananame handles moduleName =
  (lookupRegAnaServer ananame) moduleName handles False >> done

--- Generic operation to analyze a module.
--- The parameters are the analysis, the show operation for analysis results,
--- the name of the main module to be analyzed, the handles for the workers,
--- and a flag indicating whether the analysis results should be loaded
--- and returned (if the flag is false, the result contains the empty
--- program information).
--- An error occurred during the analysis is returned as `(Right ...)`.
analyzeAsString :: Analysis a -> (a->String) -> String -> [Handle] -> Bool
                -> IO (Either (ProgInfo String) String)
analyzeAsString analysis showres modname handles load = do
  analyzeMain analysis modname handles load >>=
    return . either (Left . mapProgInfo showres) Right

--- Generic operation to analyze a module.
--- The parameters are the analysis, the name of the main module
--- to be analyzed, the handles for the workers,
--- and a flag indicating whether the analysis results should be loaded
--- and returned (if the flag is false, the result contains the empty
--- program information).
--- An error occurred during the analysis is returned as `(Right ...)`.
analyzeMain :: Analysis a -> String -> [Handle] -> Bool
            -> IO (Either (ProgInfo a) String)
analyzeMain analysis modname handles load = do
  let ananame = analysisName analysis
  debugMessage 2 ("start analysis "++modname++"/"++ananame)
  modulesToDo <- getModulesToAnalyze analysis modname
  workresult <-
    if null modulesToDo
    then return Nothing
    else do
     prepareCombinedAnalysis analysis modname (map fst modulesToDo) handles
     numworkers <- numberOfWorkers
     if numworkers>0
       then do debugMessage 2 "start WorkerLoop"
               workerLoop handles [] ananame modname modulesToDo []
       else analyzeLocally ananame (map fst modulesToDo)
  result <-
    maybe (if load
           then do debugMessage 3 ("Reading analysis of: "++modname)
                   loadCompleteAnalysis ananame modname >>= return . Left
           else return (Left emptyProgInfo))
          (return . Right)
          workresult
  debugMessage 4 ("result: " ++ either showProgInfo id result)
  return result

-- Analyze a module and all its imports locally without worker processes.
analyzeLocally :: String -> [String] -> IO (Maybe String)
analyzeLocally ananame modules = do
  debugMessage 3 ("Local analysis of: "++ananame++"/"++show modules)
  (lookupRegAnaWorker ananame) modules -- run client
  return Nothing


-- Perform the first analysis part of a combined analysis
-- so that their results are available for the main analysis.
prepareCombinedAnalysis:: Analysis a -> String -> [String] -> [Handle] -> IO ()
prepareCombinedAnalysis analysis moduleName depmods handles =
  if isCombinedAnalysis analysis
  then
    if isSimpleAnalysis analysis
    then do
      -- the directly imported interface information might be required...
      importedModules <- getImports moduleName
      mapIO_ (runAnalysisWithWorkersNoLoad baseAnaName handles)
             (importedModules++[moduleName])
    else do
      -- for a dependency analysis, the information of all implicitly
      -- imported modules might be required:
      mapIO_ (runAnalysisWithWorkersNoLoad baseAnaName handles) depmods
  else done
 where
   baseAnaName = baseAnalysisName analysis

--------------------------------------------------------------------
