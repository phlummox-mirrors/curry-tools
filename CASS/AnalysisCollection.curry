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
  functionAnalysisInfos,registeredAnalysisNames,
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
import Demandedness

--------------------------------------------------------------------
--- Each analysis used in our tool must be registered in this list
--- together with an operation to show the analysis result as a string.
registeredAnalysis :: [RegisteredAnalysis]
registeredAnalysis =
  [cassAnalysis "Overlapping rules"          overlapAnalysis showOverlap
  ,cassAnalysis "Deterministic operations"   nondetAnalysis  showDet
  ,cassAnalysis "Right-linear operations"    rlinAnalysis    showRightLinear
  ,cassAnalysis "Solution completeness"      solcompAnalysis showSolComplete
  ,cassAnalysis "Pattern completeness"       patCompAnalysis showComplete
  ,cassAnalysis "Totally defined operations" totalAnalysis   showTotally
  ,cassAnalysis "Indeterministic operations" indetAnalysis   showIndet
  ,cassAnalysis "Demanded arguments"         demandAnalysis  showDemand
  ,cassAnalysis "Higher-order datatypes"     hiOrdType       showOrder
  ,cassAnalysis "Higher-order constructors"  hiOrdCons       showOrder
  ,cassAnalysis "Higher-order functions"     hiOrdFunc       show
  ,cassAnalysis "Sibling constructors"       siblingCons     show
  ]



--------------------------------------------------------------------
-- Static part of this module follows below
--------------------------------------------------------------------

--- This auxiliary operation creates a new program analysis to be used
--- by the server/client analysis tool from a given analysis and
--- analysis show function. The first argument is a short title for the
--- analysis.
cassAnalysis :: String -> Analysis a -> (a->String) -> RegisteredAnalysis
cassAnalysis title analysis showres =
  RegAna (analysisName analysis)
         (isFunctionAnalysis analysis)
         title
         (analyzeAsString analysis showres)
         (analysisClient analysis)

--- The type of all registered analysis.
--- The components are as follows:
--- * the name of the analysis
--- * is this a function analysis?
--- * a long meaningful name of the analysis
--- * the operation used by the server to distribute analysis work
---   to the clients
--- * the worker operation to analyze a list of modules
data RegisteredAnalysis =
  RegAna String
         Bool
         String
         (String -> [Handle] -> Bool -> IO (Either (ProgInfo String) String))
         ([String] -> IO ())

regAnaName (RegAna n _ _ _ _) = n

regAnaServer (RegAna _ _ _ a _) = a

regAnaWorker (RegAna _ _ _ _ a) = a

--- Names of all registered analyses.
registeredAnalysisNames = map regAnaName registeredAnalysis

--- Names and titles of all registered function analyses.
functionAnalysisInfos =
  map (\ (RegAna n _ t _ _) -> (n,t))
      (filter (\ (RegAna _ fa _ _ _) -> fa) registeredAnalysis)

lookupRegAna :: String -> [RegisteredAnalysis] -> Maybe RegisteredAnalysis
lookupRegAna _ [] = Nothing
lookupRegAna aname (ra@(RegAna raname _ _ _ _) : ras) =
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
