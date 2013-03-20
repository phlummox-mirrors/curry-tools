-----------------------------------------------------------------------
--- Operations to handle dependencies of analysis files.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version March 2013
-----------------------------------------------------------------------

module AnalysisDependencies(getModulesToAnalyze,reduceDependencies,
   readNewestFlatCurry) where

import FlatCurry
import FlatCurryGoodies(progImports)
import ReadShowTerm(readQTerm,showQTerm)
import Directory(doesFileExist,getModificationTime)
import Distribution(findFileInLoadPath)
import Maybe(fromMaybe)
import List(delete)
import Time(ClockTime)
import Analysis
import GenericProgInfo
import LoadAnalysis(getAnalysisPublicFile,storeImportModuleList,getImportModuleListFile)
import Configuration(debugMessageLevel,getWithPrelude)
import CurryFiles

debugMessage dl message = debugMessageLevel dl ("Dependencies: "++message)

-----------------------------------------------------------------------
--- Compute the modules and their imports which must be analyzed
--- w.r.t. a given analysis and main module.
getModulesToAnalyze :: Analysis a -> String -> IO [(String,[String])]
getModulesToAnalyze analysis moduleName =
  if isSimpleAnalysis analysis
  then do
    ananewer <- isAnalysisFileNewer ananame moduleName
    return (if ananewer then [] else [(moduleName,[])])
  else do
   valid <- isAnalysisValid ananame moduleName
   if valid 
    then do
     debugMessage 3 ("Analysis file for '"++moduleName++"' up-to-date")
     return []
    else do
     moduleList <- getDependencyList [moduleName] []
     debugMessage 3 ("Complete module list: "++ show moduleList)
     storeImportModuleList moduleName (map fst moduleList)
     sourceTimeList <- mapIO getSourceFileTime (map fst moduleList)
     --debugMessage 3 ("Source time list: "++ show sourceTimeList)
     anaTimeList <- mapIO (getAnaFileTime ananame) (map fst moduleList)
     --debugMessage 3 ("Analysis time list: "++ show anaTimeList)
     let (modulesToDo,modulesUpToDate) =
            findModulesToAnalyze moduleList anaTimeList sourceTimeList ([],[])
     --debugMessage 3 ("Modules up-to-date: "++ show modulesUpToDate)
     withprelude <- getWithPrelude
     let modulesToAnalyze =
           if withprelude=="no"
           then let reduced = reduceDependencies modulesToDo 
                                              (modulesUpToDate ++ ["Prelude"])
                 in case reduced of (("Prelude",_):remaining) -> remaining
                                    _ -> reduced
           else reduceDependencies modulesToDo modulesUpToDate
     debugMessage 3 ("Modules to analyze: " ++ show modulesToAnalyze)
     return modulesToAnalyze
 where
   ananame = analysisName analysis

-- Check whether the analysis file is newer than the source file.
isAnalysisFileNewer ananame modname = do
  atime <- getAnaFileTime ananame modname
  stime <- getSourceFileTime modname
  return (snd atime >= Just (snd stime))

-- Read current import dependencies.
isAnalysisValid ananame modname =
  getImportModuleListFile modname >>= maybe
    (return False)
    (\importListFile -> do
      itime <- getModificationTime importListFile
      stime <- getSourceFileTime modname >>= return . snd
      if itime>=stime
       then do
        implist <- readFile importListFile >>= return . readQTerm
        sourceTimeList <- mapIO getSourceFileTime implist
        anaTimeList <- mapIO (getAnaFileTime ananame) implist
        return (all (uncurry (>=))
                 (zip (map snd anaTimeList) (map (Just . snd) sourceTimeList)))
       else return False)


--- Gets the list of all modules required by the first module.
--- The result is sorted according to their dependencies
--- (Prelude first, main module last)
getDependencyList :: [String] -> [(String,[String])]
                  -> IO [(String,[String])]
getDependencyList [] moddeps = return moddeps
getDependencyList (mname:mods) moddeps =
  let (newmoddeps,imps,processed) = checkAndReorder mname [] moddeps
   in if not processed
      then do --debugMessage 3 ("Getting imports of "++ mname)
              --debugMessage 3 ("Still to do: "++ show mods)
              imports <- getImports mname
              let newimports = filter (`notElem` mods) imports
              getDependencyList (mods++newimports) ((mname,imports):moddeps)
      else getDependencyList (mods++imps) newmoddeps

checkAndReorder _ _ [] = ([], [], False)
checkAndReorder key1 list1 ((key2,value):rest)
  | key1==key2 = ((key2,value):reverse list1++rest, value, True)
  | otherwise = checkAndReorder key1 ((key2,value):list1) rest

-- get timestamp of analysis file
getAnaFileTime :: String -> String -> IO (String,Maybe ClockTime)
getAnaFileTime anaName moduleName = do
  fileName <- getAnalysisPublicFile moduleName anaName
  fileExists <- doesFileExist fileName
  if fileExists
    then do time <- getModificationTime fileName
            return (moduleName,Just time)
    else return (moduleName,Nothing)


-- check if analysis result of a module can be loaded or needs to be
-- newly analyzed
findModulesToAnalyze :: [(String,[String])] -> [(String,Maybe ClockTime)]
                     -> [(String,ClockTime)]
                     -> ([(String,[String])],[String])
                     -> ([(String,[String])],[String])
findModulesToAnalyze [] _ _ (modulesToDo,modulesUpToDate) =
  (reverse modulesToDo, modulesUpToDate)
findModulesToAnalyze (m:ms) anaTimeList sourceTimeList resultLists = 
  let (mod,imports)= m 
      (modulesToDo,modulesUpToDate) = resultLists in
  case (lookup mod anaTimeList) of
    Just Nothing    -> findModulesToAnalyze ms anaTimeList sourceTimeList
                                            ((m:modulesToDo),modulesUpToDate)
    Just(Just time) ->
      if checkTime mod time imports anaTimeList sourceTimeList modulesToDo
      then findModulesToAnalyze ms anaTimeList sourceTimeList
                                (modulesToDo,(mod:modulesUpToDate))
      else findModulesToAnalyze ms anaTimeList sourceTimeList
                                ((m:modulesToDo),modulesUpToDate)

-- function to check if result file is up-to-date
-- compares timestamp of analysis result file with module source file 
-- and with timpestamp of result files of all imported modules
checkTime :: String -> ClockTime -> [String] -> [(String,Maybe ClockTime)]
          -> [(String,ClockTime)] -> [(String,[String])] -> Bool
checkTime mod time1 [] _ sourceTimeList _ =
  (Just time1) >= (lookup mod sourceTimeList)
checkTime mod time1 (impt:impts) anaTimeList sourceTimeList resultList =
  ((lookup impt resultList)==Nothing)
  &&((Just time1)>=(fromMaybe Nothing (lookup impt anaTimeList)))
  &&(checkTime mod time1 impts anaTimeList sourceTimeList resultList)

-----------------------------------------------------------------------
-- Remove the module analysis dependencies (first argument) w.r.t.
-- a list of modules that are already analyzed (second argument).
reduceDependencies :: [(String,[String])] -> [String] -> [(String,[String])]
reduceDependencies modulesToDo [] = modulesToDo
reduceDependencies modulesToDo (mod:mods) =
  let modulesToDo2 = map (\ (m,list) -> (m,(delete mod list))) modulesToDo
   in reduceDependencies modulesToDo2 mods

