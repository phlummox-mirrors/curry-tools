--------------------------------------------------------------------------
--- This module contains operations to load and store analysis information
--- persistently in files.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version March 2013
--------------------------------------------------------------------------

module LoadAnalysis where

import Directory
import FileGoodies(separatorChar,splitDirectoryBaseName,stripSuffix)
import System(system,getArgs,getEnviron)
import GenericProgInfo
import Configuration(debugMessageLevel,getWithPrelude)
import IO
import FiniteMap
import ReadShowTerm(readQTerm,showQTerm)
import FlatCurry(QName)
import CurryFiles(findSourceFileInLoadPath)

debugMessage n message = debugMessageLevel n ("LoadAnalysis: "++message)

--- Get the file name in which analysis results are stored
--- (without suffix ".pub" or ".priv")
-- TODO: does not work for Windows
getAnalysisBaseFile :: String -> String -> IO String
getAnalysisBaseFile moduleName anaName = do
  analysisDirectory <- getAnalysisDirectory
  fileName <- findSourceFileInLoadPath moduleName
  let (fileDir,_) = splitDirectoryBaseName fileName
  if fileDir == "."
    then do
      currentDir <- getCurrentDirectory
      return (analysisDirectory++currentDir++"/"++moduleName++"."++anaName)
    else
     if head fileDir /= '/' -- TODO: does not work for Windows
     then do -- is relative path name
       currentDir <- getCurrentDirectory
       return (analysisDirectory++currentDir++"/"++fileDir++"/"++
               moduleName++"."++anaName)
     else return (analysisDirectory++fileDir++"/"++moduleName++"."++anaName)

--- Get the file name in which public analysis results are stored.
getAnalysisPublicFile :: String -> String -> IO String
getAnalysisPublicFile modname ananame = do
  getAnalysisBaseFile modname ananame >>= return . (++".pub")

-- directory where analysis info files are stored ($HOME has to be set) 
getAnalysisDirectory :: IO String
getAnalysisDirectory = do
  homeDir <- getHomeDirectory
  return (homeDir++"/.curry/Analysis/")

-- splits directory path in hierarchic list of folders of path
splitDirectories :: String -> [String]
splitDirectories dir = 
  let (rbase,rdir) = break (==separatorChar) (reverse dir) in
    if null rdir then []
    else (splitDirectories (reverse (tail rdir)))++[(reverse rbase)]

-- loads analysis results for a list of modules
getInterfaceInfos :: String -> [String] -> IO (ProgInfo a)
getInterfaceInfos _ [] = return emptyProgInfo
getInterfaceInfos anaName (mod:mods) =
  do modInfo  <- loadPublicAnalysis anaName mod 
     modsInfo <- getInterfaceInfos anaName mods
     return (combineProgInfo modInfo modsInfo)

--- Gets the file name in which default analysis values different from
--- standard start values are stored. Typically, such a file contains
--- specific analysis information for external operations.
--- The file must contain a term of the type `[(String,a)]` where
--- the first component of each pair is the name of the operation
--- (it is assumed that this denotes an operation of the current module)
--- and the second component is an analysis value.
loadDefaultAnalysisValues :: String -> String -> IO [(QName,a)]
loadDefaultAnalysisValues anaName moduleName = do
  fileName <- findSourceFileInLoadPath moduleName
  let defaultFileName = stripSuffix fileName ++ ".defaults."++anaName
  fileExists <- doesFileExist defaultFileName
  if fileExists
    then do debugMessage 3 ("Load default values from " ++ defaultFileName)
            defaultValues <- readFile defaultFileName >>= return . readQTerm
            return (map (\ (f,a) -> ((moduleName,f),a)) defaultValues)
    else return []

--- Loads the currently stored analysis information for a module.
loadCompleteAnalysis :: String -> String -> IO (ProgInfo _)
loadCompleteAnalysis ananame mainModule =
  getAnalysisBaseFile mainModule ananame >>= readAnalysisFiles

--- Reads analysis result from file for the public entities of a given module.
loadPublicAnalysis:: String -> String -> IO (ProgInfo a) 
loadPublicAnalysis anaName moduleName = do
  withprelude <- getWithPrelude
  if withprelude=="no" && moduleName=="Prelude"
   then return emptyProgInfo
   else getAnalysisPublicFile moduleName anaName >>= readAnalysisPublicFile

--- Store current import dependencies.
storeImportModuleList :: String -> [String] -> IO ()
storeImportModuleList modname modlist = do
  importListFile <- getAnalysisBaseFile modname "IMPORTLIST"
  let (dir,_) = splitDirectoryBaseName importListFile
  createDirectoryR dir
  writeFile importListFile (showQTerm modlist)

--- Gets the file containing import dependencies for a main module
--- (if it exists).
getImportModuleListFile :: String -> IO (Maybe String)
getImportModuleListFile modname = do
  importListFile <- getAnalysisBaseFile modname "IMPORTLIST"
  iflExists <- doesFileExist importListFile
  return $ if iflExists then Just importListFile else Nothing

--- Store an analysis results in a file and create directories if neccesssary.
--- The first argument is the analysis name.
storeAnalysisResult:: String -> String -> ProgInfo a -> IO ()
storeAnalysisResult ananame moduleName result = do
   baseFileName <- getAnalysisBaseFile moduleName ananame
   let (dir,_) = splitDirectoryBaseName baseFileName
   createDirectoryR dir
   debugMessage 4 ("Analysis result: " ++ showProgInfo result)
   writeAnalysisFiles baseFileName result

-- creates directory (and all needed root-directories) recursively
createDirectoryR::String->IO()
createDirectoryR dir = do
  let dirList = splitDirectories dir
  createDirectoryRHelp "" dirList

createDirectoryRHelp::String->[String]->IO()
createDirectoryRHelp _ [] = done  
createDirectoryRHelp dirname (dir:restList) = do
  let createdDir = dirname++"/"++dir
  dirExists <- doesDirectoryExist createdDir
  if (dirExists)
    then done 
    else createDirectory createdDir
  createDirectoryRHelp createdDir restList


-- delete all savefiles of analysis
deleteAnalysisFiles ananame = do
   analysisDir <- getAnalysisDirectory
   system ("find "++analysisDir++" -name '*."++ananame++"' -type f -delete")

