--------------------------------------------------------------------------
--- This module contains operations to load and store analysis information
--- persistently in files.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version January 2015
--------------------------------------------------------------------------

-- {-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module LoadAnalysis where

import Directory
import Distribution(stripCurrySuffix)
import FilePath
import System(system,getArgs,getEnviron)
import GenericProgInfo
import Configuration(debugMessage,getWithPrelude)
import IO
import FiniteMap
import ReadShowTerm(readQTerm,showQTerm)
import FlatCurry(QName)
import CurryFiles(findModuleSourceInLoadPath)


--- Get the file name in which analysis results are stored
--- (without suffix ".pub" or ".priv")
getAnalysisBaseFile :: String -> String -> IO String
getAnalysisBaseFile moduleName anaName = do
  analysisDirectory <- getAnalysisDirectory
  currentDir        <- getCurrentDirectory >>= return . dropDrive
  let modAnaName = moduleName <.> anaName
  (fileDir,_) <- findModuleSourceInLoadPath moduleName
  if isAbsolute fileDir
   then return (analysisDirectory </> dropDrive fileDir </> modAnaName)
   else return (analysisDirectory </> currentDir </> fileDir </> modAnaName)

--- Get the file name in which public analysis results are stored.
getAnalysisPublicFile :: String -> String -> IO String
getAnalysisPublicFile modname ananame = do
  getAnalysisBaseFile modname ananame >>= return . (<.> "pub")

-- directory where analysis info files are stored ($HOME has to be set) 
getAnalysisDirectory :: IO String
getAnalysisDirectory = do
  homeDir <- getHomeDirectory
  return (homeDir </> ".curry" </> "Analysis")

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
  (_,fileName) <- findModuleSourceInLoadPath moduleName
  let defaultFileName = stripCurrySuffix fileName ++ ".defaults."++anaName
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
  createDirectoryR (dropFileName importListFile)
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
storeAnalysisResult :: String -> String -> ProgInfo a -> IO ()
storeAnalysisResult ananame moduleName result = do
   baseFileName <- getAnalysisBaseFile moduleName ananame
   createDirectoryR (dropFileName baseFileName)
   debugMessage 4 ("Analysis result: " ++ showProgInfo result)
   writeAnalysisFiles baseFileName result

-- creates directory (and all needed root-directories) recursively
createDirectoryR :: String -> IO ()
createDirectoryR maindir = 
  let (drv,dir) = splitDrive maindir
   in createDirectories drv (splitDirectories dir)
 where
  createDirectories _ [] = done  
  createDirectories dirname (dir:dirs) = do
    let createdDir = dirname </> dir
    dirExists <- doesDirectoryExist createdDir
    unless dirExists $ do
      debugMessage 3 ("Creating directory '"++createdDir++"'...")
      createDirectory createdDir
    createDirectories createdDir dirs

-- delete all savefiles of analysis
deleteAnalysisFiles :: String -> IO Int
deleteAnalysisFiles ananame = do
   analysisDir <- getAnalysisDirectory
   system ("find "++analysisDir++" -name '*."++ananame++"' -type f -delete")

