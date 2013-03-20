-----------------------------------------------------------------------
--- This module provides some operations to deal with Curry/FlatCurry files
--- and their dependencies. of analysis files.
---
--- @author Michael Hanus
--- @version March 2013
-----------------------------------------------------------------------

module CurryFiles(getImports,getSourceFileTime,readNewestFlatCurry) where

import FlatCurry
import FlatCurryGoodies(progImports)
import Directory(doesFileExist,getModificationTime)
import Distribution(findFileInLoadPath)
import Time(ClockTime)
import Configuration(debugMessageLevel)


-- Get the imports of a module.
getImports :: String -> IO [String]
getImports moduleName = do
  debugMessageLevel 3 ("Reading interface of module "++moduleName)
  readNewestFlatCurryInt moduleName >>= return . progImports

-- Get timestamp of a Curry source file (together with its name)
getSourceFileTime :: String -> IO (String,ClockTime)
getSourceFileTime moduleName = do
  fileName <- findFileInLoadPath (moduleName++".curry")
  time <- getModificationTime fileName
  return (moduleName,time)

--- Returns name of a source file of a module if its FlatCurry file
--- exists and is newer than the source file.
flatCurryFileNewer :: String -> IO (Maybe String)
flatCurryFileNewer modname = do
  sourceFileName <- findFileInLoadPath (modname++".curry")
  stime <- getModificationTime sourceFileName
  let fcyFileName = flatCurryFileName sourceFileName
  fcyExists <- doesFileExist fcyFileName
  if fcyExists
   then do itime <- getModificationTime fcyFileName
           return (if itime >= stime then Just sourceFileName else Nothing)
   else return Nothing

--- Returns the newest FlatCurry program for a module.
--- The source program is parsed if the interface older than the source,
--- otherwise the FlatCurry program is read without parsing
--- (note that this returns only the correct version if the
--- imported modules are already parsed or are not relevant here).
readNewestFlatCurry :: String -> IO Prog
readNewestFlatCurry modname =
  flatCurryFileNewer modname >>=
  maybe (readFlatCurry modname) (readFlatCurryFile . flatCurryFileName)

--- Returns the newest FlatCurry interface for a module.
--- The source program is parsed if the interface older than the source,
--- otherwise the FlatCurry interface file is read without parsing
--- (note that this returns only the correct version if the
--- imported modules are already parsed or are not relevant here).
readNewestFlatCurryInt :: String -> IO Prog
readNewestFlatCurryInt modname =
  flatCurryFileNewer modname >>=
  maybe (readFlatCurryInt modname) (readFlatCurryFile . flatCurryIntName)

