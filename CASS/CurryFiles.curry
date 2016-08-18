-----------------------------------------------------------------------
--- This module provides some operations to deal with Curry/FlatCurry files
--- and their dependencies.
---
--- @author Michael Hanus
--- @version January 2015
-----------------------------------------------------------------------

module CurryFiles(getImports,findModuleSourceInLoadPath,
                  getSourceFileTime,getFlatCurryFileTime,
                  readNewestFlatCurry) where

import Distribution      (lookupModuleSourceInLoadPath)
import Directory         (doesFileExist, getModificationTime)
import FlatCurry.Types
import FlatCurry.Files
import FlatCurry.Goodies (progImports)
import Time              (ClockTime)

import Configuration     (debugMessage)


-- Get the imports of a module.
getImports :: String -> IO [String]
getImports moduleName = do
  debugMessage 3 ("Reading interface of module "++moduleName)
  readNewestFlatCurryInt moduleName >>= return . progImports

--- Returns a directory name and the actual source file name for a module
--- by looking up the module source in the current load path.
--- If the module is hierarchical, the directory is the top directory
--- of the hierarchy.
--- An error is raised if there is no corresponding source file.
findModuleSourceInLoadPath :: String -> IO (String,String)
findModuleSourceInLoadPath modname =
  lookupModuleSourceInLoadPath modname >>=
  maybe (error $ "Source file for module '"++modname++"' not found!")
        return

-- Get timestamp of a Curry source module file (together with the module name)
getSourceFileTime :: String -> IO (String,ClockTime)
getSourceFileTime moduleName = do
  (_,fileName) <- findModuleSourceInLoadPath moduleName
  time <- getModificationTime fileName
  return (moduleName,time)

-- Get timestamp of FlatCurry file (together with the module name)
getFlatCurryFileTime :: String -> IO (String,Maybe ClockTime)
getFlatCurryFileTime modname =
  lookupFlatCurryFileInLoadPath modname >>=
  maybe (return (modname, Nothing))
        (\fcyFileName -> do
            ftime <- getModificationTime fcyFileName
            return (modname, Just ftime))

--- Returns name of the FlatCurry file of a module if this file exists
--- and is newer than the source file.
flatCurryFileNewer :: String -> IO (Maybe String)
flatCurryFileNewer modname = do
  (_,sourceFileName) <- findModuleSourceInLoadPath modname
  stime <- getModificationTime sourceFileName
  lookupFlatCurryFileInLoadPath modname >>=
   maybe (return Nothing)
         (\fcyFileName -> do
            itime <- getModificationTime fcyFileName
            return (if itime >= stime then Just fcyFileName else Nothing))

--- Returns the newest FlatCurry program for a module.
--- The source program is parsed if the interface older than the source,
--- otherwise the FlatCurry program is read without parsing
--- (note that this returns only the correct version if the
--- imported modules are already parsed or are not relevant here).
readNewestFlatCurry :: String -> IO Prog
readNewestFlatCurry modname =
  flatCurryFileNewer modname >>=
  maybe (readFlatCurry modname) readFlatCurryFile

--- Returns the newest FlatCurry interface for a module.
--- The source program is parsed if the interface older than the source,
--- otherwise the FlatCurry interface file is read without parsing
--- (note that this returns only the correct version if the
--- imported modules are already parsed or are not relevant here).
readNewestFlatCurryInt :: String -> IO Prog
readNewestFlatCurryInt modname =
  flatCurryFileNewer modname >>=
  maybe (readFlatCurryInt modname) (readFlatCurryFile . flat2intName)

--- Translates FlatCurry file name to corresponding FlatCurry interface
--- file name.
flat2intName :: String -> String
flat2intName fn = reverse ("tnif" ++ drop 3 (reverse fn))