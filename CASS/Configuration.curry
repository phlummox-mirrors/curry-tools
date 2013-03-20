--------------------------------------------------------------------------
--- This module supports the configuration of the analysis system
--- and provides access to some values in Config file.
---
--- It also provides an operation to get the port number of
--- the analysis server (which is implicitly started if necessary).
---
--- @author Michael Hanus
--- @version March 2013
--------------------------------------------------------------------------

module Configuration
 (systemBanner,baseDir,getServerAddress,updateRCFile,getFPMethod,getWithPrelude,
  storeServerPortNumber,removeServerPortNumber,getServerPortNumber,
  getDefaultPath,waitTime,numberOfWorkers,debugMessageLevel) where

import System
import Distribution(installDir,curryCompiler)
import PropertyFile
import ReadNumeric
import FilePath(FilePath, (</>), (<.>))
import Directory
import ReadShowTerm
import Sort(mergeSort)
import Global
import Char(isSpace)

systemBanner =
  let bannerText = "CASS: Curry Analysis Server System ("++
                   "version of 20/03/2013 for "++curryCompiler++")"
      bannerLine = take (length bannerText) (repeat '=')
   in bannerLine ++ "\n" ++ bannerText ++ "\n" ++ bannerLine


--- The base directory of the analysis tool containing all programs.
--- Required to copy the configuration file and to the find executables
--- of the server and the workers.
baseDir = installDir ++ "/currytools/CASS"

--- The address of the server when it is connected from the worker clients.
getServerAddress = return "127.0.0.1" -- run only on local machine

--------------------------------------------------------------------------
-- Name of user property file:
propertyFileName = getHomeDirectory >>= return . (</> ".curryanalysisrc")

defaultPropertyFileName = baseDir </> "curryanalysisrc"

--- Install user property file if it does not exist.
installPropertyFile = do
  fname <- propertyFileName
  pfexists <- doesFileExist fname
  if pfexists then done else do
    copyFile defaultPropertyFileName fname
    putStrLn ("New analysis configuration file '"++fname++"' installed.")

--- Reads the rc file (which must be present) and compares the definitions
--- with the distribution rc file. If the set of variables is different,
--- update the rc file with the distribution but keep the user's definitions.
updateRCFile :: IO ()
updateRCFile = do
  installPropertyFile
  rcName    <- propertyFileName
  userprops <- readPropertyFile rcName
  distprops <- readPropertyFile defaultPropertyFileName
  if (rcKeys userprops == rcKeys distprops) then done else do
    putStrLn $ "Updating \"" ++ rcName ++ "\"..."
    renameFile rcName $ rcName <.> "bak"
    copyFile defaultPropertyFileName rcName
    mapIO_ (\ (n, v) -> maybe done
              (\uv -> if uv==v then done else updatePropertyFile rcName n uv)
              (lookup n userprops))
           distprops

rcKeys :: [(String, String)] -> [String]
rcKeys = mergeSort (<=) . map fst

--- Reads the user property file (which must be installed!)
--- and store the properties in a global variable for next access.
getProperties :: IO [(String,String)]
getProperties =
  (readGlobal currProps) >>=
  maybe (do pfn <- propertyFileName
            props <- readPropertyFile pfn
            writeGlobal currProps (Just props)
            return props )
        return

--- Global variable to store the current properties.
currProps :: Global (Maybe [(String,String)])
currProps = global Nothing Temporary

--------------------------------------------------------------------------
--- Gets the name of file containing the current server port and pid
--- ($HOME has to be set) 
getServerPortFileName = do
  homeDir <- getHomeDirectory
  return $ homeDir++"/.curryanalysis.port"

--- Stores the current server port number together with the pid of
--- the server process.
storeServerPortNumber :: Int -> IO ()
storeServerPortNumber portnum = do
  mypid <- getPID
  serverPortFileName <- getServerPortFileName
  writeQTermFile serverPortFileName (portnum,mypid)

--- Removes the currently stored server port number.
removeServerPortNumber :: IO ()
removeServerPortNumber = getServerPortFileName >>= removeFile

readServerPortPid :: IO (Int,Int)
readServerPortPid = getServerPortFileName >>= readQTermFile

--- Reads the current server port number. If the server is not running,
--- it is also started.
getServerPortNumber :: IO Int
getServerPortNumber = do
  serverPortFileName <- getServerPortFileName
  exfile <- doesFileExist serverPortFileName
  if exfile
   then do (portnum,pid) <- readServerPortPid
           flag <- system ("ps -p "++show pid++" > /dev/null")
           if flag==0
            then return portnum
            else do removeFile serverPortFileName
                    getServerPortNumber
   else do debugMessageLevel 2 "Starting analysis server..."
           tcmd <- getTerminalCommand
           let serverCmd = baseDir++"/cass"
           if all isSpace tcmd
            then system ("\""++serverCmd++"\"  > /dev/null 2>&1 &")
            else system (tcmd++" \""++baseDir++"/cass\" &")
           sleep 1
           waitForServerPort serverPortFileName
 where
  waitForServerPort serverPortFileName = do
    exfile <- doesFileExist serverPortFileName
    if exfile
     then readServerPortPid >>= return . fst
     else do debugMessageLevel 2 "Waiting for server start..."
             sleep 1
             waitForServerPort serverPortFileName

--------------------------------------------------------------------------
-- Get terminalCommand from Config file
getTerminalCommand = do
  properties <- getProperties
  let tcmd = lookup "terminalCommand" properties
  return (maybe "" id tcmd)

-- Get the fixpoint computation method from Config file
getFPMethod =
  getProperties >>= return . maybe "simple" id . lookup "fixpoint"

-- Get the option to analyze also the prelude from Config file
getWithPrelude =
  getProperties >>= return . maybe "yes" id . lookup "prelude"

-- timeout for network message passing: -1 is wait time infinity
waitTime = -1  

-- Default number of workers (if the number is not found in the
-- configuration file).
defaultWorkers=0

--- Gets the default load path from the property file (added add the end
--- of CURRYPATH).
getDefaultPath = do
  currypath <- getEnviron "CURRYPATH"
  properties <- getProperties
  let proppath = lookup "path" properties
  return $ case proppath of
    Just value -> if all isSpace value then currypath else
                  if null currypath then value else currypath++':':value
    Nothing -> currypath

-- number of worker threads running at the same time
numberOfWorkers = do
  properties <- getProperties
  let number = lookup "numberOfWorkers" properties
  case number of
    Just value -> do 
      case (readInt value) of
        Just (int,_) -> return int
        Nothing -> return defaultWorkers
    Nothing -> return defaultWorkers 

--- Prints a message if debugging level (as specified in the Config file)
--- is at least n:
debugMessageLevel :: Int -> String -> IO ()
debugMessageLevel n message = do
  properties <- getProperties
  let number = lookup "debugLevel" properties
  case number of
    Just value -> do 
      case (readInt value) of
        Just (dl,_) -> if dl>=n then putStrLn message else done
        Nothing -> done
    Nothing -> done

-- Debug level:
-- 0 : show nothing
-- 1 : show worker activity, e.g., timings
-- 2 : show server communication
-- 3 : ...and show read/store information
-- 4 : ...show also stored/computed analysis data
