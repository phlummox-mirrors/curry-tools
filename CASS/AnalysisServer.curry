--------------------------------------------------------------------------
--- This is the main module of the analysis system.
--- One can either use the 'main' operation to start the system
--- in "server mode" or "batch mode" or use one of the operations below
--- to use the analysis system in another Curry program.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version August 2014
--------------------------------------------------------------------------

module AnalysisServer(main, initializeAnalysisSystem,
                      analyzeModuleForBrowser, analyzeFunctionForBrowser,
                      analyzeGeneric, analyzePublic, analyzeInterface)
  where

import ReadNumeric(readNat)
import Char(isSpace)
import Directory
import FlatCurry(QName)
import Socket(Socket(..),listenOn,listenOnFresh,sClose,waitForSocketAccept)
import IO
import ReadShowTerm(readQTerm,showQTerm)
import System(system,sleep,setEnviron,getArgs)
import FileGoodies(stripSuffix,splitDirectoryBaseName)
import AnalysisCollection
import ServerFormats
import ServerFunctions(WorkerMessage(..))
import Configuration
import GenericProgInfo
import Analysis(Analysis,AOutFormat(..))

-- Messages to communicate with the analysis server from external programs.
data AnalysisServerMessage = 
    GetAnalysis
  | AnalyzeModule    String String String Bool
  | AnalyzeEntity  String String String String
  | StopServer
  | SetCurryPath String
  | ParseError

--- Main function to start the server.
--- Without any program arguments, the server is started on a socket.
--- Otherwise, it is started in batch mode to analyze a module.
main = do
  debugMessageLevel 1 systemBanner
  initializeAnalysisSystem
  args <- getArgs
  processArgs False args

processArgs enforce args = case args of
  [] -> mainServer Nothing
  ["-p",port]  -> maybe showError
                        (\ (p,r) -> if all isSpace r
                                    then mainServer (Just p)
                                    else showError )
                        (readNat port)
  ["-h"]       -> showHelp
  ["-?"]       -> showHelp
  ["--help"]   -> showHelp
  ("-r":rargs) -> processArgs True rargs
  (('-':'D':kvs):rargs) -> let (key,eqvalue) = break (=='=') kvs
                            in if null eqvalue
                               then showError
                               else do updateCurrentProperty key (tail eqvalue)
                                       processArgs enforce rargs
  [ananame,mname] ->
      if ananame `elem` registeredAnalysisNames
      then analyzeModule ananame (stripSuffix mname) enforce AText >>=
             putStrLn . formatResult mname "Text" Nothing True
      else showError
  _ -> showError
 where
  showError =
    error ("Illegal arguments (use '--help' for description):\n"++unwords args)

--- Initializations to be done when the system is started.
initializeAnalysisSystem :: IO ()
initializeAnalysisSystem = updateRCFile

showHelp = putStrLn $
  "Usage: cass <options> [-p <port>] :\n" ++
  "       start analysis system in server mode\n\n"++
  "       <port>: port number for communication\n" ++
  "               (if omitted, a free port number is selected)\n\n"++
  "Usage: cass <options> <analysis name> <module name> :\n"++
  "       analyze a module with a given analysis\n\n"++
  "where <options> can contain:\n"++
  "-Dname=val : set property (of ~/.curryanalysisrc) 'name' as 'val'\n"++
  "-r         : force re-analysis (i.e., ignore old analysis information)\n"++
  "\nRegistered analyses names:\n" ++
  unlines registeredAnalysisNames

--- Start the analysis server on a socket.
mainServer :: Maybe Int -> IO ()
mainServer mbport = do
  putStrLn "Start Server"
  (port1,socket1) <- maybe listenOnFresh
                           (\p -> listenOn p >>= \s -> return (p,s))
                           mbport
  putStrLn ("Server Port: "++show port1)
  storeServerPortNumber port1
  getDefaultPath >>= setEnviron "CURRYPATH" 
  numworkers <- numberOfWorkers
  if numworkers>0
   then do
    serveraddress <- getServerAddress
    (workerport,workersocket) <- listenOnFresh
    debugMessageLevel 2 ("SERVER: port to workers: "++show workerport)
    handles <- startWorkers numworkers workersocket serveraddress workerport []
    serverLoop socket1 handles
    sClose workersocket
   else
    serverLoop socket1 []


--- Run the analysis system to show the analysis results in the BrowserGUI.
--- Note that, before its first use, the analysis system must be initialized
--- by 'initializeAnalysisSystem'.
analyzeModuleForBrowser :: String -> String -> AOutFormat -> IO [(QName,String)]
analyzeModuleForBrowser ananame moduleName aoutformat =
  analyzeModule ananame moduleName False aoutformat >>=
    return . either pinfo2list (const [])
 where
   pinfo2list pinfo = let (pubinfo,privinfo) = progInfo2Lists pinfo
                       in pubinfo++privinfo

--- Run the analysis system to show the analysis result of a single function
--- in the BrowserGUI.
--- Note that before its first use, the analysis system must be initialized
--- by 'initializeAnalysisSystem'.
analyzeFunctionForBrowser :: String -> QName -> AOutFormat -> IO String
analyzeFunctionForBrowser ananame qn@(mname,_) aoutformat = do
  analyzeModule ananame mname False aoutformat >>=
    return . either (maybe "" id . lookupProgInfo qn) (const "")

--- Analyze a complete module for a given analysis result format.
--- The third argument is a flag indicating whether the
--- (re-)analysis should be enforced.
--- Note that before its first use, the analysis system must be initialized
--- by 'initializeAnalysisSystem'.
analyzeModule :: String -> String -> Bool -> AOutFormat
              -> IO (Either (ProgInfo String) String)
analyzeModule ananame moduleName enforce aoutformat = do
  let (mdir,mname) = splitDirectoryBaseName moduleName
  getDefaultPath >>= setEnviron "CURRYPATH"
  curdir <- getCurrentDirectory
  unless (mdir==".") $ setCurrentDirectory mdir
  numworkers <- numberOfWorkers
  aresult <-
   if numworkers>0
     then do
      serveraddress <- getServerAddress
      (port,socket) <- listenOnFresh
      handles <- startWorkers numworkers socket serveraddress port []
      result <- runAnalysisWithWorkers ananame aoutformat enforce handles mname
      stopWorkers handles
      sClose socket
      return result
     else runAnalysisWithWorkers ananame aoutformat enforce [] mname
  setCurrentDirectory curdir
  return aresult

--- Start the analysis system with a particular analysis.
--- The analysis must be a registered one if workers are used.
--- If it is a combined analysis, the base analysis must be also
--- a registered one.
analyzeGeneric :: Analysis a -> String -> IO (Either (ProgInfo a) String)
analyzeGeneric analysis moduleName = do
  initializeAnalysisSystem
  let (mdir,mname) = splitDirectoryBaseName moduleName
  getDefaultPath >>= setEnviron "CURRYPATH" 
  curdir <- getCurrentDirectory
  unless (mdir==".") $ setCurrentDirectory mdir
  numworkers <- numberOfWorkers
  aresult <-
    if numworkers>0
     then do
      serveraddress <- getServerAddress
      (port,socket) <- listenOnFresh
      handles <- startWorkers numworkers socket serveraddress port []
      result <- analyzeMain analysis mname handles False True
      stopWorkers handles
      sClose socket
      return result
     else
      analyzeMain analysis mname [] False True
  setCurrentDirectory curdir
  return aresult
 
--- Start the analysis system with a given analysis to compute properties
--- of a module interface.
--- The analysis must be a registered one if workers are used.
--- If it is a combined analysis, the base analysis must be also
--- a registered one.
analyzePublic :: Analysis a -> String -> IO (Either (ProgInfo a) String)
analyzePublic analysis moduleName =
  analyzeGeneric analysis moduleName
  >>= return . either (Left . publicProgInfo) Right

--- Start the analysis system with a given analysis to compute properties
--- of a module interface.
--- The analysis must be a registered one if workers are used.
--- If it is a combined analysis, the base analysis must be also
--- a registered one.
analyzeInterface :: Analysis a -> String -> IO (Either [(QName,a)] String)
analyzeInterface analysis moduleName =
  analyzeGeneric analysis moduleName
  >>= return . either (Left . publicListFromProgInfo) Right

--------------------------------------------------------------------------
-- start a number of workers at server start
startWorkers:: Int -> Socket -> String -> Int -> [Handle] -> IO [Handle]
startWorkers number workersocket serveraddress workerport handles = do
  if number>0
    then do
      debugMessageLevel 4 ("Number:"++(show number))
      let command = baseDir++"/cass_worker "++serveraddress++" "
                                            ++(show workerport)++" &"
      debugMessageLevel 4 ("system command: "++command)
      system command
      debugMessageLevel 4 ("Wait for socket accept for client "++show number)
      connection <- waitForSocketAccept workersocket waitTime
      debugMessageLevel 4 ("Socket accept for client "++show number)
      case connection of
        Just (_,handle) -> do
          startWorkers (number-1) workersocket serveraddress workerport
                       (handle:handles)
        Nothing -> do
          putStrLn ("startWorkers: connection error worker "++(show number))
          startWorkers (number-1) workersocket serveraddress workerport handles
    else return handles

-- stop all workers at server stop
stopWorkers [] = done
stopWorkers (handle:whandles) = do
  hPutStrLn handle (showQTerm StopWorker)
  hClose handle
  stopWorkers whandles

--------------------------------------------------------------------------
-- server loop to answer analysis requests over network
serverLoop socket1 whandles = do
  --debugMessageLevel 3 "SERVER: serverLoop"
  connection <- waitForSocketAccept socket1 waitTime
  case connection of 
    Just (_,handle) -> serverLoopOnHandle socket1 whandles handle
    Nothing -> do
      putStrLn "serverLoop: connection error: time out in waitForSocketAccept"
      sleep 1
      serverLoop socket1 whandles

--- Reads a line from an input handle and returns it.
hGetLineUntilEOF  :: Handle -> IO String
hGetLineUntilEOF h = do
  eof <- hIsEOF h
  if eof
   then return ""
   else do c <- hGetChar h
           if c=='\n' then return ""
                      else do cs <- hGetLineUntilEOF h
                              return (c:cs)

serverLoopOnHandle socket1 whandles handle = do
  eof <- hIsEOF handle
  if eof
   then do hClose handle
           debugMessageLevel 2 "SERVER connection: eof"
           serverLoop socket1 whandles
   else do
     string <- hGetLineUntilEOF handle
     debugMessageLevel 2 ("SERVER got message: "++string)
     let force = False
     case parseServerMessage string of
       ParseError -> do
         sendServerError handle ("Illegal message received: "++string)
         serverLoopOnHandle socket1 whandles handle
       GetAnalysis -> do
         sendServerResult handle showAnalysisNamesAndFormats
         serverLoopOnHandle socket1 whandles handle
       AnalyzeModule ananame outForm modname public ->
         catch (runAnalysisWithWorkers ananame AText force whandles modname >>=
                return . formatResult modname outForm Nothing public >>=
                sendResult)
               sendAnalysisError
       AnalyzeEntity ananame outForm modname functionName ->
         catch (runAnalysisWithWorkers ananame AText force whandles modname >>=
                return . formatResult modname outForm
                                      (Just functionName) False >>= sendResult)
               sendAnalysisError
       SetCurryPath path -> do
         setEnviron "CURRYPATH" path
         changeWorkerPath path whandles
         sendServerResult handle ""
         serverLoopOnHandle socket1 whandles handle
       StopServer -> do
         stopWorkers whandles
         sendServerResult handle ""
         hClose handle
         sClose socket1
         putStrLn "Stop Server"
         removeServerPortNumber
 where
  sendResult resultstring = do
    debugMessageLevel 4 ("formatted result:\n"++resultstring)
    sendServerResult handle resultstring
    serverLoopOnHandle socket1 whandles handle

  sendAnalysisError err = do
    sendServerError handle ("ERROR in analysis server: "++showError err)
    serverLoopOnHandle socket1 whandles handle

-- Send a server result in the format "ok <n>\n<result text>" where <n>
-- is the number of lines of the <result text>.
sendServerResult handle resultstring = do
  let resultlines = lines resultstring
  hPutStrLn handle ("ok " ++ show (length resultlines))
  hPutStr handle (unlines resultlines)
  hFlush handle

-- Send a server error in the format "error <error message>\n".
sendServerError handle errstring = do
  debugMessageLevel 1 errstring
  hPutStrLn handle ("error "++errstring)
  hFlush handle

-- Inform the worker threads about a given changed library search path
changeWorkerPath :: String -> [Handle] -> IO ()
changeWorkerPath _ [] = done
changeWorkerPath path (handle:whandles) = do
  hPutStrLn handle (showQTerm (ChangePath path))
  changeWorkerPath path whandles

-- parse incoming message for type of request
parseServerMessage :: String -> AnalysisServerMessage
parseServerMessage message = case words message of
  [] -> ParseError
  w:ws -> case w of 
    "GetAnalysis" -> GetAnalysis
    "AnalyzeModule" -> case ws of 
      s1:s2:s3:[] -> checkFormat s2 $ AnalyzeModule s1 s2 s3 False
    "AnalyzeInterface" -> case ws of 
      s1:s2:s3:[] -> checkFormat s2 $ AnalyzeModule s1 s2 s3 True
      _ -> ParseError
    "AnalyzeFunction" -> case ws of
      s1:s2:s3:s4:[] -> checkFormat s2 $ AnalyzeEntity s1 s2 s3 s4
      _ -> ParseError
    "AnalyzeTypeConstructor" -> case ws of
      s1:s2:s3:s4:[] -> checkFormat s2 $ AnalyzeEntity s1 s2 s3 s4
      _ -> ParseError
    "AnalyzeDataConstructor" -> case ws of
      s1:s2:s3:s4:[] -> checkFormat s2 $ AnalyzeEntity s1 s2 s3 s4
      _ -> ParseError  
    "SetCurryPath" -> case ws of
      s:[] -> SetCurryPath s
      _ -> ParseError
    "StopServer" -> StopServer
    _ -> ParseError 
 where
  checkFormat fmt msg = if fmt `elem` serverFormats then msg else ParseError

--- Show all analysis names and formats.
showAnalysisNamesAndFormats :: String
showAnalysisNamesAndFormats =
  unlines (concatMap (\an -> map ((an++" ")++) serverFormats)
                     registeredAnalysisNames)

