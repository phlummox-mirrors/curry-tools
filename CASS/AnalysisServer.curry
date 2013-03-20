--------------------------------------------------------------------------
--- This is the main module of the analysis system.
--- One can either use the 'main' operation to start the system
--- in "server mode" or "batch mode" or use one of the operations below
--- to use the analysis system in another Curry program.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version March 2013
--------------------------------------------------------------------------

module AnalysisServer(main,analyzeModuleForBrowser,analyzeGeneric,
                      analyzeInterface) where

import FlatCurry(QName)
import Socket(Socket(..),listenOnFresh,sClose,waitForSocketAccept)
import IO
import ReadShowTerm(readQTerm,showQTerm)
import System(system,sleep,setEnviron,getArgs)
import AnalysisCollection
import ServerFormats
import ServerFunctions(WorkerMessage(..))
import Configuration
import GenericProgInfo(ProgInfo,publicListFromProgInfo)
import Analysis(Analysis)

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
  initializeSystem
  args <- getArgs
  if null args then mainServer else case args of
    ["-h"]     -> showHelp
    ["-?"]     -> showHelp
    ["--help"] -> showHelp
    [ananame,modname] ->
      analyzeModuleWithOutputFormat ananame modname "Text" True >>= putStrLn
    _ -> error "Illegal arguments (use '--help' for description)"

--- Initializations to be done when the system is started.
initializeSystem = updateRCFile

showHelp = putStrLn $
  "Usage:\n"++
  "cass : start analysis system in server mode\n\n"++
  "cass <analysis name> <module name> :\n"++
  "analyze a module with a given analysis\n\n"++
  "Registered analyses names:\n" ++
  unlines registeredAnalysisNames

--- Start server on a socket.
mainServer = do
  putStrLn "Start Server"
  (port1,socket1) <- listenOnFresh
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
    outerLoop socket1 handles
    sClose workersocket
   else
    outerLoop socket1 []


--- Start the analysis system to show the results in the BrowserGUI.
analyzeModuleForBrowser :: String -> String -> IO [(QName,String)]
analyzeModuleForBrowser ananame moduleName = do
  initializeSystem
  result <- analyzeModuleWithOutputFormat ananame moduleName "CurryTerm" False
  return (readQTerm result)

-- Analyze a complete module where an output format must be specified.
analyzeModuleWithOutputFormat :: String -> String -> String -> Bool -> IO String
analyzeModuleWithOutputFormat ananame moduleName outFormat public = do
  numworkers <- numberOfWorkers
  if numworkers>0
   then do
    serveraddress <- getServerAddress
    (port,socket) <- listenOnFresh
    handles <- startWorkers numworkers socket serveraddress port []
    result <- runAnalysisWithWorkers ananame handles moduleName
    stopWorkers handles
    sClose socket
    return (formatResult moduleName outFormat Nothing public result)
   else do
    result <- runAnalysisWithWorkers ananame [] moduleName
    return (formatResult moduleName outFormat Nothing public result)
   
--- Start the analysis system with a particular analysis.
--- The analysis must be a registered one if workers are used.
--- If it is a combined analysis, the base analysis must be also
--- a registered one.
analyzeGeneric :: Analysis a -> String -> IO (Either (ProgInfo a) String)
analyzeGeneric analysis moduleName = do
  initializeSystem
  numworkers <- numberOfWorkers
  if numworkers>0
   then do
    serveraddress <- getServerAddress
    (port,socket) <- listenOnFresh
    handles <- startWorkers numworkers socket serveraddress port []
    result <- analyzeMain analysis moduleName handles True
    stopWorkers handles
    sClose socket
    return result
   else
    analyzeMain analysis moduleName [] True

--- Start the analysis system with a given analysis to compute properties
--- of a module interface.
--- The analysis must be a registered one if workers are used.
--- If it is a combined analysis, the base analysis must be also
--- a registered one.
analyzeInterface :: Analysis a -> String -> IO (Either [(QName,a)] String)
analyzeInterface analysis moduleName = do
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
outerLoop socket1 whandles = do
  --debugMessageLevel 3 "SERVER: outerLoop"
  connection <- waitForSocketAccept socket1 waitTime
  case connection of 
    Just (_,handle) -> do
      readable <- hWaitForInput handle waitTime
      if readable
        then do 
          string <- hGetLine handle
          debugMessageLevel 2 ("SERVER got message: "++string)
          case parseServerMessage string of
             ParseError -> do
               sendServerError handle ("Illegal message received: "++string)
               outerLoop socket1 whandles
             GetAnalysis -> do
               sendServerResult handle showAnalysisNamesAndFormats
               outerLoop socket1 whandles
             AnalyzeModule ananame outForm modname public ->
               catch (runAnalysisWithWorkers ananame whandles modname >>=
                      return . formatResult modname outForm Nothing public >>=
                      sendResult handle)
                     (sendAnalysisError handle)
             AnalyzeEntity ananame outForm modname functionName ->
               catch (runAnalysisWithWorkers ananame whandles modname >>=
                      return . formatResult modname outForm
                                            (Just functionName) False >>=
                      sendResult handle)
                     (sendAnalysisError handle)
             SetCurryPath path -> do
               setEnviron "CURRYPATH" path
               changeWorkerPath path whandles
               sendServerResult handle ""
               outerLoop socket1 whandles
             StopServer -> do
               stopWorkers whandles
               sendServerResult handle ""
               sClose socket1
               putStrLn "Stop Server"
               removeServerPortNumber
        else do
          putStrLn "input error"
          outerLoop socket1 whandles
    _ -> do
      putStrLn "outerLoop: connection error: time out in waitForSocketAccept"
      sleep 1
      outerLoop socket1 whandles
 where
   sendResult handle resultstring = do
     debugMessageLevel 4 ("formatted result:\n"++resultstring)
     sendServerResult handle resultstring
     outerLoop socket1 whandles

   sendAnalysisError handle err = do
     sendServerError handle ("ERROR in analysis server: "++showError err)
     outerLoop socket1 whandles

-- Send a server result in the format "ok <n>\n<result text>" where <n>
-- is the number of lines of the <result text>. Close handle afterwards.
sendServerResult handle resultstring = do
  let resultlines = lines resultstring
  hPutStrLn handle ("ok " ++ show (length resultlines))
  hPutStr handle (unlines resultlines)
  hFlush handle
  hClose handle

-- Send a server error in the format "error <error message>\n"
-- close handle afterwards.
sendServerError handle errstring = do
  debugMessageLevel 1 errstring
  hPutStrLn handle ("error "++errstring)
  hFlush handle
  hClose handle

-- worker threads are given changed library-search-path
changeWorkerPath _ [] = return()
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
                     (map (\ (an,_,_) -> an) analysisInfos))

