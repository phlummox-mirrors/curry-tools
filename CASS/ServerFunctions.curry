-- analysis computations on the server side

module ServerFunctions where

import FlatCurry(QName,readFlatCurryInt)
import FlatCurryGoodies(progImports)
import Socket(Socket(..),listenOnFresh,sClose,waitForSocketAccept)
import IO(Handle(..),hClose,hFlush,hGetLine,hPutStrLn,hWaitForInput,hWaitForInputs)
import ReadShowTerm(readQTerm,showQTerm)
import System(system,sleep)
import Directory(doesFileExist,getModificationTime)
import Distribution(findFileInLoadPath)
import Maybe(fromMaybe)
import List(delete)
import Time(ClockTime)

import Analysis
import GenericProgInfo
import AnalysisDependencies
import XML(showXmlDoc,xml)
import Configuration(debugMessageLevel,waitTime)

data WorkerMessage = Task String String | ChangePath String | StopWorker

debugMessage dl message = debugMessageLevel dl ("ServerFunctions: "++message)


-- loop for communication with workers
-- Argument 1: handles for workers that are currently free
-- Argument 2: handles for workers that are currently busy
-- Argument 3: the analysis name
-- Argument 4: the name of the main module
-- Argument 5: the modules to be analyzed (with their dependencies)
-- Argument 6: names of modules that are ready be to analyzed (since their
--             imports are already analyzed)
-- Result: Nothing (in case of successful work) or (Just <error>)
workerLoop :: [Handle] -> [Handle] -> String -> String
           -> [(String,[String])] -> [String] -> IO (Maybe String)
workerLoop _ [] _ _ [] [] = do
  debugMessage 2 "workerLoop terminated"
  return Nothing

workerLoop _ (b:busyWorker) ananame mainModule [] [] = do
  debugMessage 2 "workerLoop waiting for worker result"
  inputHandle <- hWaitForInputs (b:busyWorker) waitTime 
  if inputHandle/=0
    then return (Just "No input from any worker received")
    else do
      let handle =  b
      input <- hGetLine handle
      debugMessage 2 ("got message: "++input)
      let Task ananame2 moduleName2 = readQTerm input
      if ananame==ananame2 && moduleName2==mainModule
        then return Nothing
        else return (Just "Received analysis does not match requested analysis")

workerLoop idleWorker busyWorker ananame mainModule
           modulesToDo@(_:_) [] = do
  debugMessage 3 ("modulesToDo: "++(showQTerm modulesToDo))
  let modulesToDo2 = filter ((not . null) . snd) modulesToDo
      waitList     = map fst (filter (null . snd) modulesToDo)
  if null waitList
    then do
      debugMessage 2 "WorkerLoop: waiting for workers to finish"
      inputHandle <- hWaitForInputs busyWorker waitTime 
      if inputHandle<0
        then return (Just "No input from any worker received")
        else do
          let handle =  busyWorker !! inputHandle
          input <- hGetLine handle
          debugMessage 2 ("got message: "++input)
          let Task ananame2 moduleName2 = readQTerm input
          if ananame==ananame2
            then do
              let modulesToDo3 = reduceDependencies modulesToDo2 [moduleName2]
                  busyWorker2= deleteIndex inputHandle busyWorker 
              workerLoop (handle:idleWorker) busyWorker2 ananame
                         mainModule modulesToDo3 waitList  
            else
             return
              (Just "Received analysis does not match requested analysis type")
    else workerLoop idleWorker busyWorker ananame mainModule modulesToDo2
                    waitList

workerLoop (handle:idleWorker) busyWorker ananame mainModule modulesToDo
           (modName:waitList) = do
  debugMessage 2 "Worker available, send task to a worker..."
  let newTask = showQTerm (Task ananame modName)
  hPutStrLn handle newTask
  hFlush handle
  debugMessage 2 ("send message: "++newTask)
  workerLoop idleWorker (handle:busyWorker) ananame mainModule
             modulesToDo waitList

workerLoop [] busyWorker ananame mainModule modulesToDo
           waits@(modName:waitList) = do
  debugMessage 2 $ "Waiting for analysis results for: "++show waits
  inputHandle <- hWaitForInputs busyWorker waitTime 
  if inputHandle<0
    then return (Just "No input from any worker received")
    else do
      let handle = busyWorker !! inputHandle
      input <- hGetLine handle
      debugMessage 2 ("got message: "++input)
      let Task _ finishedmodule = readQTerm input
          newTask = showQTerm (Task ananame modName)
      hPutStrLn handle newTask
      hFlush handle
      debugMessage 2 ("send message: "++newTask)
      let modulesToDo2 = reduceDependencies modulesToDo [finishedmodule]
      workerLoop [] busyWorker ananame mainModule modulesToDo2 waitList

deleteIndex :: Int -> [a] -> [a]
deleteIndex _ [] = []
deleteIndex n (x:xs) | n==0      = xs
                     | otherwise = x : deleteIndex (n-1) xs

-----------------------------------------------------------------------
