------------------------------------------------------------------------
--- Implementation of a worker client to analyze a module
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version March 2013
------------------------------------------------------------------------

module AnalysisWorker(main) where

import IO(Handle,hClose,hFlush,hWaitForInput,hPutStrLn,hGetLine)
import ReadShowTerm(readQTerm)
import Socket(connectToSocket)
import System(getArgs,setEnviron)

import AnalysisCollection(lookupRegAnaWorker)
import ServerFunctions(WorkerMessage(..))
import Configuration(debugMessage,waitTime,getDefaultPath)

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
   then error "Analysis worker program started with illegal arguments"
   else do
     let port = readQTerm (args!!1)
     debugMessage 2 ("start analysis worker on port " ++ show port)
     getDefaultPath >>= setEnviron "CURRYPATH" 
     handle <- connectToSocket (head args) port
     worker handle

-- communication loop
worker :: Handle -> IO ()
worker handle = do
  gotInput <- hWaitForInput handle waitTime
  if gotInput
    then do
       input <- hGetLine handle
       debugMessage 3 ("input: "++input)
       case readQTerm input of
         Task ananame moduleName -> do
           debugMessage 1 ("Start task: "++ananame++" for "++moduleName)
           -- Run the analysis worker for the given analysis and module:
           (lookupRegAnaWorker ananame) [moduleName]
           debugMessage 1 ("Finished task: "++ananame++" for "++moduleName)
           debugMessage 3 ("Output: "++input)
           hPutStrLn handle input
           hFlush handle
           worker handle
         ChangePath path -> do
           setEnviron "CURRYPATH" path
           worker handle
         StopWorker -> do
           debugMessage 2 "Stop worker"
           hClose handle
           done
    else done
