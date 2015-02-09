------------------------------------------------------------------------------
--- The main module of currypp, the Curry Preprocessor
--- ===================================================
---
--- The Curry Preprocessor transforms the source code of Curry programs.
--- Currently, only the translation of foreign code integrated in Curry code
--- is supported (option --foreigncode, see module `Translator`).
---
--- @author Michael Hanus
--- @version February 2015
------------------------------------------------------------------------------

import System

import Translator(translateFile)

cppTitle :: String
cppTitle = "Curry Preprocessor (version of 09/02/2015)"

--- The main function should be called with three arguments and uses
--- translateFile to translate Curry with Integrated Code to standard Curry.
main :: IO ()
main = do
  args <- getArgs
  case args of
    (orgSourceFile:inFile:outFile:options) ->
       let (save,version,optError,ppts) = processOptions (False,False,False,[]) options
        in if optError
           then showUsage args
           else do
             when version (putStrLn cppTitle)
             preprocess ppts inFile outFile
             when save (saveFile orgSourceFile outFile)
    _ -> showUsage args
 where
  processOptions (save,version,opterror,ppts) opts = case opts of
     []                -> (save,version,opterror,ppts)
     ("-o":os)         -> processOptions (True,version,opterror,ppts) os
     ("-v":os)         -> processOptions (save,True,opterror,ppts) os
     (('-':'-':ts):os) -> if ts `elem` ["foreigncode"]
                          then processOptions (save,version,opterror,ts:ppts) os
			  else (False,False,True,[])
     _                 -> (False,False,True,[])

  saveFile orgSourceFile outFile = do
    let sFile = orgSourceFile++".CURRYPP"
    system ("cp "++outFile++" "++sFile)
    putStrLn $ "Translated Curry file written to '"++sFile++"'"

  showUsage args = do
    putStrLn cppTitle
    putStrLn $ "\nERROR: Illegal arguments: " ++ unwords args ++ "\n"
    putStrLn usageText
    exitWith 1

usageText :: String
usageText = 
  "Usage: currypp <OrgFileName> <InputFilePath> <OutputFilePath> <options>\n\n"++
  "<OrgFileName>   : name of original program source file\n" ++
  "<InputFilePath> : name of the actual input file\n" ++
  "<OutputFilePath>: name of the file where output should be written\n\n" ++
  "where <options> can contain:\n" ++
  "--foreigncode : translate foreign code pieces in the source file\n" ++
  "-o            : store output also in file <OrgFileName>.CURRYPP\n" ++
  "-v            : show preprocessor version on stdout\n"

-- Start the Curry preprocessor:
preprocess :: [String] -> String -> String -> IO ()
preprocess pptargets infile outfile
  | null pptargets = do
    putStrLn cppTitle
    putStrLn "ERROR: no preprocessing target (e.g., --foreigncode) specified!\n"
    putStrLn usageText
    exitWith 1
  | "foreigncode" `elem` pptargets = translateFile infile outfile
  | otherwise = error "currypp: internal error"
