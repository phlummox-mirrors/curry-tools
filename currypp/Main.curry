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

import Distribution(installDir)
import System

import Translator(translateFile)

cppTitle :: String
cppTitle = "Curry Preprocessor (version of 01/06/2015)"

--- Preprocessor options:
data PPOpts = PPOpts { optHelp :: Bool
                     , optSave :: Bool
		     , optVers :: Bool
		     , optTgts :: [String]
		     }

initOpts :: PPOpts
initOpts = PPOpts { optHelp = False
                  , optSave = False
 	          , optVers = False
		  , optTgts = []
		  }
		  
--- The main function of the Curry Preprocessor.
main :: IO ()
main = do
  args <- getArgs
  case args of
    (orgSourceFile:inFile:outFile:options) ->
       maybe (showUsage args)
	     (\opts ->
	       if optHelp opts
	       then putStrLn (cppTitle ++ "\n" ++ usageText)
	       else do
                when (optVers opts) $ putStrLn cppTitle
                preprocess (optTgts opts) orgSourceFile inFile outFile
                when (optSave opts) $ saveFile orgSourceFile outFile )
	     (processOptions initOpts options)
    _ -> maybe (showUsage args)
	       (\opts -> if optHelp opts
	                 then putStrLn (cppTitle ++ "\n" ++ usageText)
	                 else showUsage args)
               (processOptions initOpts args)
 where
  processOptions opts optargs = case optargs of
    []                -> Just opts
    ("-h":_)          -> Just opts { optHelp = True}
    ("-?":_)          -> Just opts { optHelp = True}
    ("-o":os)         -> processOptions opts { optSave = True } os
    ("-v":os)         -> processOptions opts { optVers = True } os
    (('-':'-':ts):os) -> if ts `elem` ["foreigncode","seqrules","default"]
                         then processOptions opts {optTgts = ts:optTgts opts} os
    	 	         else Nothing
    (ts:os)           -> if ts `elem` ["foreigncode","seqrules","defaultrules"]
                         then processOptions opts {optTgts = ts:optTgts opts} os
    	 	         else Nothing

  saveFile orgSourceFile outFile = do
    let sFile = orgSourceFile++".CURRYPP"
    system $ "cp "++outFile++" "++sFile
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
  "where <options> contain preprocessing targets\n\n" ++
  "foreigncode  : translate foreign code pieces in the source file\n" ++
  "seqrules     : implement sequential rule selection strategy\n" ++
  "defaultrules : implement default rules\n" ++
  "\nand optional settings:\n" ++
  "-o           : store output also in file <OrgFileName>.CURRYPP\n" ++
  "-v           : show preprocessor version on stdout\n" ++
  "-h|-?        : show help message and quit\n"

-- Start the Curry preprocessor:
preprocess :: [String] -> String -> String -> String -> IO ()
preprocess pptargets orgSourceFile infile outfile
  | null pptargets = do
    putStrLn cppTitle
    putStrLn "ERROR: no preprocessing target (e.g., --foreigncode) specified!\n"
    putStrLn usageText
    exitWith 1
  | "foreigncode"  `elem` pptargets = translateFile infile outfile
  | "seqrules"     `elem` pptargets =
      system (unwords [installDir++"/currytools/currypp/SequentialRules/Main",
                       orgSourceFile,infile,outfile]) >> done
  | "defaultrules" `elem` pptargets =
      system (unwords [installDir++"/currytools/currypp/DefaultRules/Transform",
                       orgSourceFile,infile,outfile]) >> done
  | otherwise = error "currypp: internal error"
