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

import Char(isDigit,digitToInt)
import Distribution(installDir)
import System

import Translator(translateFile)

cppBanner :: String
cppBanner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText = "Curry Preprocessor (version of 21/08/2015)"
   bannerLine = take (length bannerText) (repeat '=')

--- Preprocessor options:
data PPOpts = PPOpts { optHelp :: Bool
                     , optSave :: Bool
		     , optVerb :: Int
		     , optTgts :: [String]
		     }

initOpts :: PPOpts
initOpts = PPOpts { optHelp = False
                  , optSave = False
 	          , optVerb = 0
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
	       then putStrLn (cppBanner ++ usageText)
	       else do
                when (optVerb opts > 0) $ putStr cppBanner
                preprocess opts orgSourceFile inFile outFile
                when (optSave opts) $ saveFile orgSourceFile outFile )
	     (processOptions initOpts options)
    _ -> maybe (showUsage args)
	       (\opts -> if optHelp opts
	                 then putStrLn (cppBanner ++ usageText)
	                 else showUsage args)
               (processOptions initOpts args)
 where
  processOptions opts optargs = case optargs of
    []                -> Just opts
    ("-h":_)          -> Just opts { optHelp = True}
    ("-?":_)          -> Just opts { optHelp = True}
    ("-o":os)         -> processOptions opts { optSave = True } os
    ("-v":os)         -> processOptions opts { optVerb = 1 } os
    (['-','v',vl]:os) -> if isDigit vl
                         then processOptions opts { optVerb = digitToInt vl } os
			 else Nothing
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
    putStr cppBanner
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
preprocess :: PPOpts -> String -> String -> String -> IO ()
preprocess opts orgSourceFile infile outfile
  | null pptargets = do
    putStr cppBanner
    putStrLn "ERROR: no preprocessing target (e.g., --foreigncode) specified!\n"
    putStrLn usageText
    exitWith 1
  | "foreigncode"  `elem` pptargets = translateFile infile outfile
  | "seqrules"     `elem` pptargets =
      system (unwords [installDir++"/currytools/currypp/SequentialRules/Main",
                       orgSourceFile,infile,outfile]) >> done
  | "defaultrules" `elem` pptargets =
      system
       (unwords $ [installDir++"/currytools/currypp/DefaultRules/Transform",
                   orgSourceFile,infile,outfile] ++
		  (if verb==0 then [] else ["-v"++show verb])) >> done
  | otherwise = error "currypp: internal error"
 where
  pptargets = optTgts opts
  verb      = optVerb opts
  
