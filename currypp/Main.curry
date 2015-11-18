------------------------------------------------------------------------------
--- The main module of currypp, the Curry Preprocessor
--- ===================================================
---
--- The Curry Preprocessor transforms the source code of Curry programs.
--- Currently, only the translation of foreign code integrated in Curry code
--- is supported (option --foreigncode, see module `Translator`).
---
--- @author Michael Hanus
--- @version November 2015
------------------------------------------------------------------------------

import Char(isDigit,digitToInt)
import Directory(copyFile,renameFile)
import Distribution(installDir)
import List(delete)
import System

import TransICode(translateICFile)
import TransDefRules(transDefaultRules)
import TransSeqRules(transSequentialRules)

cppBanner :: String
cppBanner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText = "Curry Preprocessor (version of 13/11/2015)"
   bannerLine = take (length bannerText) (repeat '=')

--- Preprocessor targets:
data PPTarget = ForeignCode | SequentialRules | DefaultRules

parseTarget :: String -> Maybe PPTarget
parseTarget t | t=="foreigncode"  = Just ForeignCode
              | t=="defaultrules" = Just DefaultRules
	      | t=="seqrules"     = Just SequentialRules
	      | otherwise         = Nothing
	      
--- Preprocessor options:
data PPOpts = PPOpts { optHelp :: Bool
                     , optSave :: Bool
		     , optVerb :: Int
		     , optTgts :: [PPTarget]
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
	       then putStrLn (cppBanner ++ usageText) >> exitWith 1
	       else do
                when (optVerb opts > 0) $ putStr cppBanner
                when (optVerb opts > 2) $ do
		  putStrLn ("Original file name: " ++ orgSourceFile)
		  putStrLn ("Input    file name: " ++ inFile)
		  putStrLn ("Output   file name: " ++ outFile)
                preprocess opts orgSourceFile inFile outFile
                when (optSave opts) $ saveFile orgSourceFile outFile
		when (optVerb opts > 3) (readFile outFile >>= putStrLn) )
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
    (('-':'-':ts):os) -> maybe Nothing
                               (\t -> processOptions
			                opts {optTgts = t : optTgts opts} os)
    	 	               (parseTarget ts)
    (ts:os)           -> maybe Nothing
                               (\t -> processOptions
			                opts {optTgts = t : optTgts opts} os)
    	 	               (parseTarget ts)

  saveFile orgSourceFile outFile = do
    let sFile = orgSourceFile++".CURRYPP"
    copyFile outFile sFile
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
  "-v<n>        : show more information about the preprocessor:\n" ++
  "               <n>=1 : show version\n" ++
  "               <n>=2 : show timing\n" ++
  "               <n>=3 : show used file names\n" ++
  "               <n>=4 : show transformed Curry program\n" ++
  "-h|-?        : show help message and quit\n"

-- Start the Curry preprocessor:
preprocess :: PPOpts -> String -> String -> String -> IO ()
preprocess opts orgfile infile outfile
  | null pptargets = do
    putStr cppBanner
    putStrLn "ERROR: no preprocessing target (e.g., --foreigncode) specified!\n"
    putStrLn usageText
    exitWith 1
  | SequentialRules `elem` pptargets && DefaultRules `elem` pptargets
   = do putStr cppBanner
        putStrLn "ERROR: cannot use 'defaultrules' together with 'seqrules'!\n"
        exitWith 1
  | ForeignCode `elem` pptargets
   = do starttime <- getCPUTime
        translateICFile orgfile infile outfile
        stoptime <- getCPUTime
        when (verb>1) $ putStrLn
          ("Foreign code transformation time: " ++
	   show (stoptime-starttime) ++ " ms")
        let rpptargets = delete ForeignCode pptargets
        unless (null rpptargets) $ do
	  let savefile = orgfile ++ ".SAVEORGPP"
	  renameFile orgfile savefile
	  copyFile outfile orgfile
	  copyFile orgfile infile
	  preprocess opts{optTgts = rpptargets} orgfile infile outfile
	  renameFile savefile orgfile
  | SequentialRules `elem` pptargets
   = transSequentialRules verb orgfile infile outfile
  | DefaultRules    `elem` pptargets
   = transDefaultRules verb orgfile infile outfile
  | otherwise = error "currypp: internal error"
 where
  pptargets = optTgts opts
  verb      = optVerb opts
  
