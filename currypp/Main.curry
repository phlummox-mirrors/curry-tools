------------------------------------------------------------------------------
--- The main module of currypp, the Curry Preprocessor
--- ===================================================
---
--- The Curry Preprocessor transforms the source code of Curry programs.
--- Currently, only the translation of foreign code integrated in Curry code
--- is supported (option `foreigncode`, see module `Translator`).
---
--- @author Michael Hanus
--- @version May 2016
------------------------------------------------------------------------------

import AbstractCurry.Types
import AbstractCurry.Files(readCurry, readUntypedCurry)
import AbstractCurry.Pretty(showCProg)
import AbstractCurry.Select(progName)
import Char(isDigit,digitToInt)
import Directory(copyFile,renameFile)
import Distribution(installDir, stripCurrySuffix)
import List(delete, intersect, isPrefixOf, isInfixOf)
import System

import TransICode(translateIntCode)
import TransDefRules(transDefaultRules)
import Sequential(transSequentialRules)
import TransContracts(transContracts)

cppBanner :: String
cppBanner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText = "Curry Preprocessor (version of 03/05/2016)"
   bannerLine = take (length bannerText) (repeat '=')

--- Preprocessor targets:
data PPTarget = ForeignCode | SequentialRules | DefaultRules | Contracts

parseTarget :: String -> Maybe PPTarget
parseTarget t | t=="foreigncode"  = Just ForeignCode
              | t=="defaultrules" = Just DefaultRules
	      | t=="seqrules"     = Just SequentialRules
	      | t=="contracts"    = Just Contracts
	      | otherwise         = Nothing
	      
--- Preprocessor options:
data PPOpts = PPOpts { optHelp :: Bool
                     , optSave :: Bool       -- save the transformed program?
		     , optVerb :: Int        -- verbosity level
		     , optTgts :: [PPTarget] -- target of preprocessor
		     , optModel:: String     -- model for the SQL preprocessor
                     , optMore :: [String]   -- further specific options
		     }

initOpts :: PPOpts
initOpts = PPOpts { optHelp = False
                  , optSave = False
 	          , optVerb = 0
		  , optTgts = []
                  , optModel = ""
		  , optMore = []
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
    (('-':'-':ts):os) -> if isPrefixOf "model:" ts
                         then processOptions 
                                opts {optModel = tail (dropWhile (/=':') ts) }
                                os
                         else Nothing
    (ts:os)           -> maybe (processOptions
		                  opts {optMore = optMore opts ++ [ts]} os)
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
  "--model:<ERD_Name>_UniSQLCode.info : data model to translate embedded "++
                                        "sql requests.\n"++
  "seqrules     : implement sequential rule selection strategy\n" ++
  "defaultrules : implement default rules\n" ++
  "contracts    : implement dynamic contract checking\n" ++
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
    putStrLn "ERROR: no preprocessing target (e.g., 'foreigncode') specified!\n"
    putStrLn usageText
    exitWith 1
  | SequentialRules `elem` pptargets && DefaultRules `elem` pptargets
  = do putStr cppBanner
       putStrLn "ERROR: cannot use 'defaultrules' together with 'seqrules'!\n"
       exitWith 1
  | otherwise
  = do let savefile = orgfile++".SAVEPPORG"
       starttime <- getCPUTime
       renameFile orgfile savefile
       srcprog <- readFile infile >>= return . replaceOptionsLine
       -- remove currypp option to avoid recursive preprocessor calls:
       writeFile orgfile srcprog
       outtxt <- catch (callPreprocessors opts srcprog orgfile infile outfile)
                       (\_ -> renameFile savefile orgfile >> exitWith 1)
       writeFile outfile outtxt
       renameFile savefile orgfile
       stoptime <- getCPUTime
       when (optVerb opts > 1) $ putStrLn
         ("Transformation time: " ++
         show (stoptime-starttime) ++ " ms")
 where
  pptargets = optTgts opts

-- Invoke the variaous preprocessors:
callPreprocessors :: PPOpts -> String -> String -> String -> String -> IO String
callPreprocessors opts srcprog orgfile infile outfile
  | ForeignCode `elem` pptargets
  = do icouttxt <- translateIntCode (optModel opts) orgfile srcprog
       if null (intersect [SequentialRules, DefaultRules, Contracts] pptargets)
        then return icouttxt -- no further preprocessors
        else do writeFile orgfile icouttxt
                --writeFile infile  icouttxt -- just to be save
                let rpptargets = delete ForeignCode pptargets
                callPreprocessors opts {optTgts = rpptargets}
                                  srcprog orgfile infile outfile
  | SequentialRules `elem` pptargets
  = do seqprog <- readCurry modname >>=
                  transSequentialRules verb moreopts srcprog
       if Contracts `elem` pptargets
        then transContracts verb moreopts srcprog seqprog >>= return . showCProg
        else return (showCProg seqprog)
  | DefaultRules `elem` pptargets
  = do -- specific handling since DefaultRules requires and process
       -- untyped Curry but Contracts requires typed Curry:
       defprog <- readUntypedCurry modname >>=
                  transDefaultRules verb moreopts srcprog
       if Contracts `elem` pptargets
        then do writeFile orgfile (showCProg defprog)
                readCurry modname >>= transContracts verb moreopts srcprog
                                  >>= return . showCProg
        else return (showCProg defprog)
  | Contracts `elem` pptargets
  = readCurry modname >>= transContracts verb moreopts srcprog
                      >>= return . showCProg
  | otherwise
  = error "currypp internal error during dispatching"
 where
  pptargets = optTgts opts
  verb      = optVerb opts
  moreopts  = optMore opts
  modname   = stripCurrySuffix orgfile

-- Replace OPTIONS_CYMAKE line containing currypp call
-- in a source text by blank line (to avoid recursive calls):
replaceOptionsLine :: String -> String
replaceOptionsLine = unlines . map replOptLine . lines
 where
  replOptLine s = if "{-# OPTIONS_CYMAKE " `isPrefixOf` s -- -}
                     && "currypp" `isInfixOf` s
                  then " "
                  else s

------------------------------------------------------------------------------
