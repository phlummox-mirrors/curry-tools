------------------------------------------------------------------------------
--- The main module of currypp, the Curry Preprocessor
--- ===================================================
---
--- The Curry Preprocessor transforms the source code of Curry programs.
--- Currently, only the translation of foreign code integrated in Curry code
--- is supported (option `foreigncode`, see module `Translator`).
---
--- @author Michael Hanus
--- @version June 2016
------------------------------------------------------------------------------

import AbstractCurry.Types
import AbstractCurry.Files
import AbstractCurry.Pretty(showCProg)
import AbstractCurry.Select(progName)
import Char(isDigit,digitToInt)
import Directory(copyFile,renameFile)
import Distribution
import FilePath (splitDirectories)
import List
import System

import TransICode(translateIntCode)
import TransDefRules(transDefaultRules)
import Sequential(transSequentialRules)
import TransContracts(transContracts)

cppBanner :: String
cppBanner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText = "Curry Preprocessor (version of 04/06/2016)"
   bannerLine = take (length bannerText) (repeat '=')

--- Preprocessor targets, i.e., kind of entities to be preprocessed:
data PPTarget = ForeignCode | SequentialRules | DefaultRules | Contracts

parseTarget :: String -> Maybe PPTarget
parseTarget t | t=="foreigncode"  = Just ForeignCode
              | t=="defaultrules" = Just DefaultRules
	      | t=="seqrules"     = Just SequentialRules
	      | t=="contracts"    = Just Contracts
	      | otherwise         = Nothing
	      
--- Preprocessor options:
data PPOpts =
  PPOpts { optHelp      :: Bool
         , optSave      :: Bool       -- save the transformed program?
         , optVerb      :: Int        -- verbosity 
         , optTgts      :: [PPTarget] -- targets of the preprocessor
         , optModel     :: String     -- model for the SQL preprocessor
         , optDefRules  :: [String]   -- options for DefaultRules
         , optContracts :: [String]   -- options for Contracts
	 }

initOpts :: PPOpts
initOpts = PPOpts { optHelp      = False
                  , optSave      = False
 	          , optVerb      = 1
		  , optTgts      = []
                  , optModel     = ""
		  , optDefRules  = []
		  , optContracts = []
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
                let modname = pathToModName (stripCurrySuffix orgSourceFile)
                when (optVerb opts > 1) $ putStr cppBanner
                when (optVerb opts > 2) $ putStr $ unlines
		  ["Module name        : " ++ modname
		  ,"Original file name : " ++ orgSourceFile
		  ,"Input    file name : " ++ inFile
		  ,"Output   file name : " ++ outFile ]
                preprocess opts modname orgSourceFile inFile outFile
                when (optSave opts) $ saveFile orgSourceFile outFile
		when (optVerb opts > 3) $ do
                  putStrLn "TRANSFORMED PROGRAM:"
                  putStrLn "===================="
                  readFile outFile >>= putStrLn
             )
	     (processOptions initOpts options)
    _ -> maybe (showUsage args)
	       (\opts -> if optHelp opts
	                 then putStrLn (cppBanner ++ usageText)
	                 else showUsage args)
               (processOptions initOpts args)
 where
  saveFile orgSourceFile outFile = do
    let sFile = orgSourceFile++".CURRYPP"
    copyFile outFile sFile
    putStrLn $ "Translated Curry file written to '"++sFile++"'"

processOptions :: PPOpts -> [String] -> Maybe PPOpts
processOptions opts optargs = case optargs of
    []                -> Just opts
    ("-h":_)          -> Just opts { optHelp = True}
    ("-?":_)          -> Just opts { optHelp = True}
    ("-o":os)         -> processOptions opts { optSave = True } os
    ("-v":os)         -> processOptions opts { optVerb = 2 } os
    (['-','v',vl]:os) -> if isDigit vl
                         then processOptions opts { optVerb = digitToInt vl } os
			 else Nothing
    (('-':'-':ts):os) -> if isPrefixOf "model:" ts
                         then processOptions 
                                opts {optModel = tail (dropWhile (/=':') ts) }
                                os
                         else Nothing
    (o:os)  -> if o `elem` ["-e","-t"]
               then processOptions
                      opts {optContracts = optContracts opts ++ [o]} os
               else
                if o `elem` ["nodupscheme","specscheme"]
                then processOptions
                       opts {optDefRules = optDefRules opts ++ [o]} os
                else
                  maybe Nothing
                        (\t -> processOptions
                                 opts {optTgts = t : optTgts opts} os)
                        (parseTarget o)

showUsage :: [String] -> IO ()
showUsage args = do
  putStr cppBanner
  putStrLn $ "\nERROR: Illegal arguments: " ++ unwords args ++ "\n"
  putStrLn usageText
  exitWith 1

usageText :: String
usageText = unlines $
 [ "Usage: currypp <OrgFileName> <InputFilePath> <OutputFilePath> <options>\n"
 , "<OrgFileName>   : name of original program source file"
 , "<InputFilePath> : name of the actual input file"
 , "<OutputFilePath>: name of the file where output should be written\n"
 , "where <options> contain preprocessing targets\n"
 , "foreigncode  : translate foreign code pieces in the source file"
 , "--model:<ERD_Name>_UniSQLCode.info :"
 , "               data model to translate embedded sql requests."
 , "seqrules     : implement sequential rule selection strategy"
 , "defaultrules : implement default rules"
 , "contracts    : implement dynamic contract checking"
 , ""
 , "and optional settings:"
 , "-o           : store output also in file <OrgFileName>.CURRYPP"
 , "-v           : same as -v2"
 , "-v<n>        : show more information about the preprocessor:"
 , "               <n>=0 : quiet"
 , "               <n>=1 : show some information (default)"
 , "               <n>=2 : show version and timing"
 , "               <n>=3 : show used file names"
 , "               <n>=4 : show transformed Curry program"
 , "-h|-?        : show help message and quit"
 , ""
 , "For 'defaultrules':"
 , "specscheme   : default translation scheme (as in PADL'16 paper)"
 , "nodupscheme  : translation scheme without checking conditions twice"
 , ""
 , "For 'contracts':"
 , "-e           : encapsulate nondeterminism of assertions"
 , "-t           : assert contracts only to top-level (not recursive) calls"
 ]

-- Start the Curry preprocessor:
preprocess :: PPOpts -> String -> String -> String -> String -> IO ()
preprocess opts modname orgfile infile outfile
  | null pptargets
  = -- no target specified: apply all reasonable transformations
    preprocess opts { optTgts = [ForeignCode, DefaultRules, Contracts] }
               modname orgfile infile outfile
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
       outtxt <- catch (callPreprocessors opts (optionLines srcprog)
                                          modname srcprog orgfile outfile)
                       (\err -> renameFile savefile orgfile >> ioError err)
       writeFile outfile outtxt
       renameFile savefile orgfile
       stoptime <- getCPUTime
       when (optVerb opts > 1) $ putStrLn
         ("Transformation time: " ++
         show (stoptime-starttime) ++ " ms")
 where
  pptargets = optTgts opts

-- Invoke the various preprocessors:
callPreprocessors :: PPOpts -> String -> String -> String -> String -> String
                  -> IO String
callPreprocessors opts optlines modname srcprog orgfile outfile
  | ForeignCode `elem` pptargets
  = do icouttxt <- translateIntCode verb (optModel opts) orgfile srcprog
       if null (intersect [SequentialRules, DefaultRules, Contracts] pptargets)
        then return icouttxt -- no further preprocessors
        else do writeFile orgfile icouttxt
                let rpptargets = delete ForeignCode pptargets
                callPreprocessors opts {optTgts = rpptargets}
                                  optlines modname srcprog orgfile outfile
  | SequentialRules `elem` pptargets
  = do seqprog <- readCurry modname >>=
                  transSequentialRules verb [] srcprog
       if Contracts `elem` pptargets
        then transContracts verb contopts srcprog seqprog >>= return . showCProg
        else return (showCProg seqprog)
  | DefaultRules `elem` pptargets
  = do -- specific handling since DefaultRules requires and process
       -- untyped Curry but Contracts requires typed Curry:
       defprog <- readUntypedCurry modname >>=
                  transDefaultRules verb defopts srcprog
       if Contracts `elem` pptargets
        then do writeFile orgfile (optlines ++ showCProg defprog)
                readCurry modname >>= transContracts verb contopts srcprog
                                  >>= return . showCProg
        else return (showCProg defprog)
  | Contracts `elem` pptargets
  = readCurry modname >>= transContracts verb contopts srcprog
                      >>= return . showCProg
  | otherwise
  = error "currypp internal error during dispatching"
 where
  pptargets = optTgts opts
  verb      = optVerb opts
  defopts   = optDefRules opts
  contopts  = optContracts opts

--- Transforms a file path name for a module back into a hierarchical module
--- since only the file path of a module is passed to the preprocessor.
--- We assume that these are always the local file path names,
--- otherwise it is difficult to reconstruct the original module name
--- from the file path.
pathToModName :: String -> String
pathToModName = intercalate "." . splitDirectories


-- Replace OPTIONS_CYMAKE line containing currypp call
-- in a source text by blank line (to avoid recursive calls):
replaceOptionsLine :: String -> String
replaceOptionsLine = unlines . map replOptLine . lines
 where
  replOptLine s = if isOptionLine s && "currypp" `isInfixOf` s
                  then " "
                  else s

-- Is this a OPTIONS_CYMAKE comment line?
isOptionLine :: String -> Bool
isOptionLine s = "{-# OPTIONS_CYMAKE " `isPrefixOf` s -- -}

-- Extract all OPTIONS_CYMAKE lines:
optionLines :: String -> String
optionLines = unlines . filter isOptionLine . lines

------------------------------------------------------------------------------
