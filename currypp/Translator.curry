------------------------------------------------------------------------------
--- Translator from Curry with Integrated Code to Curry
--- ===================================================
---
--- Integrated Code can be used in Curry in the form
---
---   AccentGraves Langtag Whitespaces Code SingleQuotes
---
--- where AccentGraves is a number of ` greater than 2
---       SingleQuotes is the same number of '
---       Langtag is an arbitrary sequence of characters without
---         whitespaces, tabs and newlines,
---       Whitespaces is a combination of spaces, tabs and newlines,
---       and Code is code in the language Langtag.
--- Is is allowed to use ` and ' in the code, as long as they amount of
--- sequential ` or ' is smaller than their number in AccentGraves.
---
--- If there is a corresponding parser to the langtag, the expression can be
--- translated into type-safe Curry code.
---
--- Currently available Langtags:
--- format - see the FormatParser and Format library
--- regex  - see the RegexParser and Regex library
--- html   - see the MLParser and HTML library
--- xml    - see the MLParser and XML library
---
--- @author Jasper Sikorra (with changes by Michael Hanus)
--- @version May 2014
------------------------------------------------------------------------------
module Translator where

import List
import System

import ParseTypes

import qualified CIParser

import DummyParser   as DummyParser
import FormatParser  as FormatParser
import RegexParser   as RegexParser
import MLTranslate   as MLTranslate

cppTitle :: String
cppTitle = "Curry Preprocessor (version of 07/11/2014)"

-- Parser for Curry with Integrated Code
ciparser :: Filename -> String -> IO (PM [StandardToken])
ciparser = CIParser.parse

-- Selection of parsers for the conversion of Integrated Code expressions
-- to Curry
parsers :: Maybe Langtag -> LangParser
parsers = maybe iden pars
  where
    iden _ s = return $ cleanPM s
    pars :: Langtag -> LangParser
    pars l p =
      case l of
        "dummy"     -> DummyParser.parse p
        "format"    -> FormatParser.parse ""       p
        "printf"    -> FormatParser.parse "putStr" p
        "regex"     -> RegexParser.parse p
        "html"      -> liftIO (mapWarnsPM (addRealFname (getFilename p))) .
                               MLTranslate.translate l p
        "xml"       -> liftIO (mapWarnsPM (addRealFname (getFilename p))) .
                               MLTranslate.translate l p
        _           -> (\_ -> return $ throwPM p ("Bad langtag: " ++ l))

addRealFname :: Filename -> Warning -> Warning
addRealFname f w = setWarnPos w (setFilename (getWarnPos w) f)

-- Formatting and terminating with Errors
errfun :: [PError] -> _
errfun (e1:es) = error $ "\nERRORS in " ++ getFilename (getPErrorPos e1) ++ ":"
                                     ++ foldr (++) "" (map formatErr (e1:es))
  where
    formatErr :: PError -> String
    formatErr e = "\n" ++ "Line " ++ show (getLn (getPErrorPos e))
                       ++ " Col " ++ show (getCol (getPErrorPos e))
                       ++ " | "   ++ getPErrorMsg e

-- Formatting Warnings
formatWarnings :: [Warning] -> String
formatWarnings []              = ""
formatWarnings ws@((p,_):_) = "\nWARNINGS in " ++ getFilename p ++ ":"
                                             ++ foldr (++) "" (map formatW ws)
                                             ++ "\n\n"
  where
    formatW :: Warning -> String
    formatW w = "\n" ++ "Line " ++ show (getLn (getWarnPos w))
                     ++ " Col " ++ show (getCol (getWarnPos w))
                     ++ " | "   ++ getWarnMsg w


--- The main function should be called with three arguments and uses
--- translateFile to translate Curry with Integrated Code to standard Curry.
main :: IO ()
main = do
  args <- getArgs
  case args of
    (orgSourceFile:inFile:outFile:options) ->
       let (save,version,optError) = processOptions (False,False,False) options
        in if optError
           then showUsage args
           else do
             when version (putStrLn cppTitle)
             translateFile inFile outFile
             when save (saveFile orgSourceFile outFile)
    _ -> showUsage args
 where
  processOptions (save,version,opterror) opts = case opts of
     []        -> (save,version,opterror)
     ("-o":os) -> processOptions (True,version,opterror) os
     ("-v":os) -> processOptions (save,True,opterror) os
     _         -> (False,False,True)

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
  "Usage: currypp <OrgFileName> <InputFilePath> <OutputFilePath> [-o] [-v]\n\n"++
  "<OrgFileName>   : name of original program source file\n" ++
  "<InputFilePath> : name of the actual input file\n" ++
  "<OutputFilePath>: name of the file where output should be written\n" ++
  "-o              : store output also in file <OrgFileName>.CURRYPP\n" ++
  "-v              : show preprocessor version on stdout\n"

--- Translates a file with Curry with Integrated Code to a file with Curry Code.
--- @param in_fp - The filepath to the input file
--- @param out_fp - The filepath to the output file
translateFile :: String -> String -> IO ()
translateFile in_fp out_fp =
  do in_st  <- readFile in_fp
     out_st <- translateString in_fp in_st
     writeFile out_fp out_st

--- Translates a string with Curry with Integrated Code to a string with Curry
--- Code.
--- @param s - The string that should be translated
--- @return The translated string
translateString :: String -> String -> IO String
translateString name s =
  do stw <- concatAllIOPM $ applyLangParsers $ ciparser name s
     putStr (formatWarnings (getWarnings stw))
     return $ escapePR (discardWarnings stw) errfun

--- Handles the IO and PM monads around the StandardTokens for the
--- concatenation, so they will not disturb in the real concat function
--- concatAll
--- @param ioprpt - A list of StandardTokens wrapped in IO and a ParserMonad
concatAllIOPM :: IO (PM [StandardToken]) -> IO (PM String)
concatAllIOPM ioprpt =
  do prpt <- ioprpt
     return $ liftPM (\pt -> concatAll pt) prpt

{-
Problems with insertion of newlines:
The case that a Curry expression directly follows integrated expression,
without a newline is problematic, if the integrated expression has multiple
lines. This stems from the Curry layout rule.  The problem is depicted in the
example:
                  -- Ln. 1: isEmail s = s ``regex
                  -- Ln. 2:  a'' && True
                  -- Ln. 3:
                  -- Ln. 4:  || False
                  -- Result:
                  -- Ln. 1: isEmail s = s `match` [(Literal 'a')] && True
                  -- Ln. 2:
                  -- Ln. 3:
                  -- Ln. 4:  || False
For this line, wrong positions will be calculate in the Curry compiler, if an
error occurs. In the example: Ln 1 instead of Ln 2. All other lines have
the right positions.
-}

--- Concatenates the result of the translation process, inserting newlines
--- and offsets if necessary
--- @param tks - A list of StandardTokens containing the results
--- @result    - The resulting program code
concatAll :: [StandardToken] -> String
concatAll []      = ""
concatAll (t1:tks) = getCode t1 ++ (concatAllHelper
                                   (getIdentPos t1)
                                   (containsDSL t1)
                                   tks)
  where
    concatAllHelper :: Pos -> Bool -> [StandardToken] -> String
    concatAllHelper _ _ []        = ""
    concatAllHelper op b (t:toks) =
      let s      = getCode t
          p      = getIdentPos t
         -- if generated dsl code was processed before
      in if b
        then
          let lnDiff = lnDifference op p
          in
            -- if the first word of s was in a newline after the dsl
            if (null s)
              then genLines lnDiff ++ concatAllHelper p (containsDSL t) toks
              else
                if (head s == '\n')
                      then (genLines lnDiff ++ s
                            ++ concatAllHelper p (containsDSL t) toks)
                  -- If the first word of s was in the last line of the dsl.
                      else
                        let (headLine,restOfCurry) = splitByLine s
                        in
                            headLine ++ genLines lnDiff ++ restOfCurry
                            ++ concatAllHelper p (containsDSL t) toks
        else (s ++ concatAllHelper p (containsDSL t) toks)

--- The function genLines generates lines
--- @param n - The number of line to be generated
--- @result  - A string containing n lines
genLines :: Int -> String
genLines = flip replicate '\n'

--- The function splitByLine splits a string at the first newline
--- @param s - The string
--- @result A pair of strings, one containg the string before the newline
---         with the newline, the other containing the string after the newline
splitByLine :: String -> (String,String)
splitByLine s = splitByLineIter "" s
  where
    splitByLineIter acc "" = (reverse acc,"")
    splitByLineIter acc (c:cs) | c == '\n' = (reverse ('\n':acc),cs)
                               | otherwise = splitByLineIter (c:acc) cs

--- Applies the corresponding translators of the DSL to Curry on the
--- StandardTokens
--- @param iotks - The input StandardTokens wrapped in IO and ParserMonad
--- @result      - The translated StandardTokens wrapped in IO and ParserMonad
applyLangParsers :: IO (PM [StandardToken])
                    -> IO (PM [StandardToken])
applyLangParsers iotks =
  do prtks <- iotks
     prpr <- swapIOPM
              (liftPM (\tks -> sequenceIO (map (applyLangParser) tks)) prtks)
     return (crumplePM (liftPM (\prpt -> sequencePM prpt) prpr))

--- Select the right translator and apply it to a single StandardToken
--- @param t - The input StandardToken
--- result   - The translated StandardToken wrapped in IO and ParserMonad
applyLangParser :: StandardToken -> IO (PM StandardToken)
applyLangParser (StTk p pexp l c) =
  do parsedStringNoIO <- (parsers l) pexp c
     return (bindPM parsedStringNoIO (\s -> cleanPM (StTk p pexp l s)))
