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
--- @version November 2015
------------------------------------------------------------------------------
module TransICode where

import IO(stderr,hPutStrLn)
import List
import System

import ParseTypes

import qualified CIParser

import DummyParser   as DummyParser
import FormatParser  as FormatParser
import RegexParser   as RegexParser
import MLTranslate   as MLTranslate

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
errfun :: [PError] -> IO _
errfun (e1:es) = do
  hPutStrLn stderr $ "\nERRORS in " ++ getFilename (getPErrorPos e1) ++ ":"
                                    ++ concatMap formatErr (e1:es)
  error "Failure during preprocessing of Curry source file!"
 where
  formatErr :: PError -> String
  formatErr e = "\n" ++ "Line " ++ show (getLn (getPErrorPos e))
                     ++ " Col " ++ show (getCol (getPErrorPos e))
                     ++ ": "   ++ getPErrorMsg e

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

--- Translates a file with Curry with Integrated Code to a file with Curry Code.
--- @param orgfile - The file path to the original Curry file
--- @param infile - The file path to the input file
--- @param outfile - The file path to the output file
translateICFile :: String -> String -> String -> IO ()
translateICFile orgfile infile outfile =
  readFile infile >>= translateICString orgfile >>= writeFile outfile

--- Translates a string containing a Curry program with Integrated Code
--- into a string with pure Curry code.
--- @param fname - The name of the original Curry file
--- @param s - The string that should be translated
--- @return The translated string
translateICString :: String -> String -> IO String
translateICString fname s =
  do stw <- concatAllIOPM $ applyLangParsers $ ciparser fname s
     putStr (formatWarnings (getWarnings stw))
     escapePR (discardWarnings stw) errfun

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
