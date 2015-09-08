------------------------------------------------------------------------------
--- This is the main module of the sequential rules preprocessor.
--- The preprocessor transforms a Curry program such that the
--- transformed program implements a sequential rule ordering.
--- 
--- The ideas of this transformation are described in the
--- [WFLP 2014 paper](http://ceur-ws.org/Vol-1335/wflp2014_paper5.pdf).
--- 
--- The preprocessor can be called with the following parameters:
--- 
--- ... <input> : translate <input> program and show translated program
--- 
--- ... <input> <output> : translate <input> program into <output> program
--- 
--- ... <org> <infile> <outfile> : "preprocessor mode", i.e., translate
---   <infile> to <outfile> where <org> is the original program
--- 
--- @author Lasse Folger (with changes by Michael Hanus)
--- @version September 2015
------------------------------------------------------------------------------

import AbstractCurry.Types
import AbstractCurry.Files
import Directory
import Distribution(stripCurrySuffix)
import List(isPrefixOf)
import AbstractCurry.Pretty(showCProg)
import System

import Sequential

main :: IO ()
main = do
  args <- getArgs
  case args of
    [srcmod]        -> applySequential (stripCurrySuffix srcmod) Nothing
    [srcmod,trgmod] -> applySequential (stripCurrySuffix srcmod)
                                       (Just (stripCurrySuffix trgmod))
    [orgfile,infile,outfile] -> ppSequential orgfile infile outfile
    _ -> do putStrLn $ "Illegal arguments: "++unwords args
            putStrLn "Usage: ... <sourcemodule> <targetmodule>"

applySequential :: String -> Maybe String -> IO ()
applySequential sourcemodname mbtarget = do
  inputProg <- readCurry sourcemodname
  let targetmodname = maybe sourcemodname id mbtarget
      targetprog = showCProg (translate inputProg targetmodname)
  (maybe putStr (\fn -> writeFile (fn++".curry")) mbtarget) targetprog

-- Start sequentializer in "preprocessor mode":
ppSequential :: String -> String -> String -> IO ()
ppSequential orgfile infile outfile = do
  --print (orgfile,infile,outfile)
  let savefile = orgfile++".SAVE"
      modname = stripCurrySuffix orgfile
  renameFile orgfile savefile
  readFile infile >>= writeFile orgfile . replaceOptionsLine
  inputProg <- tryReadCurry modname savefile
  --putStr (showCProg (translate inputProg modname))
  renameFile savefile orgfile
  writeFile outfile (showCProg (translate inputProg modname))
 where
   tryReadCurry mn savefile =
     catch (readCurry mn)
           (\_ -> renameFile savefile orgfile >> exitWith 1)

-- Replace OPTIONS_CYMAKE line in a source text by blank line:
replaceOptionsLine :: String -> String
replaceOptionsLine = unlines . map replOptLine . lines
 where
  replOptLine s = if "{-# OPTIONS_CYMAKE " `isPrefixOf` s -- -}
                  then " "
                  else s
