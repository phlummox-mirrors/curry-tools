----------------------------------------------------------------------
--- Implementation of CurryDoc, a utility for the automatic
--- generation of HTML documentation from Curry programs.
---
--- @author Michael Hanus
--- @version February 2013
----------------------------------------------------------------------

-- * All comments to be put into the HTML documentation must be
--   prefixed by "--- " (also in literate programs!).
--
-- * The comment of a module must occur before the first "module" or
--   "import" line of this module.
--
-- * The comment of a function or datatype must occur before the
--   first definition of this function or datatype.
--
-- * The comments can contain at the end several special comments:
--   @cons id comment   --> a comment for a constructor of a datatype
--   @param id comment  --> comment for function parameter id
--                          (list all parameters in left-to-right order)
--   @return comment    --> comments for the return value of a function
--   @author comment    --> the author of a module (only in module comments)
--   @version comment   --> the version of a module (only in module comments)
--
-- * Current restriction: doesn't properly work for infix operator definitions
--   without a type definition (so it should be always included)

module CurryDoc where

import Directory
import Distribution
import FileGoodies
import FilePath ((</>), (<.>))
import FlatCurry
import List
import System
import Time

import AnalysisServer(analyzeInterface)
import Deterministic
import TotallyDefined
import Indeterministic
import SolutionCompleteness

import CurryDocParams
import CurryDocRead
import CurryDocHtml
import CurryDocTeX
import CurryDocCDoc
import CurryDocConfig

--------------------------------------------------------------------------
-- Global definitions:

greeting = "CurryDoc (" ++ currydocVersion ++ ") - the Curry Documentation Tool\n"

-- Directory where include files for generated documention (e.g., icons,
-- css, tex includes) are stored:
includeDir = installDir </> "include"

--------------------------------------------------------------------------
-- Check arguments and call main function:
main = do
  args <- getArgs
  processArgs defaultCurryDocParams args

processArgs params args = case args of
  -- no markdown
  ("--nomarkdown":margs) -> processArgs (setMarkDown False  params) margs
  -- documentation type
  ("--html"      :margs) -> processArgs (setDocType HtmlDoc params) margs
  ("--tex"       :margs) -> processArgs (setDocType TexDoc  params) margs
  ("--cdoc"      :margs) -> processArgs (setDocType CDoc    params) margs
  -- HTML without index
  ["--noindexhtml",docdir,modname] ->
      makeCompleteDoc (setIndex False (setDocType HtmlDoc params))
                      True docdir (stripSuffix modname)
  -- HTML index only
  ("--onlyindexhtml":docdir:modnames) ->
                      makeIndexPages docdir (map stripSuffix modnames)
  (('-':_):_) -> putStrLn usageMessage
  -- module
  [modname] ->
      makeCompleteDoc params (docType params == HtmlDoc)
                      ("DOC_" ++ stripSuffix modname) (stripSuffix modname)
  -- docdir + module
  [docdir,modname] ->
      makeCompleteDoc params (docType params == HtmlDoc) docdir
                      (stripSuffix modname)
  _ -> putStrLn usageMessage

usageMessage = unlines
 [ "ERROR: Illegal arguments for currydoc"
 , "Usage: currydoc [--nomarkdown] [--html|--tex|--cdoc] [<doc directory>] <module_name>"
 , "       currydoc [--nomarkdown] --noindexhtml <doc directory> <module_name>"
 , "       currydoc --onlyindexhtml <doc directory> <module_names>"
 ]



-- create directory if not existent:
createDir :: String -> IO ()
createDir dir = do
  exdir <- doesDirectoryExist dir
  if exdir then done else system ("mkdir " ++ dir) >> done

--------------------------------------------------------------------------
--- The main function of the CurryDoc utility.
--- @param withindex - True if the index pages should also be generated
--- @param recursive - True if the documentation for the imported modules
---                    should be also generated (if necessary)
--- @param docdir - the directory name containing all documentation files
--- @param modname - the name of the main module to be documented
makeCompleteDoc :: DocParams -> Bool -> String -> String -> IO ()
makeCompleteDoc docparams recursive docdir modname = do
  putStrLn greeting
  prepareDocDir (docType docparams) docdir
  -- parsing source program:
  callFrontend FCY modname
  -- when constructing CDOC the imported modules don't have to be read from the flatCurryFile
  (alltypes, allfuns, _) <- getProg $ docType docparams
  makeDocIfNecessary docparams recursive docdir modname
  if withIndex docparams
   then do genMainIndexPage     docdir [modname]
           genFunctionIndexPage docdir allfuns
           genConsIndexPage     docdir alltypes
   else done
  -- change access rights to readable for everybody:
  system ("chmod -R go+rX "++docdir)
  done
    where getProg HtmlDoc = readFlatCurryWithImports [modname]
          getProg TexDoc  = readFlatCurryWithImports [modname]
          getProg CDoc    = do
              Prog _ _ t f o <- readFlatCurry modname
              return (t, f, o)


--- Generate only the index pages for a list of (already compiled!) modules:
makeIndexPages :: String -> [String] -> IO ()
makeIndexPages docdir modnames = do
  putStrLn greeting
  prepareDocDir HtmlDoc docdir
  (alltypes,allfuns,_) <- readFlatCurryWithImports modnames
  genMainIndexPage     docdir modnames
  genFunctionIndexPage docdir allfuns
  genConsIndexPage     docdir alltypes
  -- change access rights to readable for everybody:
  system ("chmod -R go+rX "++docdir)
  done

-- create documentation directory (if necessary) with gifs and stylesheets:
prepareDocDir :: DocType -> String -> IO ()
prepareDocDir HtmlDoc docdir = do
  createDir docdir
  putStrLn ("Copying icons into documentation directory \""++docdir++"\"...")
  -- copying all icons:
  copyIncludeIfPresent docdir "currydocicons/*.gif"
  -- copy style sheet:
  copyIncludeIfPresent docdir "currydoc.css"
prepareDocDir TexDoc docdir = do
  createDir docdir
  putStrLn $ "Copy macros into documentation directory \""++docdir++"\"..."
  copyIncludeIfPresent docdir "currydoc.tex"
prepareDocDir CDoc docdir = do
  createDir docdir
  putStrLn ("Directory was created succesfully")

copyIncludeIfPresent docdir inclfile = do
  existIDir <- doesDirectoryExist includeDir
  if existIDir
   then system ("cp "++includeDir++"/"++inclfile++" "++docdir) >> done
   else done

-- read and generate all analysis infos:
readAnaInfo modname = do
  nondet   <- analyzeInterface ndAnalysis modname >>= stopIfError
  complete <- analyzeInterface patCompAnalysis modname >>= stopIfError
  indet    <- analyzeInterface indetAnalysis   modname >>= stopIfError
  solcomp  <- analyzeInterface solcompAnalysis modname >>= stopIfError
  return (AnaInfo (\qn -> nondet qn == NDet) complete indet solcomp)
 where
   stopIfError (Right err) = error ("Analysis error: "++err)
   stopIfError (Left results) =
     return (\qn -> maybe (error $ "No analysis result for function "++show qn)
                          id
                          (lookup qn results))

-- generate documentation for a single module:
makeDoc :: DocParams -> Bool -> String -> String -> String -> IO ()
makeDoc docparams recursive docdir modname progname = do
  putStrLn ("Reading comments from file \""++progname++".curry\"...")
  (modcmts,progcmts) <- readComments (progname++".curry")
  putStrLn ("Reading analysis information for module \""++modname++"\"...")
  anainfo <- readAnaInfo modname
  makeDocWithComments (docType docparams) docparams recursive docdir
                      anainfo progname modcmts progcmts

makeDocWithComments HtmlDoc docparams recursive docdir anainfo progname
                    modcmts progcmts = do
  writeOutfile docparams recursive docdir progname
               (generateHtmlDocs docparams anainfo progname modcmts progcmts)
  translateSource2ColoredHtml docdir progname
  writeOutfile (DocParams CDoc False False) False docdir progname
               (generateCDoc progname modcmts progcmts anainfo)


makeDocWithComments TexDoc docparams recursive docdir anainfo progname
                    modcmts progcmts = do
  writeOutfile docparams recursive docdir progname
               (generateTexDocs docparams anainfo progname modcmts progcmts)


makeDocWithComments CDoc docparams recursive docdir anainfo progname
                    modcmts progcmts = do
  writeOutfile docparams recursive docdir progname
               (generateCDoc progname modcmts progcmts anainfo)


--- Generates the documentation for a module if it is necessary.
--- I.e., the documentation is generated if no previous documentation
--- file exists or if the existing documentation file is older than
--- the FlatCurry file.
makeDocIfNecessary :: DocParams -> Bool -> String -> String -> IO ()
makeDocIfNecessary docparams recursive docdir modname = do
  progname <- findSourceFileInLoadPath modname
  let docfile = docdir </> getLastName progname ++
                (if docType docparams == HtmlDoc then ".html" else ".tex")
  docexists <- doesFileExist docfile
  if not docexists
   then copyOrMakeDoc docparams recursive docdir modname progname 
   else do
     ctime  <- getModificationTime (flatCurryFileName progname)
     dftime <- getModificationTime docfile
     if compareClockTime ctime dftime == GT
      then copyOrMakeDoc docparams recursive docdir modname progname
      else if recursive
           then do imports <- getImports progname
                   mapIO_ (makeDocIfNecessary docparams recursive docdir)
                          imports
           else done

-- get imports of a program by reading the interface, if possible:
getImports progname = do
  let fintname = flatCurryIntName progname
  fintexists <- doesFileExist fintname
  (Prog _ imports _ _ _) <- if fintexists
                            then readFlatCurryFile fintname
                            else readFlatCurryFile (flatCurryFileName progname)
  return imports

copyOrMakeDoc :: DocParams -> Bool -> String -> String -> String -> IO ()
copyOrMakeDoc docparams recursive docdir modname progname = do
  hasCopied <- copyDocIfPossible docparams docdir progname
  if hasCopied then done
               else makeDoc docparams recursive docdir modname progname

--- Copy the documentation file from standard documentation directoy "CDOC"
--- (used for documentation of system libraries) if possible.
--- Returns true if the copy was possible.
copyDocIfPossible :: DocParams -> String -> String -> IO Bool
copyDocIfPossible docparams docdir progname =
  if docType docparams == TexDoc
  then return False -- ignore copying for TeX docs
  else do
    let docprogname = getDirName progname </> "CDOC" </> getLastName progname
        docHtmlFile = docprogname <.> "html"
    docexists <- doesFileExist docHtmlFile
    if not docexists
      then return False
      else do
        ctime <- getModificationTime (flatCurryFileName progname)
        htime <- getModificationTime docHtmlFile
        if compareClockTime ctime htime == GT
          then return False
          else do
            putStrLn ("Copying doc file from " ++ docHtmlFile)
            system ("cp " ++ docHtmlFile ++ ' ':docdir)
            system ("cp " ++ docprogname ++ "_curry.html "++docdir)
            return True

-----------------------------------------------------------------------
-- auxiliaries:

-- extract directory name from a path name:
getDirName n =
  let revdirname = dropWhile (/='/') (reverse n)
   in if revdirname=="" then "."
                        else reverse (tail revdirname)

-- read a list of FlatCurry modules together with all their imported modules
-- and return the lists of type, function, and operator declarations:
readFlatCurryWithImports :: [String] -> IO ([TypeDecl],[FuncDecl],[OpDecl])
readFlatCurryWithImports modules = collectMods modules []
 where
  collectMods []     _       = return ([],[],[])
  collectMods (m:ms) implist
    | m `elem` implist = collectMods ms implist
    | otherwise        = do
      filename <- findFileInLoadPath (m <.> "fcy")
      (Prog _ imps types funs ops) <- readFlatCurryFile filename
      (ts,fs,os) <- collectMods (ms++imps) (m:implist)
      return (types++ts, funs++fs, ops++os)

-- add a directory name for a Curry source file by looking up the
-- current load path (CURRYPATH):
findSourceFileInLoadPath modname = do
  loadpath <- getLoadPathForFile modname
  mbfname <- lookupFileInPath (baseName modname) [".lcurry",".curry"] loadpath
  maybe (error ("Curry file for module \""++modname++"\" not found!"))
        (return . stripSuffix)
        mbfname

-- get the associated file extenstion from DocType
fileExtension :: DocType -> String
fileExtension HtmlDoc = "html"
fileExtension TexDoc  = "tex"
fileExtension CDoc    = "cdoc"

-- harmonized writeFile function for all docType
writeOutfile :: DocParams -> Bool -> String -> String -> IO String -> IO ()
writeOutfile docparams recursive docdir progname generate = do
  doc     <- generate
  imports <- getImports progname
  let outfile = docdir </> getLastName progname <.> fileExtension (docType docparams)
  putStrLn ("Writing documentation to \"" ++ outfile ++ "\"...")
  writeFile outfile doc
  if recursive
    then mapIO_ (makeDocIfNecessary docparams recursive docdir) imports
    else done

-- -----------------------------------------------------------------------