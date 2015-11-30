module ERD2Curry where

import AbstractCurry.Pretty
import XML
import XML2ERD
import ERD
import ERDGoodies
import Transformation
import CodeGeneration
import System(getArgs,system)
import Time
import Directory
import ERD2Graph
import System(exitWith)
import Distribution(curryCompiler)

systemBanner =
  let bannerText = "ERD->Curry Compiler (Version of 30/11/15)"
      bannerLine = take (length bannerText) (repeat '-')
   in bannerLine ++ "\n" ++ bannerText ++ "\n" ++ bannerLine

--- Main function for saved state. The argument is the directory containing
--- these sources.
main erd2currydir = do
  putStrLn systemBanner
  args <- getArgs
  callStart erd2currydir (parseArgs ("",False,SQLite ".",False) args)

parseArgs :: (String,Bool,Storage,Bool) -> [String]
          -> Maybe (String,Bool,Storage,Bool)
parseArgs _ [] = Nothing
parseArgs (file,fromxml,storage,vis) (arg:args) = case arg of
  "-t" -> parseArgs (file,False,storage,vis) args
  "-x" -> parseArgs (file,True,storage,vis) args
  "-l" -> parseArgs (file,fromxml,setSQLite storage,vis) args
  "-f" -> if curryCompiler == "pakcs"
          then parseArgs (file,fromxml,setFileDB storage,vis) args
          else error "Wrong parameter -f: file-based database only available in PAKCS!"
  "-d" -> parseArgs (file,fromxml,DB,vis) args
  "-p" -> if null args then Nothing else
          parseArgs (file,fromxml,setFilePath (head args) storage,vis)
                    (tail args)
  "--dbpath" -> if null args then Nothing else
                parseArgs (file,fromxml,setFilePath (head args) storage,vis)
                          (tail args)
  "-v" -> parseArgs (file,fromxml,storage,True) args
  f    -> if null args then Just (f,fromxml,storage,vis) else Nothing
 where
  setFilePath path (Files  _) = Files  path
  setFilePath path (SQLite _) = SQLite path
  setFilePath _    DB         = DB

  setSQLite (Files  p) = SQLite p
  setSQLite (SQLite p) = SQLite p
  setSQLite DB         = SQLite "."

  setFileDB (Files  p) = Files p
  setFileDB (SQLite p) = Files p
  setFileDB DB         = Files "."

helpText =
  "Usage: erd2curry [-l] [-f] [-d] [-t] [-x] [-v] [--dbpath <dir>] <file>\n" ++
  "Parameters:\n" ++
  "-l: generate interface to SQLite3 database (default)\n" ++
  "-f: generate interface to file-based database implementation (only in PAKCS)\n" ++
  "-d: generate interface to SQL database (experimental)\n" ++
  "-t: generate from ERD term file (default)\n" ++
  "-x: generate from ERD xmi document\n" ++
  "-v: show visualization of ERD term file with dotty\n" ++
  "--dbpath <dir>: name of the directory where DB files are stored\n" ++
  "<file>: name of file containing xmi document or ERD term\n"

callStart _ Nothing = do
  putStrLn $ "ERROR: Illegal arguments\n\n" ++ helpText
  exitWith 1
callStart erd2currydir (Just (file,fromxml,storage,vis)) =
 if vis
 then readERDTermFile file >>= viewERD
 else start erd2currydir (storage,WithConsistencyTest) fromxml file "."

--- Main function to invoke the ERD->Curry translator.
start :: String -> Option -> Bool -> String -> String -> IO ()
start erd2currydir opt fromxml srcfile path = do
  (erdfile,erd) <- if fromxml
                   then transformXmlFile srcfile path
                   else readERDTermFile srcfile >>= \e -> return (srcfile,e)
  let transerdfile = addPath path (erdName erd ++ "_ERDT.term")
      curryfile    = addPath path (erdName erd ++ ".curry")
      transerd     = transform erd
  writeFile transerdfile
            ("{- ERD specification transformed from "++erdfile++" -}\n\n " ++
             showERD 2 transerd ++ "\n")
  putStrLn $ "Transformed ERD term written into file '"++transerdfile++"'."
  moveOldVersion curryfile
  writeFile curryfile $ prettyCProg $ erd2code opt $ transform erd
  copyAuxiliaryFiles
  putStrLn $ "Database operations generated into file '"++curryfile++"'\n"++
             "with " ++ showOption (erdName erd) opt ++ ".\n"
 where
  prettyCProg = prettyCurryProg (setQualification OnDemand defaultOptions)
  
  -- Copy auxiliary files ERDGeneric.curry and KeyDatabase.curry to target dir
  copyAuxiliaryFiles = do
    if isSQLite opt
      then copyFile (erd2currydir++"/KeyDatabase.curry.sqlite")
                    (addPath path "KeyDatabase.curry")
      else done
    copyFile (erd2currydir++"/ERDGeneric.curry")
             (addPath path "ERDGeneric.curry")

  showOption _ (Files f,_) = "database files stored in directory '"++f++"'"
  showOption ername (SQLite p,_) =
    "SQLite3 database stored in file '"++p++"/"++ername++".db'"
  showOption _ (DB,_) = "SQL database interface"

--- Adds a path to a file name.
addPath :: String -> String -> String
addPath path fname | path=="." = fname
                   | otherwise = path++"/"++fname

--- Moves a file (if it exists) to one with extension ".versYYMMDDhhmmss".
moveOldVersion :: String -> IO ()
moveOldVersion fname = do
  exists <- doesFileExist fname
  if exists
   then do
     mtime <- getModificationTime fname
     cmtime <- toCalendarTime mtime
     let fnamevers = fname ++ ".vers" ++ calTime2Digits cmtime
     system $ "mv "++fname++" "++fnamevers
     putStrLn $ "Old contents of file \""++fname++"\" saved into file \""++
                fnamevers++"\"."
   else done
 where
  calTime2Digits (CalendarTime y mo d h mi s _) =
    toD (y `mod` 100) ++ toD mo ++ toD d ++ toD h ++ toD mi ++ toD s

  toD i = if i<10 then '0':show i else show i
  

--test = start "." (Files ".", WithConsistencyTest) True "./Uni.xmi" "."
test = start "." (Files ".", WithConsistencyTest) False "./Uni_ERD.term" "."

testTransformation = do
  xml <- readXmlFile "./Uni.xmi"
  let erd = convert xml
  putStr (showERD 0 erd)
  putStr "\n\n"
  putStrLn (showERD 0 (transform erd))
  
--- Read an ERD specification from an XML file in Umbrello format.
transformXmlFile :: String -> String -> IO (String,ERD)
transformXmlFile xmlfile path = do
  putStrLn $ "Reading XML file " ++ xmlfile ++ "..."
  xml <- readXmlFile xmlfile
  let erd     = convert xml
  let erdfile = addPath path (erdName erd ++ "_ERD.term")
  writeFile erdfile
            ("{- ERD specification read from "++xmlfile++" -}\n\n " ++
             showERD 2 erd ++ "\n")
  putStrLn $ "ERD term written into file \""++erdfile++"\"."
  return (erdfile,erd)

{-
-- Uni.xmi -> ERD term:
(ERD "Uni"
 [(Entity "Student" [(Attribute "MatNum" (IntDom Nothing) PKey False),
                     (Attribute "Name" (StringDom Nothing) NoKey False),
                     (Attribute "Firstname" (StringDom Nothing) NoKey False),
                     (Attribute "Email" (UserDefined "MyModule.Email" Nothing) NoKey True)]),
  (Entity "Lecture" [(Attribute "Id" (IntDom Nothing) PKey False),
                     (Attribute "Title" (StringDom Nothing) Unique False),
                     (Attribute "Hours" (IntDom (Just 4)) NoKey False)]),
  (Entity "Lecturer" [(Attribute "Id" (IntDom Nothing) PKey False),
                      (Attribute "Name" (StringDom Nothing) NoKey False),
                      (Attribute "Firstname" (StringDom Nothing) NoKey False)]),
  (Entity "Group" [(Attribute "Time" (StringDom Nothing) NoKey False)])]
 [(Relationship "Teaching" [(REnd "Lecturer" "taught_by" (Exactly 1)),
                            (REnd "Lecture" "teaches" (Between 0 Infinite))]),
  (Relationship "Participation" [(REnd "Student" "participated_by" (Between 0 Infinite)),
                                 (REnd "Lecture" "participates" (Between 0 Infinite))]),
  (Relationship "Membership" [(REnd "Student" "consists_of" (Exactly 3)),
                              (REnd "Group" "member_of" (Between 0 Infinite))])])

-- Transformation of ERD term:
(ERD "Uni"
 [(Entity "Membership"
          [(Attribute "Student_Membership_Key" (KeyDom "Student") PKey False),
           (Attribute "Group_Membership_Key" (KeyDom "Group") PKey False)]),
  (Entity "Participation"
          [(Attribute "Student_Participation_Key" (KeyDom "Student") PKey False),
           (Attribute "Lecture_Participation_Key" (KeyDom "Lecture") PKey False)]),
  (Entity "Student"
          [(Attribute "Key" (IntDom Nothing) PKey False),
           (Attribute "MatNum" (IntDom Nothing) Unique False),
           (Attribute "Name" (StringDom Nothing) NoKey False),
           (Attribute "Firstname" (StringDom Nothing) NoKey False),
           (Attribute "Email" (UserDefined "MyModule.Email" Nothing) NoKey True)]),
  (Entity "Lecture"
          [(Attribute "Key" (IntDom Nothing) PKey False),
           (Attribute "Lecturer_Teaching_Key" (KeyDom "Lecturer") NoKey False),
           (Attribute "Id" (IntDom Nothing) Unique False),
           (Attribute "Title" (StringDom Nothing) Unique False),
           (Attribute "Hours" (IntDom (Just 4)) NoKey False)]),
  (Entity "Lecturer"
          [(Attribute "Key" (IntDom Nothing) PKey False),
           (Attribute "Id" (IntDom Nothing) Unique False),
           (Attribute "Name" (StringDom Nothing) NoKey False),
           (Attribute "Firstname" (StringDom Nothing) NoKey False)]),
  (Entity "Group"
          [(Attribute "Key" (IntDom Nothing) PKey False),
           (Attribute "Time" (StringDom Nothing) NoKey False)])]
 [(Relationship [] [(REnd "Student" [] (Exactly 1)),
                    (REnd "Membership" "member_of" (Between 0 Infinite))]),
  (Relationship [] [(REnd "Group" [] (Exactly 1)),
                    (REnd "Membership" "consists_of" (Exactly 3))]),
  (Relationship [] [(REnd "Student" [] (Exactly 1)),
                    (REnd "Participation" "participates" (Between 0 Infinite))]),
  (Relationship [] [(REnd "Lecture" [] (Exactly 1)),
                    (REnd "Participation" "participated_by" (Between 0 Infinite))]),
  (Relationship "Teaching" [(REnd "Lecturer" "taught_by" (Exactly 1)),
                            (REnd "Lecture" "teaches" (Between 0 Infinite))])])
-}
