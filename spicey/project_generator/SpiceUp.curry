-- Main module to generate the initial Spicey application

import Database.ERDGoodies (storeERDFromProgram)
import Directory
import List                (isSuffixOf, last)
import System              (system, getArgs, exitWith)
import SpiceyScaffolding
import Distribution

systemBanner :: String
systemBanner =
  let bannerText = "Spicey Web Framework (Version of 31/10/16)"
      bannerLine = take (length bannerText) (repeat '-')
   in bannerLine ++ "\n" ++ bannerText ++ "\n" ++ bannerLine

data FileMode = Exec | NoExec

setFileMode :: FileMode -> String -> IO ()
setFileMode fmode filename =
  if fmode==Exec then system ("chmod +x \"" ++ filename ++ "\"") >> done
                 else done

data DirTree =
   Directory String [DirTree] -- a directory to be created
 | ResourceFile FileMode String   -- a file to be copied from resource directory
 | ResourcePatchFile FileMode String (String->String) -- file to be copied from
      -- resource directory where its contents is patched by the given function
 | GeneratedFromERD (String -> String -> String -> String -> IO ())
   -- takes an operation to generate code from ERD specification

spiceyStructure :: DirTree
spiceyStructure = 
  Directory "." [
    ResourceFile NoExec "README.txt",
    ResourcePatchFile NoExec "Makefile" replaceCurryDir,
    ResourceFile NoExec "Main.curry",
    Directory "system" [
      ResourceFile NoExec "Spicey.curry",
      ResourceFile NoExec "Routes.curry",
      ResourceFile NoExec "Crypto.curry",
      ResourceFile NoExec "Session.curry",
      ResourceFile NoExec "SessionInfo.curry",
      ResourceFile NoExec "Authorization.curry",
      ResourceFile NoExec "Authentication.curry",
      ResourceFile NoExec "Processes.curry" ],
    Directory "views" [
      ResourceFile NoExec "SpiceySystemView.curry",
      GeneratedFromERD createViewsForTerm,
      GeneratedFromERD createHtmlHelpersForTerm ],
    Directory "controllers" [
      ResourceFile NoExec "SpiceySystemController.curry",
      GeneratedFromERD createControllersForTerm ],
    Directory "models" [
      GeneratedFromERD createModelsForTerm ],
    Directory "config" [
      ResourceFile NoExec "UserProcesses.curry",
      GeneratedFromERD createRoutesForTerm ],
    Directory "public" [
      ResourceFile NoExec "index.html",
      ResourceFile NoExec "favicon.ico",
      Directory "css" [
        ResourceFile NoExec "bootstrap.min.css",
        ResourceFile NoExec "spicey.css"
      ],
      Directory "js" [
        ResourceFile NoExec "bootstrap.min.js",
        ResourceFile NoExec "jquery.min.js"
      ],
      Directory "fonts" [
        ResourceFile NoExec "glyphicons-halflings-regular.eot",
        ResourceFile NoExec "glyphicons-halflings-regular.svg",
        ResourceFile NoExec "glyphicons-halflings-regular.ttf",
        ResourceFile NoExec "glyphicons-halflings-regular.woff",
        ResourceFile NoExec "glyphicons-halflings-regular.woff2"
      ],
      Directory "images" [
        ResourceFile NoExec "spicey-logo.png",
        ResourceFile NoExec "text.png",
        ResourceFile NoExec "time.png",
        ResourceFile NoExec "number.png",
        ResourceFile NoExec "foreign.png"
      ]
    ]
  ]

resourceDirectoryLocal :: String
resourceDirectoryLocal = "resource_files" -- script directory gets prepended

-- Replace every occurrence of "XXXCURRYBINXXX" by installDir++"/bin"
replaceCurryDir :: String -> String
replaceCurryDir [] = []
replaceCurryDir (c:cs)
  | c=='X' && take 13 cs == "XXCURRYBINXXX"
    = installDir ++ "/bin" ++ replaceCurryDir (drop 13 cs)
  | otherwise = c : replaceCurryDir cs

prependPath :: String -> String -> String
prependPath path name  | last path == '/'  = path ++ name
                       | otherwise         = path ++ "/" ++ name

  
copyFileLocal :: FileMode -> String -> String -> String -> IO ()
copyFileLocal fmode path generator_path filename = do
  let infile  = prependPath (prependPath generator_path resourceDirectoryLocal)
                            filename
  let outfile = prependPath path filename
  system $ "cp \"" ++ infile ++ "\" \"" ++ outfile ++ "\""
  setFileMode fmode outfile

-- checks if given path exists (file or directory) and executes
-- given action if not
ifNotExistsDo :: String -> IO () -> IO ()
ifNotExistsDo path cmd = do
  fileExists <- doesFileExist path
  dirExists  <- doesDirectoryExist path
  if fileExists || dirExists
    then putStrLn ("Skipping " ++ path ++ " ...")
    else cmd

createStructure :: String -> String -> String -> String -> DirTree -> IO ()
createStructure target_path generator_path _ _ (ResourceFile fmode filename) =
  let full_path = prependPath target_path filename
   in ifNotExistsDo full_path
         (putStrLn ("Creating file " ++ full_path ++ " ...") >>
          copyFileLocal fmode target_path generator_path filename)
      
createStructure target_path generator_path _ _
                (ResourcePatchFile fmode filename f) =
  let full_path = prependPath target_path filename
   in ifNotExistsDo full_path $ do
         putStrLn ("Creating file " ++ full_path ++ " ...")
         cnt <- readFile (prependPath (prependPath generator_path
                                                   resourceDirectoryLocal)
                                       filename)
         let outfile = prependPath target_path filename
         writeFile outfile (f cnt)
         setFileMode fmode outfile

createStructure target_path generator_path term_path db_path
                (Directory dirname subtree) = do
  let full_path = prependPath target_path dirname
  ifNotExistsDo full_path (putStrLn ("Creating directory "++full_path++" ...")
                           >> createDirectory full_path)
  foldl (\a b -> a >> createStructure full_path generator_path term_path db_path b)
        done subtree
      
createStructure target_path generator_path term_path db_path
                (GeneratedFromERD generatorFunction) = do
  putStrLn $ "Generating from term file " ++ term_path ++ " ..."
  generatorFunction generator_path term_path target_path db_path

--- The main operation to start the scaffolding.
--- The argument is the directory containing the project generator.
main :: String -> IO ()
main generatordir = do
  putStrLn systemBanner
  args <- getArgs
  case args of
    ["--dbpath",dbpath,orgfile] -> createStructureWith orgfile dbpath
    [orgfile]                   -> createStructureWith orgfile "."
    _ -> putStrLn ("Wrong arguments!\n" ++ helpText) >> exitWith 1
  putStrLn $ take 70 (repeat '-')
  putStrLn "Source files for the application generated.\n"
  putStrLn "IMPORTANT NOTE: Before you deploy your web application (by 'make deploy'),"
  putStrLn "you should define the variable WEBSERVERDIR in the Makefile!"
 where
  createStructureWith orgfile dbpath = do
    exfile <- doesFileExist orgfile
    unless exfile $ error ("File `" ++ orgfile ++ "' does not exist!")
    termfile <- if ".curry" `isSuffixOf` orgfile ||
                   ".lcurry" `isSuffixOf` orgfile
                  then storeERDFromProgram orgfile
                  else return orgfile
    curdir <- getCurrentDirectory
    createStructure curdir generatordir termfile dbpath spiceyStructure
    -- delete generated ERD term file:
    when (orgfile /= termfile) $ removeFile termfile

helpText :: String
helpText =
  "Usage: curry spiceup [--dbpath <dirpath>] <ERD program file>\n" ++
   "Parameters:\n" ++
  "--dbpath <dir>    : name of the directory where DB files are stored\n" ++
  "<ERD program file>: name of Curry program file containing ERD definition\n"
