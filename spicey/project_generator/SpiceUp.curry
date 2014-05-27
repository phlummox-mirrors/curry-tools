-- Main module to generate the initial Spicey application

import Directory
import List(last)
import System(system, getArgs, exitWith)
import SpiceyScaffolding
import Distribution

systemBanner =
  let bannerText = "Spicey Web Framework (Version of 27/12/13)"
      bannerLine = take (length bannerText) (repeat '-')
   in bannerLine ++ "\n" ++ bannerText ++ "\n" ++ bannerLine

data FileMode = Exec | NoExec

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

structure = 
  Directory "." [
    ResourceFile NoExec "README.txt",
    ResourcePatchFile NoExec "Makefile" replaceCurryDir,
    ResourceFile NoExec "Main.curry",
    Directory "scripts" [ 
      ResourcePatchFile Exec "deploy.sh"  replaceCurryDir,
      ResourcePatchFile Exec "load.sh"    replaceCurryDir,
      ResourcePatchFile Exec "run.sh"     replaceCurryDir,
      ResourcePatchFile Exec "compile.sh" replaceCurryDir],
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
        ResourceFile NoExec "bootstrap.css",
        ResourceFile NoExec "bootstrap-responsive.css",
        ResourceFile NoExec "style.css"
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

resourceDirectoryLocal = "resource_files" -- script directory gets prepended

-- Replace every occurrence of
-- "XXXCURRYBINXXX" by installDir++"/bin"
-- "XXXCURRYEXECXXX" by installDir++"/bin"++curryCompiler
replaceCurryDir :: String -> String
replaceCurryDir [] = []
replaceCurryDir (c:cs)
  | c=='X' && take 13 cs == "XXCURRYBINXXX"
    = installDir ++ "/bin" ++ replaceCurryDir (drop 13 cs)
  | c=='X' && take 14 cs == "XXCURRYEXECXXX"
    = installDir ++ "/bin/" ++ curryCompiler ++ replaceCurryDir (drop 14 cs)
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
main generatordir = do
  putStrLn systemBanner
  curdir <- getCurrentDirectory
  args <- getArgs
  case args of
    ["--dbpath",dbpath,termfile] -> 
      createStructure curdir generatordir termfile dbpath structure
    [termfile] -> createStructure curdir generatordir termfile "." structure
    _ -> putStrLn ("Wrong argument!\n" ++ helpText) >> exitWith 1

helpText =
  "Usage: spiceup [--dbpath <dirpath>] <ERD term file>\n" ++
   "Parameters:\n" ++
  "--dbpath <dir> : name of the directory where DB files are stored\n" ++
  "<ERD term file>: name of file containing the ERD term\n"