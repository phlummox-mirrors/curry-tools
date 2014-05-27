------------------------------------------------------------------------
--- This is the main file for scaffolding.
------------------------------------------------------------------------

import ERD
import IO
import AbstractCurry
import AbstractCurryPrinter
import PrettyAbstract
import AbstractCurryGoodies
import Transformation
import ERDGoodies
import System(system)
import Distribution(installDir)

import GenerationHelper
import ControllerGeneration
import ViewGeneration
import RouteGeneration
import EntitiesToHtmlGeneration

  
getRelationships :: ERD -> [Relationship]
getRelationships (ERD _ _ relationships) = relationships

getEntities :: ERD -> [Entity]
getEntities (ERD _ entities _) = entities

createViewsForTerm :: String -> String -> String -> String -> IO ()
createViewsForTerm _ termpath path _ = do
  erd <- readERDTermFile termpath
  createViews path erd
    
createViews :: String -> ERD -> IO ()
createViews path (ERD name entities relationship) =
   mapIO_ (saveView name (getEntities erdt) (getRelationships erdt))
          (filter (not . GenerationHelper.isGenerated) (getEntities erdt))
  where
    erdt = transform (ERD name entities relationship)

    saveView :: String -> [Entity] -> [Relationship] -> Entity -> IO ()
    saveView erdname allEntities relationships (Entity ename attrlist) = do
      putStrLn ("Saving View "++ename++"View.curry ...")
      writeFile (path++"/"++ename++"View.curry")
                (showCProg (generateViewsForEntity erdname allEntities
                              (Entity ename attrlist) relationships))

createControllersForTerm :: String -> String -> String -> String -> IO ()
createControllersForTerm _ termpath path _ = do
  erd <- readERDTermFile termpath
  createControllers path erd
      
createControllers :: String -> ERD -> IO ()
createControllers path (ERD name entities relationship) = do
   mapIO_ (saveController name (getEntities erdt) (getRelationships erdt))
          (filter (not . GenerationHelper.isGenerated) (getEntities erdt))
   putStrLn "Generating default controller authorization AuthorizedControllers.curry..."
   writeFile (path++"/"++"DefaultController.curry")
             (showCProg (generateDefaultController name entities))
   writeFile (path++"/"++"AuthorizedControllers.curry")
             (showCProg (generateAuthorizations name entities))
 where
  erdt = transform (ERD name entities relationship)

  saveController :: String -> [Entity] -> [Relationship] -> Entity -> IO ()
  saveController erdname allEntities relationships (Entity ename attrlist) = do
    putStrLn ("Saving Controller "++ename++"Controller.curry ...")
    writeFile (path++"/"++ename++"Controller.curry")
              (showCProg (generateControllersForEntity erdname allEntities
                            (Entity ename attrlist) relationships))

createHtmlHelpersForTerm :: String -> String -> String -> String -> IO ()
createHtmlHelpersForTerm _ termpath path _ = do
  erd <- readERDTermFile termpath
  createHtmlHelper path erd    
    
createHtmlHelper :: String -> ERD -> IO ()
createHtmlHelper path (ERD name entities relationship) =
    saveToHtml name (getEntities erdt) (getRelationships erdt)
 where
  erdt = transform (ERD name entities relationship)

  saveToHtml :: String -> [Entity] -> [Relationship] -> IO ()
  saveToHtml erdname allEntities relationships = do
    putStrLn ("Saving "++erdname++"EntitiesToHtml.curry ...")
    fileh <- openFile (path++"/"++erdname++"EntitiesToHtml.curry") WriteMode
    hPutStr fileh (showCProg (generateToHtml erdname allEntities relationships))
    hClose fileh

-- uses Curry's build-in tool for ERD to Curry transformation
createModelsForTerm :: String -> String -> String -> String -> IO ()
createModelsForTerm _ term_path path db_path = do
  erd <- readERDTermFile term_path
  system $ installDir ++ "/bin/erd2curry -l -t --dbpath " ++ db_path
                      ++ " " ++ term_path
  let orgerdfile   = erdName erd ++ "_ERD.term"
      transerdfile = erdName erd ++ "_ERDT.term"
      curryfile    = erdName erd ++ ".curry"
  system $ unwords ["mv",transerdfile,curryfile,"ERDGeneric.curry",
                         "KeyDatabase.curry",path]
  system $ unwords ["cp",term_path,path ++ "/" ++ orgerdfile]
  done

createRoutesForTerm :: String -> String -> String -> String -> IO ()
createRoutesForTerm _ termpath path _ = do
  erd <- readERDTermFile termpath
  createRoutes path erd

createRoutes :: String -> ERD -> IO ()
createRoutes path erd = do
  putStrLn ("Saving "++mappingModuleName++".curry ...")
  mmfileh <- openFile (path++"/"++mappingModuleName++".curry") WriteMode
  hPutStr mmfileh (showCProg (generateRoutesForERD erd))
  hClose mmfileh  
  putStrLn ("Saving "++dataModuleName++".curry ...")
  dmfileh <- openFile (path++"/"++dataModuleName++".curry") WriteMode
  hPutStr dmfileh (showCProg (generateStartpointDataForERD erd))
  hClose dmfileh

------------------------------------------------------------------------