-------------------------------------------------------------------------
--- A transformation of Curry programs into verification tools.
---
--- @author Michael Hanus
--- @version May 2016
-------------------------------------------------------------------------

module ToVerifier where

import AbstractCurry.Types
import AbstractCurry.Files
import AbstractCurry.Select
import AbstractCurry.Transform
import Distribution      (stripCurrySuffix)
import GetOpt
import List
import Maybe             (fromJust)
import Rewriting.Files
import Rewriting.Term
import Rewriting.Rules
import System            (exitWith, getArgs)
import TheoremUsage
import ToAgda
import VerifyOptions

-- to use the determinism analysis:
import AnalysisServer    (analyzeGeneric)
import GenericProgInfo
import Deterministic     (Deterministic(..), nondetAnalysis)
import TotallyDefined    (totalAnalysis)

-- Banner of this tool:
cvBanner :: String
cvBanner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText =
     "curry2verify: Curry programs -> Verifiers (version of 24/05/2016)"
   bannerLine = take (length bannerText) (repeat '-')

-- Help text
usageText :: String
usageText = usageInfo ("Usage: curry2verify [options] <module names>\n") options
  
main :: IO ()
main = do
  argv <- getArgs
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts = foldl (flip id) defaultOptions funopts
  unless (null opterrors)
         (putStr (unlines opterrors) >> putStrLn usageText >> exitWith 1)
  when (optVerb opts > 0) $ putStr cvBanner 
  when (null args || optHelp opts) (putStrLn usageText >> exitWith 1)
  mapIO_ (generateTheorems opts) (map stripCurrySuffix args)

-------------------------------------------------------------------------

-- Generate a file for each theorem found in a module.
generateTheorems :: Options -> String -> IO ()
generateTheorems opts mname = do
  prog <- readCurry mname
  let theorems = filter (isTheoremName . snd . funcName) (functions prog)
  if null theorems
   then putStrLn "No theorems found!"
   else mapIO_ (generateTheorem opts) (map funcName theorems)

-- Generate a file for a given theorem name.
generateTheorem :: Options -> QName -> IO ()
generateTheorem opts qtheoname = do
  (newopts, allprogs, allfuncs) <- getAllFunctions opts [] [] [qtheoname]
  let alltypenames = foldr union []
                           (map (\fd -> tconsOfType (funcType fd)) allfuncs)
  alltypes <- getAllTypeDecls opts allprogs alltypenames []
  case optTarget opts of
    "agda" -> theoremToAgda newopts qtheoname allfuncs alltypes
    t      -> error $ "Unknown translation target: " ++ t

-------------------------------------------------------------------------
--- Extract all type declarations that are refererred in the types
--- of the given functions.
getAllTypeDecls :: Options -> [CurryProg] -> [QName] -> [CTypeDecl]
               -> IO [CTypeDecl]
getAllTypeDecls _ _ [] currtypes = return currtypes
getAllTypeDecls opts currmods (tc:tcs) currtypes
  | tc `elem` primTypes opts ++ map typeName currtypes
  = getAllTypeDecls opts currmods tcs currtypes
  | fst tc `elem` map progName currmods
  = maybe
      (-- if we don't find the qname, it must be a primitive type:
        getAllTypeDecls opts currmods tcs currtypes)
      (\tdecl -> getAllTypeDecls opts currmods tcs (tdecl : currtypes))
      (find (\td -> typeName td == tc)
            (types (fromJust (find (\m -> progName m == fst tc) currmods))))
  | otherwise -- we must load a new module
  = do let mname = fst tc
       when (optVerb opts > 0) $
         putStrLn $ "Loading module '" ++ mname ++ "'..."
       newmod <- readCurry mname
       getAllTypeDecls opts (newmod:currmods) (tc:tcs) currtypes

-------------------------------------------------------------------------

--- Extract all functions that might be called by a given function.
getAllFunctions :: Options -> [CFuncDecl] -> [CurryProg] -> [QName]
                -> IO (Options, [CurryProg], [CFuncDecl])
getAllFunctions opts currfuncs currmods [] = return (opts, currmods, currfuncs)
getAllFunctions opts currfuncs currmods (newfun:newfuncs)
  | newfun `elem` standardConstructors ++ map funcName currfuncs
    || isPrimFunc opts newfun
  = getAllFunctions opts currfuncs currmods newfuncs
  | null (fst newfun) -- local declarations have empty module qualifier
  = getAllFunctions opts currfuncs currmods newfuncs
  | fst newfun `elem` map progName currmods
  = maybe
       (-- if we don't find the qname, it must be a constructor:
        getAllFunctions opts currfuncs currmods newfuncs)
      (\fdecl -> getAllFunctions opts
                    (if null (funcRules fdecl)
                      then currfuncs -- ignore external functions
                      else fdecl : currfuncs)
                    currmods (newfuncs ++ funsOfCFuncDecl fdecl))
      (find (\fd -> funcName fd == newfun)
            (functions
               (fromJust (find (\m -> progName m == fst newfun) currmods))))
  | otherwise -- we must load a new module
  = do let mname = fst newfun
       when (optVerb opts > 0) $
         putStrLn $ "Loading module '" ++ mname ++ "'..."
       newmod <- readCurry mname
       when (optVerb opts > 0) $
         putStrLn $ "Analyzing module '" ++ mname ++ "'..."
       pdetinfo <- analyzeGeneric nondetAnalysis mname
                                                >>= return . either id error
       ptotinfo <- analyzeGeneric totalAnalysis mname
                                                >>= return . either id error
       getAllFunctions
         opts { detInfos = combineProgInfo (detInfos opts) pdetinfo
              , totInfos = combineProgInfo (totInfos opts) ptotinfo }
         currfuncs (newmod:currmods) (newfun:newfuncs)

-- Some standard constructors from the prelude.
standardConstructors :: [QName]
standardConstructors = [pre "[]", pre ":", pre "()"]

-------------------------------------------------------------------------
-- Extract all function (and constructor) names occurring called in
-- a function declaration:

funsOfCFuncDecl :: CFuncDecl -> [QName]
funsOfCFuncDecl =
  nub . trCFuncDecl (\_ _ _ _ _ rules -> concatMap funsOfCRule rules)

funsOfCRule :: CRule -> [QName]
funsOfCRule = trCRule (\_ rhs -> funsOfCRhs rhs)

funsOfCRhs :: CRhs -> [QName]
funsOfCRhs =
  trCRhs (\e  ldecls -> funsOfExpr e ++ concatMap funsOfLDecl ldecls)
         (\gs ldecls -> concatMap (\ (g,e) -> funsOfExpr g ++ funsOfExpr e) gs
                        ++ concatMap funsOfLDecl ldecls)

funsOfLDecl :: CLocalDecl -> [QName]
funsOfLDecl = trCLocalDecl funsOfCFuncDecl (const funsOfCRhs) (const [])

funsOfExpr :: CExpr -> [QName]
funsOfExpr =
  trExpr (const [])
         (const [])
         (\n -> [n])
         (++)
         (const id)
         (\ldecls e -> concatMap funsOfLDecl ldecls ++ e)
         (concatMap funsOfStat)
         (\e stats -> e ++ concatMap funsOfStat stats)
         (\_ e brs -> e ++ concatMap (funsOfCRhs . snd) brs)
         (\e _ -> e)
         (\_ fields -> concatMap snd fields)
         (\e fields -> e ++ concatMap snd fields)
         
funsOfStat :: CStatement -> [QName]
funsOfStat = trCStatement funsOfExpr
                          (const funsOfExpr)
                          (concatMap funsOfLDecl)

-------------------------------------------------------------------------
