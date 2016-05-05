------------------------------------------------------------------------
--- A transformation from a Curry program with pre/postconditions and/or
--- specification into a Curry program where these conditions are integrated
--- into the code as run-time assertions.
------------------------------------------------------------------------

module TransContracts(main,transContracts) where

import AbstractCurry.Types
import AbstractCurry.Files
import AbstractCurry.Pretty
import AbstractCurry.Build
import AbstractCurry.Select
import AbstractCurry.Transform
import Char
import Directory
import Distribution
import List
import Maybe(fromJust)
import System

banner :: String
banner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText = "Contract Transformation Tool (Version of 03/05/16)"
   bannerLine = take (length bannerText) (repeat '=')

------------------------------------------------------------------------
-- Start the contract wrapper in "preprocessor mode":
transContracts :: Int -> [String] -> String -> String -> String -> IO ()
transContracts verb moreopts orgfile infile outfile = do
  when (verb>0) $ putStr banner
  opts <- processOpts defaultOptions moreopts
  let savefile = orgfile++".SAVECONTRACTS"
      modname = stripCurrySuffix orgfile
  renameFile orgfile savefile
  starttime <- getCPUTime
  srcprog <- readFile infile
  writeFile orgfile (replaceOptionsLine srcprog)
  inputProg <- tryReadCurry modname savefile
  renameFile savefile orgfile
  transformCProg opts srcprog (addCmtFuncInProg inputProg) modname outfile
  stoptime <- getCPUTime
  when (verb>1) $ putStrLn
    ("Contract wrapper transformation time: " ++
     show (stoptime-starttime) ++ " ms")
 where
  processOpts opts ppopts = case ppopts of
    []          -> return opts
    ("-e":more) -> processOpts (opts { withEncapsulate   = True }) more
    ("-t":more) -> processOpts (opts { topLevelContracts = True }) more
    _           -> showError
   where
    showError = do
      putStrLn $ "Unknown options (ignored): " ++ show (unwords ppopts)
      return opts

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

------------------------------------------------------------------------
-- Data type for transformation parameters
data Options = Options
  { -- encapsulate assertion checking by set functions?
    withEncapsulate   :: Bool
    -- should contracts be asserted only to top-level entries of an operation
    -- or also to all (recursive) calls?
  , topLevelContracts :: Bool
    -- load and execute transformed program?
  , executeProg       :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { withEncapsulate   = False
  , topLevelContracts = False
  , executeProg       = False
  }

------------------------------------------------------------------------
-- Start the contract wrapper in "stand-alone mode":
main :: IO ()
main = do
  putStrLn banner
  args <- getArgs
  processArgs defaultOptions args
 where
  processArgs opts args = case args of
     ("-e":margs) -> processArgs (opts { withEncapsulate   = True }) margs
     ("-t":margs) -> processArgs (opts { topLevelContracts = True }) margs
     ("-r":margs) -> processArgs (opts { executeProg       = True }) margs
     [mnamec]        -> let mname = stripCurrySuffix mnamec
                         in transform opts mname
                                      (transformedModName mname ++ ".curry")

     _ -> putStrLn $ unlines $
           ["ERROR: Illegal arguments for transformation: " ++ unwords args
           ,""
           ,"Usage: cwrapper [-e] [-t] [-r] <module_name>"
           ,"-e   : encapsulate nondeterminism of assertions"
           ,"-t   : assert contracts only to top-level (not recursive) calls"
           ,"-r   : load the transformed program into Curry system"
           ]

-- Specifies how the name of the transformed module is built from the
-- name of the original module.
transformedModName :: String -> String
transformedModName m = m++"C"

-- start PAKCS and load a module:
loadIntoCurry :: String -> IO ()
loadIntoCurry m = do
  putStrLn $ "\nStarting Curry system and loading module '"++m++"'..."
  system $ installDir++"/bin/curry :l "++m
  done

-- The main transformation function.
transform :: Options -> String -> String -> IO ()
transform opts modname outfile = do
  mmodsrc <- lookupModuleSourceInLoadPath modname
  srcprog <- case mmodsrc of
               Nothing -> error $
                            "Source code of module '"++modname++"' not found!"
               Just (_,progname) -> readFile progname
  let acyfile = abstractCurryFileName modname
  doesFileExist acyfile >>= \b -> if b then removeFile acyfile else done
  prog <- readCurry modname >>= return . addCmtFuncInProg
  doesFileExist acyfile >>= \b -> if b then done
                                       else error "Source program incorrect"
  transformCProg opts srcprog prog (transformedModName modname) outfile

transformCProg :: Options -> String -> CurryProg -> String -> String -> IO ()
transformCProg opts srctxt prog outmodname outfile = do
  let funposs   = linesOfFDecls srctxt prog
      fdecls    = functions prog
      funspecs  = getFunDeclsWith isSpecName prog
      specnames = map (dropSpecName . snd . funcName) funspecs
      preconds  = getFunDeclsWith isPreCondName prog
      prenames  = map (dropPreCondName  . snd . funcName) preconds
      postconds = getFunDeclsWith isPostCondName prog
      postnames = map (dropPostCondName  . snd . funcName) postconds
      checkfuns = union specnames (union prenames postnames)
      onlyprecond  = prenames  \\ map (snd . funcName) fdecls
      onlypostcond = postnames \\ map (snd . funcName) fdecls
      onlyspec     = specnames \\ map (snd . funcName) fdecls
      newprog      = transformProgram opts funposs fdecls funspecs preconds
                                      postconds prog
  unless (null onlyprecond) $
     error ("Operations with precondition but without an implementation: "
            ++ unwords onlyprecond)
  unless (null onlypostcond) $
     error ("Operations with postcondition but without an implementation: "
            ++ unwords onlypostcond)
  unless (null onlyspec) $
     error ("Operations with a specification but without an implementation: "
            ++ unwords onlyspec)
  if null checkfuns then done else
    putStrLn $ "Adding contract checking to: " ++ unwords checkfuns
  if null (funspecs++preconds++postconds)
   then putStrLn
    "No specifications or pre/postconditions found, no transformation required!"
   else do writeFile outfile (showCProg (renameCurryModule outmodname newprog))
           if executeProg opts
            then loadIntoCurry outmodname
            else done

-- Get functions from a Curry module with a name satisfying the predicate:
getFunDeclsWith :: (String -> Bool) -> CurryProg -> [CFuncDecl]
getFunDeclsWith pred prog = filter (pred . snd . funcName) (functions prog)

-- Transform a given program w.r.t. given specifications and pre/postconditions
transformProgram :: Options -> [(QName,Int)]-> [CFuncDecl] -> [CFuncDecl]
                 -> [CFuncDecl] -> [CFuncDecl] -> CurryProg -> CurryProg
transformProgram opts funposs allfdecls specdecls predecls postdecls
                 (CurryProg mname imps tdecls fdecls opdecls) =
 let newpostconds = concatMap (genPostCond4Spec opts allfdecls postdecls)
                              specdecls
     newfunnames  = map (snd . funcName) newpostconds
     wonewfuns    = filter (\fd -> snd (funcName fd) `notElem` newfunnames)
                           fdecls -- remove functions having new gen. defs.
     -- compute postconditions actually used for contract checking:
     contractpcs  = postdecls++newpostconds
  in CurryProg mname
               (nub ("Test.Contract":"SetFunctions":imps))
               tdecls
               (map deleteCmtIfEmpty
                  (map (addContract opts funposs allfdecls predecls contractpcs)
                       wonewfuns ++
                   newpostconds))
               opdecls

-- Add an empty comment to each function which has no comment
addCmtFuncInProg :: CurryProg -> CurryProg
addCmtFuncInProg (CurryProg mname imps tdecls fdecls opdecls) =
  CurryProg mname imps tdecls (map addCmtFunc fdecls) opdecls
 where
  addCmtFunc (CFunc qn ar vis texp rs) = CmtFunc "" qn ar vis texp rs
  addCmtFunc (CmtFunc cmt qn ar vis texp rs) = CmtFunc cmt qn ar vis texp rs

-- Generate a postcondition from a specification that is parameterized
-- by an "observation function".
-- If the specification is deterministic, generate an equality check,
-- otherwise generate a set containment check.
genPostCond4Spec :: Options -> [CFuncDecl] -> [CFuncDecl] -> CFuncDecl
                 -> [CFuncDecl]
genPostCond4Spec _ _ _ (CFunc _ _ _ _ _) = error "genPostCond4Spec"
genPostCond4Spec _ allfdecls postdecls (CmtFunc _ (m,f) ar vis texp _) =
 let fname     = dropSpecName f
     detspec   = isDetSpecName f -- determ. spec? (later: use prog.ana.)
     fpostname = fname++"'post"
     fpgenname = fpostname++"'generic"
     oldfpostc = filter (\fd -> snd (funcName fd) == fpostname) postdecls
     oldcmt    = if null oldfpostc then ""
                                   else '\n' : funcComment (head oldfpostc)
     varg      = (0,"g")
     argvars   = map (\i -> (i,"x"++show i)) [1..(ar+1)]
     spargvars = take ar argvars
     resultvar = last argvars
     gtype     = CTVar (0,"grt") -- result type of observation function
     varz      = (ar+2,"z")
     obsfun    = maybe (pre "id")
                       funcName
                       (find (\fd -> snd (funcName fd) == fpostname++"'observe")
                             allfdecls)
     gspecname = (m,f++"'g")
     gspec     = cfunc gspecname ar Private
                    ((resultType texp ~> gtype) ~> replaceResultType texp gtype)
                    [let gsargvars = map (\i -> (i,"x"++show i)) [1..ar] in
                     simpleRule (CPVar varg : map CPVar gsargvars)
                                (CApply (CVar varg)
                                        (applyF (m,f) (map CVar gsargvars)))]
     postcheck = CLetDecl
                  [CLocalPat (CPVar varz)
                     (CSimpleRhs (CApply (CVar varg) (CVar resultvar)) [])]
                  (if detspec
                   then applyF (pre "==")
                          [CVar varz,
                           applyF gspecname (map CVar (varg : spargvars))]
                   else applyF (pre "&&")
                         [applyF (pre "==") [CVar varz, CVar varz],
                          applyF (sfMod "valueOf")
                           [CVar varz,
                            applyF (sfMod $ "set"++show (ar+1))
                             (constF gspecname : map CVar (varg :spargvars))]])
     rename qf = if qf==(m,fpostname) then (m,fpostname++"'org") else qf
  in [cmtfunc
       ("Parametric postcondition for '"++fname++
        "' (generated from specification). "++oldcmt)
       (m,fpgenname) (ar+2) vis
       ((resultType texp ~> gtype) ~> extendFuncType texp boolType)
       [if null oldfpostc
        then simpleRule (map CPVar (varg:argvars)) postcheck
        else simpleRuleWithLocals
                (map CPVar (varg:argvars))
                (applyF (pre "&&")
                             [applyF (rename (funcName (head oldfpostc)))
                                     (map CVar argvars),
                              postcheck])
                [updQNamesInCLocalDecl rename
                        (CLocalFunc (deleteCmt (head oldfpostc)))]]
     ,gspec
     ,cmtfunc
       ("Postcondition for '"++fname++"' (generated from specification). "++
        oldcmt)
       (m,fpostname) (ar+1) vis
       (extendFuncType texp boolType)
       [simpleRule (map CPVar argvars)
                   (applyF (m,fpgenname)
                           (constF obsfun : map CVar argvars))]
     ]

-- adds contract checking to a function if it has a pre- or postcondition
addContract :: Options -> [(QName,Int)] -> [CFuncDecl] -> [CFuncDecl]
            -> [CFuncDecl] -> CFuncDecl -> CFuncDecl
addContract _ _ _ _ _ (CFunc _ _ _ _ _) = error "addContract"
addContract opts funposs allfdecls predecls postdecls
            fdecl@(CmtFunc cmt qn@(m,f) ar vis texp _) =
 let argvars   = map (\i -> (i,"x"++show i)) [1..ar]
     predecl   = find (\fd -> dropPreCondName(snd(funcName fd)) == f) predecls
     prename   = funcName (fromJust predecl)
     postdecl  = find (\fd-> dropPostCondName(snd(funcName fd)) == f) postdecls
     postname  = funcName (fromJust postdecl)
     encapsSuf = if withEncapsulate opts then "ND" else ""
     encaps fn n = if withEncapsulate opts then setFun n fn [] else constF fn
     rename qf = if qf==(m,f) then (m,f++"'org") else qf
     fref      = string2ac $
                  "'" ++ f ++ "' (module " ++ m ++
                         maybe ")"
                               (\l -> ", line " ++ show l ++ ")")
                               (lookup qn funposs)
     orgfunexp = constF (rename (m,f))
     obsfunexp = constF $
                  maybe (pre "id")
                       funcName
                       (find (\fd -> snd (funcName fd) == f++"'post'observe")
                             allfdecls)
     asrtCall  = if predecl==Nothing
                 then applyF (cMod $ "withPostContract" ++ show ar ++ encapsSuf)
                        ([fref, encaps postname (ar+1), obsfunexp, orgfunexp] ++
                         map CVar argvars)
                 else if postdecl==Nothing
                 then applyF (cMod $ "withPreContract" ++ show ar ++ encapsSuf)
                        ([fref, encaps prename ar, orgfunexp] ++
                         map CVar argvars)
                 else applyF (cMod $ "withContract" ++ show ar ++ encapsSuf)
                        ([fref, encaps prename ar,
                          encaps postname (ar+1), obsfunexp, orgfunexp] ++
                         map CVar argvars)
     oldfdecl = if topLevelContracts opts
                then updQNamesInCLocalDecl rename (CLocalFunc (deleteCmt fdecl))
                else CLocalFunc (renameFDecl rename (deleteCmt fdecl))
  in if predecl==Nothing && postdecl==Nothing then fdecl else
       cmtfunc cmt (m,f) ar vis texp
               [simpleRuleWithLocals (map CPVar argvars) asrtCall [oldfdecl]]


-- Is this the name of a specification?
isSpecName :: String -> Bool
isSpecName f = let rf = reverse f
                in take 5 rf == "ceps'" || take 6 rf == "dceps'"

-- Is this the name of a deterministic specification?
isDetSpecName :: String -> Bool
isDetSpecName f = take 6 (reverse f) == "dceps'"

-- Drop the specification suffix from the name:
dropSpecName :: String -> String
dropSpecName f =
  let rf = reverse f
   in reverse (drop (if take 5 rf == "ceps'" then 5 else
                     if take 6 rf == "dceps'" then 6 else 0) rf)

-- Is this the name of a precondition?
isPreCondName :: String -> Bool
isPreCondName f = take 4 (reverse f) == "erp'"

-- Drop the precondition suffix from the name:
dropPreCondName :: String -> String
dropPreCondName f =
  let rf = reverse f
   in reverse (drop (if take 4 rf == "erp'" then 4 else 0) rf)

-- Is this the name of a precondition?
isPostCondName :: String -> Bool
isPostCondName f = take 5 (reverse f) == "tsop'"

-- Drop the postcondition suffix from the name:
dropPostCondName :: String -> String
dropPostCondName f =
  let rf = reverse f
   in reverse (drop (if take 5 rf == "tsop'" then 5 else 0) rf)

-- An operation of the module Test.Contract:
cMod :: String -> QName
cMod f = ("Test.Contract",f)

-- An operation of the module SetFunctions:
sfMod :: String -> QName
sfMod f = ("SetFunctions",f)

-- Set function for a function name with given arity and arguments:
setFun :: Int -> QName -> [CExpr] -> CExpr
setFun n qn args = applyF (sfMod $ "set"++show n) (constF qn : args)

------------------------------------------------------------------------
-- Auxiliary operations:

-- Replaces a result type of a function type by a new type
replaceResultType :: CTypeExpr -> CTypeExpr -> CTypeExpr
replaceResultType texp ntype =
  case texp of CFuncType t1 t2 -> CFuncType t1 (replaceResultType t2 ntype)
               _               -> ntype

-- Transform a n-ary function type into a (n+1)-ary function type with
-- a given new result type
extendFuncType :: CTypeExpr -> CTypeExpr -> CTypeExpr
extendFuncType t@(CTVar _) texp = t ~> texp
extendFuncType t@(CTCons _ _) texp = t ~> texp
extendFuncType (CFuncType t1 t2) texp = t1 ~> (extendFuncType t2 texp)

--- Renames a function declaration (but not the body).
renameFDecl :: (QName -> QName) -> CFuncDecl -> CFuncDecl
renameFDecl rn (CFunc qn ar vis texp rules) = CFunc (rn qn) ar vis texp rules
renameFDecl _ (CmtFunc _ _ _ _ _ _) = error "renameFDecl"

--- Deletes the comment in a function declaration.
deleteCmt :: CFuncDecl -> CFuncDecl
deleteCmt (CFunc     qn ar vis texp rules) = CFunc qn ar vis texp rules
deleteCmt (CmtFunc _ qn ar vis texp rules) = CFunc qn ar vis texp rules

--- Deletes the comment in a function declaration if it is the empty string.
deleteCmtIfEmpty :: CFuncDecl -> CFuncDecl
deleteCmtIfEmpty (CFunc qn ar vis texp rules)     = CFunc qn ar vis texp rules
deleteCmtIfEmpty (CmtFunc cmt qn ar vis texp rules) =
  if null cmt then CFunc qn ar vis texp rules
              else CmtFunc cmt qn ar vis texp rules

------------------------------------------------------------------------
-- Compute names and lines numbers of all top-level operations in a program.
linesOfFDecls :: String -> CurryProg -> [(QName,Int)]
linesOfFDecls srctxt prog =
  map (addSourceLineNumber (map firstId (lines srctxt)))
      (map funcName (functions prog))
 where
  addSourceLineNumber ids qn = (qn, maybe 0 (+1) (elemIndex (snd qn) ids))

-- Compute the first identifier (name or operator in brackets) in a string:
firstId :: String -> String
firstId [] = ""
firstId (c:cs)
  | isAlpha c = takeWhile isIdChar (c:cs)
  | c == '('  = let bracketid = takeWhile (/=')') cs
                 in if all (`elem` infixIDs) bracketid
                    then bracketid
                    else ""
  | otherwise = ""

-- Is this an alphanumeric character, underscore, or apostroph?
isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c == '_' || c == '\''

-- All characters occurring in infix operators.
infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

------------------------------------------------------------------------