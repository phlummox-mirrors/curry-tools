------------------------------------------------------------------------------
--- Generate an interface description or a human-readable
--- presentation of a Curry module.
---
--- The interface description contains the type declarations
--- for all entities defined and exported by this module.
---
--- The human-readable presentation is (almost) Curry source code
--- generated from a FlatCurry program.
---
--- @author Michael Hanus
--- @version April 2013
------------------------------------------------------------------------------

import FlatCurry
import FlatCurryShow
import List
import Char(isAlpha)
import System(getArgs,getEnviron,system)
import Directory
import FileGoodies
import Sort(mergeSort,leqString)
import Distribution(getLoadPathForFile)

main = do
  args <- getArgs
  case args of
    ["-mod",mod] -> showCurryMod mod
    ["-int",mod] -> showInterface mod
    ["-mod",mod,target] -> writeCurryMod target mod
    ["-int",mod,target] -> writeInterface target mod
    _ -> putStrLn $ "ERROR: Illegal arguments for genint: " ++
                    intercalate " " args ++ "\n" ++
                    "Usage: [-mod|-int] module_name [targetfile]"

-- show interface on stdout:
showInterface :: String -> IO ()
showInterface progname =
  do intstring <- genInt False progname
     putStrLn ("Interface of module \""++progname++"\":\n")
     putStrLn intstring

-- write interface into target file:
writeInterface :: String -> String -> IO ()
writeInterface targetfile progname =
  do intstring <- genInt True progname
     writeFile targetfile
               ("--Interface of module \""++progname++"\":\n\n"++
                intstring)
     putStrLn ("Interface written into file \""++targetfile++"\"")


-----------------------------------------------------------------------
-- Get a FlatCurry program (parse only if necessary):
getFlatProg :: String -> IO Prog
getFlatProg modname = do
  progname <- findSourceFileInLoadPath modname
  let fcyprogname = flatCurryFileName progname
  fcyexists <- doesFileExist fcyprogname
  if not fcyexists
    then readFlatCurry progname
    else do ctime <- getSourceModificationTime progname
            ftime <- getModificationTime fcyprogname
            if ctime>ftime
             then readFlatCurry progname
             else readFlatCurryFile fcyprogname

getSourceModificationTime progname = do
  lexists <- doesFileExist (progname++".lcurry")
  if lexists then getModificationTime (progname++".lcurry")
             else getModificationTime (progname++".curry")

-- add a directory name for a Curry source file by looking up the
-- current load path (CURRYPATH):
findSourceFileInLoadPath modname = do
  loadpath <- getLoadPathForFile modname
  mbfname <- lookupFileInPath (baseName modname) [".lcurry",".curry"] loadpath
  maybe (error ("Curry file for module \""++modname++"\" not found!"))
        (return . stripSuffix)
        mbfname

-----------------------------------------------------------------------
-- Generate interface description for a program:
-- If first argument is True, generate stubs (...external) for
-- all functions so that the resulting interface is a valid Curry program.
genInt :: Bool -> String -> IO String
genInt genstub progname = do
 (Prog mod imports types funcs ops) <- getFlatInt progname
 return $ concatMap showInterfaceImport imports ++ "\n" ++
          concatMap showInterfaceOpDecl (mergeSort leqOp ops) ++
          (if null ops then "" else "\n") ++
          concatMap (showInterfaceType (showQNameInModule mod))
                    (mergeSort leqType types) ++ "\n" ++
          concatMap (showInterfaceFunc (showQNameInModule mod) genstub)
                    (mergeSort leqFunc funcs) ++ "\n"

-- Get a FlatCurry program (parse only if necessary):
getFlatInt :: String -> IO Prog
getFlatInt modname = do
  progname <- findSourceFileInLoadPath modname
  let fintprogname = flatCurryIntName progname
  fintexists <- doesFileExist fintprogname
  if not fintexists
    then readFlatCurryInt progname
    else do ctime <- getSourceModificationTime progname
            ftime <- getModificationTime fintprogname
            if ctime>ftime
             then readFlatCurryInt progname
             else readFlatCurryFile fintprogname

-- write import declaration
showInterfaceImport impmod = if impmod=="Prelude"
                             then ""
                             else "import "++impmod++"\n"

-- show operator declaration
showInterfaceOpDecl (Op op InfixOp  prec) = "infix "++show prec++" "++showOp op++"\n"
showInterfaceOpDecl (Op op InfixlOp prec) = "infixl "++show prec++" "++showOp op++"\n"
showInterfaceOpDecl (Op op InfixrOp prec) = "infixr "++show prec++" "++showOp op++"\n"

showOp (_,on) = if isAlpha (head on) then '`':on++"`"
                                     else on

-- show type declaration
showInterfaceType tt (Type (_,tcons) vis tvars constrs) =
  if vis==Public
  then "data " ++ tcons ++ concatMap (\i->[' ',chr (97+i)]) tvars ++
       (if null constxt then "" else " = " ++ constxt)
       ++ "\n"
  else ""
 where
  constxt = intercalate " | "
              (map (showExportConsDecl tt)
                   (filter (\ (Cons _ _ cvis _)->cvis==Public) constrs))
showInterfaceType tt (TypeSyn (_,tcons) vis tvars texp) =
  if vis==Public
  then "type " ++ tcons ++ concatMap (\i->[' ',chr (97+i)]) tvars ++
       " = " ++ showCurryType tt True texp ++ "\n"
  else ""

showExportConsDecl tt (Cons (_,cname) _ _ argtypes) =
  cname ++ concatMap (\t->" "++showCurryType tt True t) argtypes

-- show function type declaration
showInterfaceFunc ttrans genstub (Func (_,fname) _ vis ftype _) =
  if vis==Public
  then showCurryId fname ++ " :: " ++
       showCurryType ttrans False ftype ++ "\n" ++
       (if genstub then showCurryId fname ++ " external\n\n" else "")
  else ""

---------------------------------------------------------------------------
-- generate a human-readable representation of a Curry module:

-- show representation on stdout:
showCurryMod :: String -> IO ()
showCurryMod progname =
  do modstring <- genCurryMod progname
     putStrLn ("-- Program file: "++progname)
     putStrLn modstring

-- write representation into file:
writeCurryMod :: String -> String -> IO ()
writeCurryMod targetfile progname =
  do modstring <- genCurryMod progname
     writeFile targetfile
               ("--Program file: "++progname++"\n\n"++
                modstring)
     putStrLn ("Module written into file \""++targetfile++"\"")

genCurryMod :: String -> IO String
genCurryMod progname = do
  prog <- readFlatCurryFile (flatCurryFileName progname)
  return $ showCurryProgram prog

showCurryProgram :: Prog -> String
showCurryProgram (Prog mod imports types funcs ops) =
  "module "++mod++"("++showTypeExports types++
  showFuncExports funcs++") where\n\n"++
  concatMap showInterfaceImport imports ++ "\n" ++
  concatMap showInterfaceOpDecl ops ++
  (if null ops then "" else "\n") ++
  concatMap (showCurryDataDecl (showQNameInModule mod)) types
  ++ "\n" ++
  concatMap (showCurryFuncDecl (showQNameInModule mod)
                               (showQNameInModule mod)) funcs
  ++ "\n-- end of module " ++ mod ++ "\n"

showTypeExports types = concatMap (++",") (concatMap exptype types)
 where
   exptype (Type tcons vis _ cdecls) =
     if vis==Public
     then [snd tcons++let cs = expcons cdecls in (if cs=="()" then "" else cs)]
     else []
   exptype (TypeSyn tcons vis _ _) = if vis==Public then [snd tcons] else []

   expcons cds = "(" ++ intercalate "," (concatMap expc cds) ++ ")"
   expc (Cons cname _ vis _) = if vis==Public then [snd cname] else []

showFuncExports funcs = intercalate "," (concatMap expfun funcs)
 where
   expfun (Func fname _ vis _ _) = if vis==Public then [snd fname] else []

showCurryDataDecl tt (Type tcons _ tvars constrs) =
  "data " ++ snd tcons ++ concatMap (\i->[' ',chr (97+i)]) tvars ++
  (if null constxt then "" else " = " ++ constxt)
  ++ "\n"
 where constxt = intercalate " | " (map (showCurryConsDecl tt) constrs)
showCurryDataDecl tt (TypeSyn tcons _ tvars texp) =
  "type " ++ snd tcons ++ concatMap (\i->[' ',chr (97+i)]) tvars ++
  " = " ++ showCurryType tt True texp ++ "\n"

showCurryConsDecl tt (Cons cname _ _ argtypes) =
  snd cname ++ concatMap (\t->" "++showCurryType tt True t) argtypes


-- generate function definitions:
showCurryFuncDecl tt tf (Func fname _ _ ftype frule) =
  showCurryId (snd fname) ++" :: "++ showCurryType tt False ftype ++ "\n" ++
  showCurryRule tf fname frule

showCurryRule tf fname (External _) = showCurryId (tf fname) ++ " external\n\n"
showCurryRule tf fname (Rule lhs rhs) =
  --showCurryRuleAsCase tf fname (Rule lhs rhs)
  showCurryRuleAsPatterns tf fname (Rule lhs rhs)

-- format rule as case expression:
showCurryRuleAsCase tf fname (Rule lhs rhs) =
   showCurryId (tf fname) ++ " " ++ intercalate " " (map showCurryVar lhs) ++
   " = " ++ showCurryExpr tf False 0 rhs ++ "\n\n"

-- format rule as set of pattern matching rules:
showCurryRuleAsPatterns tf fname (Rule lhs rhs) =
  concatMap (\ (l,r) -> showCurryPatternRule tf l r)
            (rule2equations (shallowPattern2Expr fname lhs) rhs)
   ++ "\n"

splitFreeVars exp = case exp of
  Free vars e -> (vars,e)
  _ -> ([],exp)

showCurryPatternRule tf l r = let (vars,e) = splitFreeVars r in
   showCurryExpr tf False 0 l ++
   showCurryCRHS tf e ++
   (if vars==[] then "" else
    " where " ++ intercalate "," (map showCurryVar vars) ++ " free")
   ++ "\n"

showCurryCRHS tf r =
   if isGuardedExpr r
   then " | " ++ showCurryCondRule r
   else " = " ++ showCurryExpr tf False 2 r
 where
   showCurryCondRule (Comb _ _ [e1,e2]) =
     showCurryExpr tf False 2 e1 ++ " = " ++ showCurryExpr tf False 4 e2


-- transform a rule consisting of a left- and a right-hand side
-- (represented as expressions) into a set of pattern matching rules:
rule2equations :: Expr -> Expr -> [(Expr,Expr)]
rule2equations lhs rhs = case rhs of
  Case Flex (Var i) bs -> caseIntoLhs lhs i bs
  Or e1 e2 -> rule2equations lhs e1 ++ rule2equations lhs e2
  _        -> [(lhs,rhs)]

caseIntoLhs _ _ [] = []
caseIntoLhs lhs vi (Branch (Pattern c vs) e : bs) =
  rule2equations (substitute [vi] [shallowPattern2Expr c vs] lhs) e
  ++ caseIntoLhs lhs vi bs
caseIntoLhs lhs vi (Branch (LPattern lit) e : bs) =
  rule2equations (substitute [vi] [Lit lit] lhs) e
  ++ caseIntoLhs lhs vi bs

shallowPattern2Expr name vars =
               Comb ConsCall name (map (\i->Var i) vars)


-- (substitute vars exps expr) = expr[vars/exps]
-- i.e., replace all occurrences of vars by corresponding exps in the
-- expression expr
substitute vars exps expr = substituteAll vars exps 0 expr

-- (substituteAll vars exps base expr):
-- substitute all occurrences of variables by corresonding expressions:
-- * substitute all occurrences of var_i by exp_i in expr
--   (if vars=[var_1,...,var_n] and exps=[exp_1,...,exp_n])
-- * substitute all other variables (Var j) by (Var (base+j))
--
-- here we assume that the new variables in guards and case patterns
-- do not occur in the list "vars" of replaced variables!

substituteAll :: [Int] -> [Expr] -> Int -> Expr -> Expr
substituteAll vars exps b (Var i) = replaceVar vars exps i
  where replaceVar [] [] var = Var (b+var)
        replaceVar (v:vs) (e:es) var = if v==var then e
                                                 else replaceVar vs es var
substituteAll _  _  _ (Lit l) = Lit l
substituteAll vs es b (Comb combtype c exps) =
                 Comb combtype c (map (substituteAll vs es b) exps)
substituteAll vs es b (Let bindings exp) =
                 Let (map (\(x,e)->(x+b,substituteAll vs es b e)) bindings)
                     (substituteAll vs es b exp)
substituteAll vs es b (Free vars e) =
                 Free (map (+b) vars) (substituteAll vs es b e)
substituteAll vs es b (Or e1 e2) =
                 Or (substituteAll vs es b e1) (substituteAll vs es b e2)
substituteAll vs es b (Case ctype e cases) =
   Case ctype (substituteAll vs es b e) (map (substituteAllCase vs es b) cases)

substituteAllCase vs es b (Branch (Pattern l pvs) e) =
                 Branch (Pattern l (map (+b) pvs)) (substituteAll vs es b e)
substituteAllCase vs es b (Branch (LPattern l) e) =
                 Branch (LPattern l) (substituteAll vs es b e)
substituteAll vs es b (Typed e t) =
                 Typed (substituteAll vs es b e) t


-- Is the expression a guarded expressions?
isGuardedExpr :: Expr -> Bool
isGuardedExpr e = case e of
  Comb _ f _ -> f == ("Prelude","cond")
  _ -> False


-------- Definition of some orderings:
leqOp (Op (_,op1) _ p1) (Op (_,op2) _ p2) = p1>p2 || p1==p2 && op1<=op2

leqType t1 t2 = (tname t1) <= (tname t2)
 where tname (Type    (_,tn) _ _ _) = tn
       tname (TypeSyn (_,tn) _ _ _) = tn

leqFunc (Func (_,f1) _ _ _ _) (Func (_,f2) _ _ _ _) = f1 <= f2


-- Examples:
-- showInterface "genint"
-- showInterface "../examples/nats"
-- showInterface "../examples/maxtree"
-- showInterface "../lib/Flat"
-- writeInterface "genint"
-- showCurryMod "genint"
-- showCurryMod "../examples/rev"

