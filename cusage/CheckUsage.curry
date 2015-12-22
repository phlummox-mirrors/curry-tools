---------------------------------------------------------------------------
--- Set functions are intended to exist for every top-level function.
--- This module checks whether there are unintended uses of set funtions
--- defined in the module `SetFunctions` in a module.
--- Furthermore, it checks whether internal operations like
--- `Prelude.=:<=` or `Prelude.prim_` are used.
---
--- See example module `TestUsage.curry` for some examples.
---
--- @author Michael Hanus
--- @version December 2015
---------------------------------------------------------------------------

import qualified AbstractCurry.Types as AC
import AbstractCurry.Files (readCurry)
import AbstractCurryMatch
import Char(isDigit)
import Distribution(stripCurrySuffix)
import FlatCurry.Types
import FlatCurry.Files
import FlatCurryMatch
import List(intercalate)
import Read(readNat)
import SetFunctions
import System(getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [modname] -> checkModule (stripCurrySuffix modname)
    _         -> putStrLn $ unlines
                   [title
                   ,"ERROR: Illegal arguments for cusage: " ++
                    intercalate " " args
                   ,"Usage: cusage <module_name>"
                   ]

title :: String
title = "cusage - A tool to check for intended uses of Curry features"

-------------------------------------------------------------------------
-- Main function to inspect a module for unintended uses of set functions.
-- Try: checkModule "TestUsage"
checkModule :: String -> IO ()
checkModule modname = do
  AC.CurryProg _ _ _ cfdecls _ <- readCurry modname
  blerrors <- values2list (set1 blacklistUsage cfdecls)
  putStr (unlines (map showBlacklistError blerrors))
  Prog _ _ _ fdecls _ <- readFlatCurry modname
  seterrors <- values2list (set1 setUse fdecls)
  putStr (unlines (map showSetError seterrors))
 where
  showBlacklistError ((_,n),(q,f)) =
    "Function '" ++ n ++ "': usage of '" ++ q++"."++f ++ "' not allowed!"

  showSetError ((_,n),sar) =
    "Function '" ++ n ++ "': wrong use of set function 'set" ++ sar ++ "'"

---------------------------------------------------------------------
--- Returns some unintended use of a set function occurring in a list
--- of function declarations. The name of the function together with
--- the arity of the set function used is returned.
--- Set functions are intended to be used only on top-level functions
--- with the right arity.
---
--- To provide a simple implementation, we exploit functional patterns
--- with the function `funWithExp`.
setUse :: [FuncDecl] -> (QName, String)
--setUse (_ ++ [funWithExp qf (Comb ct ("SetFunctions","set"++n) args)] ++ _)
setUse (_++ [funWithinExp qf _ _ (Comb ct ("SetFunctions","set"++n) args)] ++_)
  | not (validSetFunCall ct n args) = (qf,n)

--- Checks whether an application of a set function is as intended.
validSetFunCall :: CombType -> String -> [Expr] -> Bool
validSetFunCall ct n args
  | ct==FuncCall && all isDigit n && not (null args)
  = if arity==0 then isFuncCall (head args)
                else isFuncPartCall arity (head args)
 where
  arity = readNat n

isFuncCall :: Expr -> Bool
isFuncCall e = case e of
  Comb FuncCall qf [] -> isID qf
  _                   -> False

isFuncPartCall :: Int -> Expr -> Bool
isFuncPartCall n e = case e of
  Comb (FuncPartCall p) qf _ -> p==n && isID qf
  _                          -> False

isID :: QName -> Bool
isID (_,n) = all (`elem` infixIDs) n || '.' `notElem` n
 where
  infixIDs :: String
  infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

---------------------------------------------------------------------
---------------------------------------------------------------------
--- Returns some use of a black-listed operation occurring in a list
--- of function declarations. The name of the defined function together with
--- the black-listed operation is returned.
---
--- To provide a simple implementation, we exploit functional patterns
--- with the function `funWithExp`.
---
--- TODO: check also occurrences in functional patterns
blacklistUsage :: [AC.CFuncDecl] -> (AC.QName, AC.QName)
blacklistUsage (_ ++ [cfunWithExp qf (AC.CSymbol qop)] ++ _)
  | isBlacklistedOperation qop
  = (qf,qop)

isBlacklistedOperation :: AC.QName -> Bool
isBlacklistedOperation (q,f) =
  (q == pre && take 5 f == "prim_") -- no direct call to primitive ops
  || (q,f) `elem` [(pre,"=:<="),(pre,"=:<<=")]

pre :: String
pre = "Prelude"

---------------------------------------------------------------------
