------------------------------------------------------------------------------
--- Library to read and transform a Curry program into an equivalent
--- representation where every function gets assigned the corresponding term
--- rewriting system.
---
--- @author Jan-Hendrik Matthes
--- @version May 2016
--- @category algorithm
------------------------------------------------------------------------------

module Rewriting.Files
  ( RWData
  , emptyRWData, extendRWData, lookupRWData, readCurryProgram
  , fromFunc, fromRule, fromExpr
  , showQN, readQN
  ) where

import AbstractCurry.Files (tryReadCurryFile)
import AbstractCurry.Types
import FiniteMap (FM, emptyFM, addToFM, lookupFM)
import Rewriting.Rules (TRS, Rule)
import Rewriting.Term (Term (..), tConst)

type RWData = FM String (TRS String)

emptyRWData :: RWData
emptyRWData = emptyFM (<)

extendRWData :: RWData -> String -> TRS String-> RWData
extendRWData = addToFM

lookupRWData :: RWData -> String -> Maybe (TRS String)
lookupRWData = lookupFM

readCurryProgram :: String -> IO (Either String RWData)
readCurryProgram f = do res <- tryReadCurryFile f
                        case res of
                          (Left err) -> return (Left err)
                          (Right cp) -> return (Right (fromCurryProg cp))

fromCurryProg :: CurryProg -> RWData
fromCurryProg (CurryProg _ _ _ fs _) = extend emptyRWData (map fromFunc fs)
  where
    extend :: RWData -> [(String, TRS String)] -> RWData
    extend = foldr (\(f, trs) rwd -> extendRWData rwd f trs)

fromFunc :: CFuncDecl -> (String, TRS String)
fromFunc (CFunc f _ _ _ rs)     = (showQN f, map (fromRule (showQN f)) rs)
fromFunc (CmtFunc _ f _ _ _ rs) = (showQN f, map (fromRule (showQN f)) rs)

fromRule :: String -> CRule -> Rule String
fromRule f (CRule ps rhs) = (TermCons f (map fromPattern ps), fromRhs rhs)

fromPattern :: CPattern -> Term String
fromPattern (CPVar v)        = TermVar (fst v)
fromPattern (CPLit l)        = fromLiteral l
fromPattern (CPComb f ps)    = TermCons (showQN f) (map fromPattern ps)
fromPattern (CPAs _ _)       = error "CPAs not supported!"
fromPattern (CPFuncComb _ _) = error "CPFuncComb not supported!"
fromPattern (CPLazy _)       = error "CPLazy not supported!"
fromPattern (CPRecord _ _)   = error "CPRecord not supported!"

fromLiteral :: CLiteral -> Term String
fromLiteral (CIntc i)    = tConst ("%i." ++ show i)
fromLiteral (CFloatc f)  = tConst ("%f." ++ show f)
fromLiteral (CCharc c)   = tConst ("%c." ++ [c])
fromLiteral (CStringc s) = tConst ("%s." ++ s)

fromRhs :: CRhs -> Term String
fromRhs (CSimpleRhs expr _) = fromExpr expr
fromRhs (CGuardedRhs _ _)   = error "CGuardedRhs not supported!"

fromExpr :: CExpr -> Term String
fromExpr (CVar v)         = TermVar (fst v)
fromExpr (CLit l)         = fromLiteral l
fromExpr (CSymbol s)      = tConst (showQN s)
fromExpr (CApply f e)     = case fromExpr f of
                              TermCons n ts -> TermCons n (ts ++ [fromExpr e])
                              TermVar _     -> error "Argument is not a function!"
fromExpr (CLambda _ _)    = error "CLambda not supported!"
fromExpr (CLetDecl _ _)   = error "CLetDecl not supported!"
fromExpr (CDoExpr _)      = error "CDoExpr not supported!"
fromExpr (CListComp _ _)  = error "CListComp not supported!"
fromExpr (CCase _ _ _)    = error "CCase not supported!"
fromExpr (CTyped _ _)     = error "CTyped not supported!"
fromExpr (CRecConstr _ _) = error "CRecConstr not supported!"
fromExpr (CRecUpdate _ _) = error "CRecUpdate not supported!"

--- Shows a qualified name as a string.
showQN :: QName -> String
showQN (qn,fn) = qn ++ "." ++ fn

--- Reads a qualified name string back into a QName.
readQN :: String -> QName
readQN s = let (rfn,rqn) = break (=='.') (reverse s)
            in (reverse (tail rqn), reverse rfn)
