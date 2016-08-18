------------------------------------------------------------------------------
--- Library to read and transform a curry program into an equivalent
--- representation where every function gets assigned the corresponding term
--- rewriting system.
---
--- @author Jan-Hendrik Matthes
--- @version May 2016
--- @category algorithm
------------------------------------------------------------------------------

module Rewriting.Files
  ( ConsData, TRSData, RWData
  , emptyConsData, extendConsData, lookupConsData, emptyTRSData
  , extendTRSData, lookupTRSData, readCurryProgram, fromCurryProg
  , fromTypeDecl, fromConsDecl, fromFuncDecl, fromRule, fromPattern
  , fromLiteral, fromRhs, fromExpr, showQName, readQName
  ) where

import AbstractCurry.Files (tryReadCurryFile)
import AbstractCurry.Types
import FiniteMap (FM, emptyFM, addToFM, lookupFM)
import Rewriting.Rules (Rule, TRS)
import Rewriting.Substitution (Subst, emptySubst, extendSubst, applySubst)
import Rewriting.Term (Term (..), tConst)

type ConsData = FM QName QName

type TRSData = FM QName (TRS QName)

type RWData = (TRSData, ConsData)

emptyConsData :: ConsData
emptyConsData = emptyFM (<)

extendConsData :: ConsData -> QName -> QName -> ConsData
extendConsData = addToFM

lookupConsData :: ConsData -> QName -> Maybe QName
lookupConsData = lookupFM

emptyTRSData :: TRSData
emptyTRSData = emptyFM (<)

extendTRSData :: TRSData -> QName -> TRS QName -> TRSData
extendTRSData = addToFM

lookupTRSData :: TRSData -> QName -> Maybe (TRS QName)
lookupTRSData = lookupFM

readCurryProgram :: String -> IO (Either String RWData)
readCurryProgram f = do res <- tryReadCurryFile f
                        case res of
                          (Left err) -> return (Left err)
                          (Right cp) -> return (Right (fromCurryProg cp))

fromCurryProg :: CurryProg -> RWData
fromCurryProg (CurryProg _ _ ts fs _) = (trsData, consData)
  where
    extendTRS :: TRSData -> [(QName, TRS QName)] -> TRSData
    extendTRS = foldr (\(fn, trs) trsd -> extendTRSData trsd fn trs)
    trsData = extendTRS emptyTRSData (map fromFuncDecl fs)
    extendCons :: ConsData -> [(QName, QName)] -> ConsData
    extendCons = foldr (\(cn, tn) consd -> extendConsData consd cn tn)
    consData = extendCons emptyConsData (concatMap fromTypeDecl ts)

fromTypeDecl :: CTypeDecl -> [(QName, QName)]
fromTypeDecl (CType tn _ _ cs)   = map (fromConsDecl tn) cs
fromTypeDecl (CTypeSyn _ _ _ _)  = []
fromTypeDecl (CNewType tn _ _ c) = [fromConsDecl tn c]

fromConsDecl :: QName -> CConsDecl -> (QName, QName)
fromConsDecl tn (CCons cn _ _)   = (cn, tn)
fromConsDecl tn (CRecord cn _ _) = (cn, tn)

fromFuncDecl :: CFuncDecl -> (QName, TRS QName)
fromFuncDecl (CFunc fn _ _ _ rs)     = (fn, map (fromRule fn) rs)
fromFuncDecl (CmtFunc _ fn _ _ _ rs) = (fn, map (fromRule fn) rs)

fromRule :: QName -> CRule -> Rule QName
fromRule fn (CRule ps rhs) = (TermCons fn (map fromPattern ps), rTerm)
  where
    rTerm = applySubst (asSubst ps) (fromRhs rhs)

asSubst :: [CPattern] -> Subst QName
asSubst = extend emptySubst
  where
    extend :: Subst QName -> [CPattern] -> Subst QName
    extend s []     = s
    extend s (p:ps)
      = case p of
          (CPAs v x) -> extend (extendSubst s (fst v) (fromPattern x)) ps
          _          -> extend s ps

fromPattern :: CPattern -> Term QName
fromPattern (CPVar v)          = TermVar (fst v)
fromPattern (CPLit l)          = fromLiteral l
fromPattern (CPComb fn ps)     = TermCons fn (map fromPattern ps)
fromPattern (CPAs _ p)         = fromPattern p
fromPattern (CPFuncComb fn ps) = TermCons fn (map fromPattern ps)
fromPattern (CPLazy p)         = fromPattern p
fromPattern (CPRecord _ _)     = error "CPRecord not supported!"

fromLiteral :: CLiteral -> Term QName
fromLiteral (CIntc i)    = tConst ("%i", show i)
fromLiteral (CFloatc f)  = tConst ("%f", show f)
fromLiteral (CCharc c)   = tConst ("%c", [c])
fromLiteral (CStringc s) = tConst ("%s", s)

fromRhs :: CRhs -> Term QName
fromRhs (CSimpleRhs expr _) = fromExpr expr
fromRhs (CGuardedRhs _ _)   = error "CGuardedRhs not supported!"

fromExpr :: CExpr -> Term QName
fromExpr (CVar v)         = TermVar (fst v)
fromExpr (CLit l)         = fromLiteral l
fromExpr (CSymbol s)      = tConst s
fromExpr (CApply f e)
  = case fromExpr f of
      TermCons c ts -> TermCons c (ts ++ [fromExpr e])
      TermVar _     -> error "Argument is not a function!"
fromExpr (CLambda _ _)    = error "CLambda not supported!"
fromExpr (CLetDecl _ _)   = error "CLetDecl not supported!"
fromExpr (CDoExpr _)      = error "CDoExpr not supported!"
fromExpr (CListComp _ _)  = error "CListComp not supported!"
fromExpr (CCase _ _ _)    = error "CCase not supported!"
fromExpr (CTyped _ _)     = error "CTyped not supported!"
fromExpr (CRecConstr _ _) = error "CRecConstr not supported!"
fromExpr (CRecUpdate _ _) = error "CRecUpdate not supported!"

--- Transforms a qualified name into a string representation.
showQName :: QName -> String
showQName (qn, fn) = if qn == ""
                       then fn
                       else qn ++ "." ++ fn

--- Transforms a string into a qualified name.
readQName :: String -> QName
readQName s = case break (== '.') s of
                q@(_, [])  -> q
                (qn, _:fn) -> (qn, fn)