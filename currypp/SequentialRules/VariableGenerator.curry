------------------------------------------------------------------------------
--- Module with operations to generate unused variables.
--- 
--- @author Lasse Folger (with changes by Michael Hanus)
--- @version June 2015
------------------------------------------------------------------------------

module VariableGenerator(vars,varsL) where

import AbstractCurry
import List


--gathers the used variablenames        
varsInRule:: CRule -> [String]
varsInRule rule = nub (getRVars rule)
  where getPVars []             = []
        getPVars (x:xs)         = (getPVars' x) ++ (getPVars xs)
        getPVars' p             = case p of
                                   (CPVar (_,n))     -> [n]
                                   (CPFuncComb _ pl) -> getPVars pl
                                   (CPComb _ pl)     -> getPVars pl
                                   (CPAs (_,n) pa)   -> n : (getPVars' pa)
                                   (CPLazy lp)       -> getPVars' lp
                                   _                 -> []
        getGVars []             = []
        getGVars ((x,y):xs)     = getEVars x ++ getEVars y ++ getGVars xs

        getRhsVars (CSimpleRhs  rhs ldecls) = getEVars rhs ++ getLVars ldecls
        getRhsVars (CGuardedRhs gs  ldecls) = getGVars gs  ++ getLVars ldecls
        
        getEVars e              = case e of
                                   (CVar (_,n))         -> [n]
                                   (CApply _ ae)        -> getEVars ae
                                   (CLambda pl le)      -> (getPVars pl) ++ (getEVars le)
                                   (CLetDecl ld le)     -> (getLVars ld) ++ (getEVars le)
                                   -- nur in main? (CDoExpr _) -> ?
                                   (CListComp le sl)    -> (getEVars le) ++ (getSVars sl)
                                   (CCase _ ce bl)      -> (getEVars ce) ++ (getBVars bl)
                                   _                    -> []
        getSVars []             = []
        getSVars (x:xs)         = (getSVars' x) ++ (getSVars xs) 
        getSVars' s             = case s of
                                   (CSExpr e)  -> getEVars e
                                   (CSPat p e) -> (getPVars' p) ++  (getEVars e)
                                   (CSLet ld)  -> getLVars ld
        getBVars []             = []
        getBVars ((p,rhs):xs)   = getPVars' p ++ getRhsVars rhs ++ getBVars xs

        getLVars []             = []
        getLVars (x:xs)         = getLVars' x ++ getLVars xs
        getLVars' ld            = case ld of
                                   (CLocalFunc f)     -> getFVars f
                                   (CLocalPat p rhs)  -> getPVars' p ++ getRhsVars rhs
                                   (CLocalVars lvars) -> map snd lvars
                                   
        getFVars (CFunc _ _ _ _ r)     = concatMap getRVars r
        getFVars (CmtFunc _ _ _ _ _ r) = concatMap getRVars r

        getRVars (CRule pats rhs)  = getPVars pats ++ getRhsVars rhs

--determines unused variables for a given rule
vars :: CRule -> [CVarIName]
vars r = zip [-1,-2..] newVars
 where newVars           = filter (notElem' usedVars) ['x':(show n) | n <- [1..]]
       usedVars          = varsInRule r
       notElem' x a      = not (elem a x)

varsL :: [CRule] -> [CVarIName]
varsL rs = zip [-1,-2..] newVars
  where newVars          = filter (notElem' usedVars) ['x':(show n) | n <- [1..]]
        usedVars         = usedVars' rs        
        notElem' x a     = not (elem a x)
        usedVars' []     = []
        usedVars' (x:xs) = (varsInRule x) ++ (usedVars' xs)

