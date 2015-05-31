------------------------------------------------------------------------------
--- Module defining operations to translate rules into rules
--- implementing a sequential rule selection strategy.
--- 
--- @author Lasse Folger (with changes by Michael Hanus)
--- @version June 2015
------------------------------------------------------------------------------

module Translation where

import AbstractCurry
import AbstractCurryGoodies
import AbstractCurryPrinter
import VariableGenerator
import SetFunctions

{-Dieses Modul beinhaltet alle Funktion, die für die eigentliche Uebersetzung
in sequenziellen Patternmatching benötigt werden. Es werden alle Regeln zu einer
zusammen gefasst und die Struktur der Regeln sind geschachtelte if-then-else 
Ausdrücke. 
-}

--Übersetzt alle Funktionen eines Moduls
newprog:: CurryProg -> String -> CurryProg
newprog (CurryProg _ b c fl d) output = CurryProg output newimp c newFL d
  where newFL  = (replaceRules fl output)
        newimp = if (elem "SetFunctions" b) 
                    then b
                    else ("SetFunctions" : b) 

--Übersetzt eine Liste von Funktionen
replaceRules:: [CFuncDecl] -> String -> [CFuncDecl]
replaceRules funclist name = map redefineRule funclist
 where
  redefineRule (CFunc     (_,b) c d e rules) =
    CFunc (name,b) c d e [transMRules rules name e]
  redefineRule (CmtFunc a (_,b) c d e rules) =
    CmtFunc a (name,b) c d e [transMRules rules name e]

--Die zuvor entwickelte Struktur der Regel wird zusammengefasst und so eine einzelne uebersetzte Regel,
--damit diese korrekt funktioniert werden unbenutzte Variablen erzeugt, die als neues Pattern dienen.
transSRule :: [CPattern] -> [([CPattern],CExpr,CExpr,[CLocalDecl])]
           -> String -> [CVarIName] -> CTypeExpr -> CRule
transSRule p gs name nv te = CRule p (CSimpleRhs ne nld)
  where (ne,nld)           = buildexp cenames gs name nv te       
        cenames            = filter (notElem' usednames) potnames 
        potnames           = zip ["cond"++(show n) | n <- [1..]] ["expr"++(show n) | n <- [1..]]
        notElem' x (s1,s2) = not (elem s1 x) && not (elem s2 x)
        usednames          = getNames gs

--Diese Funktion erzeugt die lokalen Deklaration, sowie die verschachtelung der if-then-else Ausdruecke.
buildexp :: [(String,String)] -> [([CPattern],CExpr,CExpr,[CLocalDecl])] -> String -> [CVarIName] -> CTypeExpr -> (CExpr,[CLocalDecl])
buildexp _ [] _ _ _     = (constF (pre "failed"),[])
buildexp ((cn,en):ns) ((ps,c,e,ld):rs) mN nv te  
  | alwaysTrue c && all isVar ps = (doexpr,[exloc])
  | otherwise = (applyF (pre "if_then_else") [check, doexpr, re], [cloc, exloc] ++ rloc)
  where isVar p         = case p of 
                           (CPVar _) -> True
                           _         -> False
        (re,rloc)       = buildexp ns rs mN nv te
        cte             = redefte te
        ld'             = removedouble ld ps        
        redefte texpr   = case texpr of
                           (CFuncType x y) -> CFuncType x (redefte y) 
                           _               -> (CTCons (pre "Success") []) 
        cloc            = CLocalFunc (cfunc (mN,cn) arity Private cte clocrule)
        clocrule        = [guardedRule ps [(c,(CSymbol (pre "success")))] ld'] 
        exloc           = CLocalFunc (cfunc (mN,en) arity Private te explocrule)
        explocrule      = [guardedRule ps [(c,e)] ld']
        newvars []      = []
        newvars (x:xs)  = (CVar x) : (newvars xs)
        nva             = newvars nv
        doexpr          = applyF (mN,en) nva        
        arity           = length ps         
        check           = applyF ("SetFunctions","notEmpty") [check']
        check' 
          | arity == 0  = applyF ("SetFunctions","set0") [constF (mN,cn)]
          | arity <= 7  = applyF ("SetFunctions","set" ++ (show(length ps))) ([constF (mN,cn)] ++ nva)
          | otherwise   = error "only functions with an arity with 7 or less are supported"

--Hilfsfunktion, um Namenskonflikte (zwischen Pattern
--und freien Variablen) zu vermeiden
removedouble :: [CLocalDecl] -> [CPattern] -> [CLocalDecl]
removedouble ld ps = filter (notin patvars) ld
  where patvars         = getPVars ps
        getPVars []     = []
        getPVars (x:xs) = (getPVars' x) ++ (getPVars xs)
        getPVars' pat   = case pat of
                            (CPVar (_,n)) -> [n]
                            _             -> []
        notin pl locd   = case locd of
                            (CLocalVars lvars) -> all (`notElem` pl) (map snd lvars) --notElem n pl
                            _                  -> True

--Erzeugt bei Bedarf eine Abstractcurry LetDecl
letDecl :: [CLocalDecl] -> CExpr -> CExpr
letDecl l e
   | null l    = e
   | otherwise = CLetDecl l e

--Ueberpruefung, ob ein Ausdruck immer wahr ist
alwaysTrue :: CExpr -> Bool
alwaysTrue c = (c == (CSymbol (pre "success"))
               || c == (CSymbol (pre "otherwise")) 
               || c == (CSymbol (pre "True"))) 

--bereitet die Uebersetzung mehrerer Regeln für eine Funktion
--vor, indem eine einheitliche Struktur erzeugt wird und an
--transSRule delegiert wird
transMRules ::  [CRule] -> String -> CTypeExpr -> CRule
transMRules rs name te = transSRule newPat newGuards name newVars te
  where (CRule ps _)        = head rs
        newVars             = take (length ps) (VariableGenerator.varsL rs)
        newPat              = generateP newVars
        newGuards           = gatherGuards rs
        gatherGuards []     = []
        gatherGuards (x:xs) = (gather x) ++ (gatherGuards xs)

--erzeugt aus Variablennamen Patternvariablen 
generateP :: [CVarIName] -> [CPattern]
generateP []     = []
generateP (x:xs) = (CPVar x) : (generateP xs)

--Die Darstellung der Regel wird an dieser Stelle für spaetere
--Umformungen angepasst. 
gather :: CRule -> [([CPattern],CExpr,CExpr,[CLocalDecl])]        
gather (CRule p (CSimpleRhs  e ld)) = [(p,CSymbol (pre "success"),e,ld)]
gather (CRule p (CGuardedRhs gs ld)) = build gs
  where build []         = []
        build ((c,e):xs)
          | alwaysTrue c = [(p,CSymbol (pre "success"),e,ld)]
          | otherwise     = (p,c,e,ld) : (build xs)

--Sucht alle benutzten Namen innerhalt einer Regel (veraenderte darstellung)
getNames :: [([CPattern],CExpr,CExpr,[CLocalDecl])] -> [String]
getNames []             = []
getNames ((p,g,e,l):rs) = 
  getPNames p ++ getENames g ++ getENames e ++ getLNames l ++ getNames rs
  where getPNames []            = []
        getPNames (pa:pas)      = (getPNames' pa) ++ (getPNames pas)
        getPNames' pa           = case pa of
          (CPVar (_,n))         -> [n]
          (CPComb (_,n) ps)     -> n : (getPNames ps)
          (CPAs (_,n) pat)      -> n : (getPNames' pat)
          (CPFuncComb (_,n) ps) -> n : (getPNames ps)
          (CPLazy pat)          -> (getPNames' pat)
          _                     -> []
        getENames expr          = case expr of
          (CVar (_,n))          -> [n]
          (CSymbol (_,n))       -> [n]
          (CApply e1 e2)        -> (getENames e1) ++ (getENames e2)
          (CLambda ps e1)       -> (getPNames ps) ++ (getENames e1)
          (CLetDecl ld e1)      -> (getLNames ld) ++ (getENames e1)
          (CDoExpr sl)          -> getSNames sl
          (CListComp e1 sl)     -> (getENames e1) ++ (getSNames sl)
          (CCase _ e1 be)       -> (getENames e1) ++ (getBNames be)
          _                     -> []
        getLNames []            = []
        getLNames (lo:los)      = (getLNames' lo) ++ (getLNames los)
        getLNames' lo           = case lo of
          (CLocalFunc fd)       -> getFNames fd
          (CLocalPat pa rhs)    -> getPNames' pa ++ getRhsNames rhs
          (CLocalVars lvars)    -> map snd lvars
        getSNames []            = []
        getSNames (s:sl)        = (getSNames' s) ++ (getSNames sl)        
        getSNames' s            = case s of
          (CSExpr e1)           -> getENames e1
          (CSPat pa e1)         -> (getPNames' pa) ++ (getENames e1)
          (CSLet ld)            -> getLNames ld
        getBNames []            = []
        getBNames ((pa,e1):bs)  = getPNames' pa ++ getRhsNames e1 ++ getBNames bs

        getGNames []            = []
        getGNames ((g1,e1):gs)  = (getENames g1) ++ (getENames e1) ++ (getGNames gs)    
        getFNames f             = case f of
          (CFunc (_,n) _ _ _ r)    -> n : (getRSNames r)
          (CmtFunc _ (_,n) _ _ _r) -> n : (getRSNames r)

        getRhsNames (CSimpleRhs  re ls) = getENames re ++ getLNames ls
        getRhsNames (CGuardedRhs gs ls) = getGNames gs ++ getLNames ls

        getRSNames []           = []
        getRSNames (ru:rus)     = (getRSNames' ru) ++ (getRSNames rus)
        getRSNames' (CRule pas rhs) = getPNames pas ++ getRhsNames rhs













