------------------------------------------------------------------------------
--- Module defining main operations to transform programs.
--- 
--- @author Lasse Folger (with changes by Michael Hanus)
--- @version June 2015
------------------------------------------------------------------------------

import Reduction
import AbstractCurry
import AbstractCurryGoodies
import PrettyAbstract
import Translation
import Selection
import System

{-In diesem Modul sind die Hauptfunktionalitäten der anderen Module zusammen geführt.
Zudem findet hier die Umbenennung des Moduls statt, d.h. alle qualifizierten Namen werden,
sofern nötig, umbenannt. Es wird mittel der Kommandozeilen Argumente ein Modul eingelesen,
die Umbenennung durchgeführt, anschließend die Übersetzung und zuletzt wird das Programm
mit dem Prettyprinter wieder ausgegeben. 
-}


main :: Prelude.IO ()
main = do
  args <- getArgs 
  applySequential args

--Hier wird das zu übersetzende Modul eingelesen weitergerecht und das übersetze
--Modul wieder ausgegeben.
applySequential :: [String] -> IO ()
applySequential args = do
        inputProg <- (AbstractCurry.readCurry input)
        writeFile (outputName ++ ".curry") (showCProg (translate inputProg outputName))
  where input                     = head args
        outputName                = head (tail args)        

--Die Funktion ist für die Übersetzung des Moduls zuständig, dazu zählt zunächst
--die Umbenennung, dann die drei Schritte der Übersetzung, Selektion, Reduktion und
--die eigentlich Übersetzung.         
translate :: CurryProg -> String -> CurryProg
translate inputProg outputName = outputProg
  where (CurryProg a b c funcs d) = inputProg
        (det,ndet)                = filterFunc newfuncs
        simpleProg                = Reduction.newprog (CurryProg a b c ndet d)
        (CurryProg a' b' _ d' e)  = Translation.newprog simpleProg outputName
        outputProg                = CurryProg a' b' newtypes (d' ++ det) e
        newtypes                  = renameT a outputName c 
        newfuncs                  = renameF a outputName funcs

--Eine Funktionsliste wird aufgeteilt in deterministische und nicht-deterministische
--Funktionen.
filterFunc :: [CFuncDecl] -> ([CFuncDecl],[CFuncDecl])
filterFunc x = filterFunc' (reverse x) ([],[])
  where filterFunc' [] ft        = ft
        filterFunc' (f:fs) (d,nd)
          | isnondeterministic f = filterFunc' fs (d,f:nd)
          | otherwise            = filterFunc' fs (f:d,nd)

--Funktionen die durch alle Strukturen des Moduls laufen 
--und qualifizierte Namen ersetzen
renameT :: String -> String -> [CTypeDecl] -> [CTypeDecl]
renameT iname oname x = map renameTD' x
  where rename' n                    = rename iname oname n
        renameTD' (CType n a b c)    = CType (rename' n) a b (map renameC c)
        renameTD' (CTypeSyn n a b t) = CTypeSyn (rename' n) a b (renameTE t)
        renameTD' (CNewType n a b t) = CNewType (rename' n) a b (renameC t)
        renameC (CCons n v t)        = CCons (rename' n) v (map renameTE t)
        renameC (CRecord n v fs)     = CRecord (rename' n) v (map renameFD fs)
        renameFD (CField n v te)     = CField (rename' n) v (renameTE te)
        renameTE v@(CTVar _)         = v
        renameTE (CFuncType i o)     = CFuncType (renameTE i) (renameTE o)
        renameTE (CTCons n t)        = CTCons (rename' n) (map renameTE t)
        renameRec t                  = map renameRec' t
        renameRec' (n,te)            = (n, renameTE te)


rename :: String -> String -> (String,String) -> (String,String)
rename iname oname (a,b) = if a == iname then (oname,b) else (a,b)

renameF :: String -> String -> [CFuncDecl] -> [CFuncDecl]
renameF iname oname fl = map renameF' fl
  where rename' n                       = rename iname oname n
        renameF' (CFunc n a v te r)     = CFunc (rename' n) a v (renameTE te) (map renameR r)
        renameF' (CmtFunc c n a v te r) = CmtFunc c (rename' n) a v (renameTE te) (map renameR r) 
        renameTE te = case te of
                        (CTVar _)       -> te
                        (CFuncType i o)   -> CFuncType (renameTE i) (renameTE o)
                        (CTCons n t)      -> CTCons (rename' n) (map renameTE t)
        renameRec t                     = map renameRec' t
        renameRec' (n,te)               = (n, renameTE te)
        renameR (CRule p rhs)          = CRule (renameP p) (renameRhs rhs)
        renameRhs (CSimpleRhs exp ld) = CSimpleRhs  (renameE exp) (renameLD ld)
        renameRhs (CGuardedRhs gs ld) = CGuardedRhs (renameG gs)  (renameLD ld)
        renameP p                       = map renameP' p
        renameP' pat           = case pat of
                                   (CPComb n pa)     -> CPComb (rename' n) (renameP pa)
                                   (CPAs id p)       -> CPAs id (renameP' p)
                                   (CPFuncComb n pa) -> CPFuncComb (rename' n) (renameP pa)
                                   (CPLazy p)        -> CPLazy (renameP' p)
                                   (CPRecord m t)    -> CPRecord m (map renamePRec t)
                                   _                 -> pat
        renamePRec (n,te)              = (n, renameP' te)
        renameG x                       = map renameG' x
        renameG' (e1,e2)                = (renameE e1, renameE e2)
        renameE exp            = case exp of
                                   (CVar _)          -> exp
                                   (CLit _)          -> exp
                                   (CSymbol n)       -> CSymbol (rename' n)
                                   (CApply e1 e2)    -> CApply (renameE e1) (renameE e2)
                                   (CLambda pa e)    -> CLambda (renameP pa) (renameE e)
                                   (CLetDecl ld e)   -> CLetDecl (renameLD ld) (renameE e)
                                   (CDoExpr s)       -> CDoExpr (renameS s)
                                   (CListComp e s)   -> CListComp (renameE e) (renameS s)
                                   (CCase ct e b)    -> CCase ct (renameE e) (map renameB b)
                                   (CTyped e t)      -> CTyped (renameE e) (renameTE t)
                                   (CRecConstr n re) -> CRecConstr (rename' n) (renameRC re)
                                   (CRecUpdate e re) -> CRecUpdate (renameE e) (renameRC re)
        renameLD x                      = map renameLD' x
        renameLD' locd = case locd of
                           (CLocalFunc fd)    -> CLocalFunc (renameF' fd)
                           (CLocalPat p rhs)  -> CLocalPat (renameP' p) (renameRhs rhs)
                           (CLocalVars _)     -> locd
        renameS x                       = map renameS' x
        renameS' sta = case sta of
                        (CSExpr e)  -> CSExpr (renameE e)
                        (CSPat p e) -> CSPat (renameP' p) (renameE e)
                        (CSLet ld)  -> CSLet (renameLD ld)
        renameB (p,rhs) = (renameP' p, renameRhs rhs)
        renameRC x                      = map renameRC' x
        renameRC' (s,e)                 = (rename' s, renameE e)





