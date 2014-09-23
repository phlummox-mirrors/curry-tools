------------------------------------------------------------------------------
--- A generator for XML data conversion.
---
--- If this program is applied to some Curry module,
--- it generates a new Curry module containing conversion functions
--- from and to an XML representation for all data types declared
--- in this module.
---
--- For instance, if `Nat` is a module containing the declaration
---
---     data Nat = Z | S Nat
---
--- applying this program to `Nat` generates a new module `NatDataToXml`
--- containing the implementation of the following operations:
---
---     natToXml :: Nat -> XmlExp
---     xmlToNat :: XmlExp -> Nat
---
--- Hence, one can store a `Nat` term `num` into the file `Nat.xml` by
---
---     writeXmlFile "Nat.xml" (natToXml num)
---
--- provided that the module `XML` is imported. Similarly, one can read
--- the data from this file by
---
---     readXmlFile "Nat.xml" >>= return . xmlToNat
---
--- @author Bernd Brassel, Michael Hanus
--- @version September 2014
------------------------------------------------------------------------------

module Data2Xml where

import AbstractCurry
import AbstractGoodies
import AbstractCurryPrinter
import Char
import FileGoodies
import List
import System
 
data Options = LowCase | FileName String

main = do
  args <- getArgs
  let opts = reverse (argsToOptions args)
  if null opts then putStrLn usageMsg
               else derive opts

usageMsg = "Usage: data2xml [-low] <filename>\n"++
           "Options:\n" ++
           "-low: make all tags lowercase"

argsToOptions args = case args of
  "-low":opts -> LowCase : argsToOptions opts
  [s]         -> [FileName (stripSuffix s)]
  _           -> []

derive (FileName fn:opts) = do
  CurryProg modName  _ ts _ _ <- readCurry fn
  let (specials,types) = if isPrelude modName 
                           then (specialFuncs opts,filterSpecials ts)
                           else ([],ts)
      progName = transModName fn
      impTypes = maybeString $ nub $ filter ((/=modName) .fst)
                             $ concatMap requiredTypesTypeDecl types
  imports <- importTypes modName impTypes
  writeFile (progName++".curry") $ showProg $
   CurryProg progName (nub $ ["XML",fn] ++ imports) [] 
             (map (mkType2Xml opts) types ++
              map (mkXml2Type opts) types ++ specials)
             []
  putStrLn ("You can now import "++progName)

maybeString xs 
  = if not (elem (p "String") xs) && elem (p "[]") xs && elem (p "Char") xs
       then (p "String":xs) else xs
  where
    p s = ("Prelude",s)

-- Transform original module name into name of the transformation module:
transModName mn = mn ++ "DataToXml"

----------------------------
-- naming the new functions
----------------------------

toXmlName (m,s) = case (isPrelude m,s,isTupleName s) of 
  (True,"[]",_)  -> (nm,"list_To_Xml")
  (True,"()",_)  -> (nm,"unitToXml")
  (True,_,True)  -> (nm,"tuple"++show (length s-1)++"ToXml")
  (_,c:cs,_)     -> (nm,toLower c:cs ++ "ToXml")
 where nm = transModName m
                  
 
fromXmlName (m,s) = case (isPrelude m,s,isTupleName s) of
  (True,"[]",_)  -> (nm,"xml_To_List")
  (True,"()",_)  -> (nm,"xmlToUnit")
  (True,_,True)  -> (nm,"xmlToTuple"++show (length s-1))
  _              -> (nm,"xmlTo"++s)
 where nm = transModName m
                 

listTag opts = tag opts "List"

isTupleName (n:name) = n=='(' && isTuple name
  where
    isTuple ""  = False
    isTuple ")" = True
    isTuple (c:c':cs) = c==',' && isTuple (c':cs)
    
----------------------------
-- generating tags
----------------------------

tag opts s = if elem LowCase opts then map toLower s else s

tagNameForCons (mname,cname)
  | isTupleName cname = "Tuple" ++ show (length cname - 1)
  | mname=="Prelude"  = cname
  | otherwise         = mname++"_"++cname

-------------------------------------------------
-- make functions to transform data terms to xml
-------------------------------------------------
 
mkType2Xml _ (CTypeSyn name vis vars t) 
  = CFunc (toXmlName name) 1 vis
          (CFuncType (CTCons name (map CTVar vars)) xmlType)
          (CRules CFlex [CRule [CPVar (0,"x0")] 
                               [noGuard (call2xml (t,0))] []])
mkType2Xml opts (CType name vis vars cs) 
  = CFunc (toXmlName name) (1+length vars) vis
          (type2XmlType vars 
            (CFuncType (CTCons name (map CTVar vars)) xmlType))
          (CRules CFlex (map (mkConsDecl2Xml opts $ 
                                map (CPVar . renVar) vars) cs))
  
mkConsDecl2Xml opts patVars (CCons name arity _ args)
  = CRule (newPatVars++[CPComb name (pVars arity)] )
          [noGuard 
             (xml opts (tagNameForCons name) []
                  (map call2xml (zip args [0..])))] []
  where
    newPatVars = renameUnused (map renVar $ concatMap allTVars args) patVars

type2XmlType vars t 
  = foldr CFuncType t (map (\x->CFuncType (CTVar x) xmlType) vars)  

call2xml (t,i) = CApply (call2xmlType t) (toVar i)

call2xmlType (CTVar v) = CVar (renVar v)
call2xmlType (CTCons name args) 
  | snd name == "[]" &&  args==[CTCons ("Prelude","Char") []]
  = sym $ toXmlName ("Prelude","String")
  | otherwise 
  = app (CSymbol $ toXmlName name) (map call2xmlType args)
call2xmlType (CFuncType _ _) = error "unable to transform functions to XML"

xml opts name attrs elems 
  = app (sym ("XML","XElem")) [cString (tag opts name),cList attrs,cList elems]

xmlType = CTCons ("XML","XmlExp") []

-------------------------------------------------
-- make functions to transform xml to data terms 
-------------------------------------------------

mkXml2Type _ (CTypeSyn name vis vars t) 
  = CFunc (fromXmlName name) 1 vis
          (CFuncType xmlType (CTCons name (map CTVar vars)))
          (CRules CFlex [CRule [CPVar (0,"x0")] 
                               [noGuard (callXml2 (t,0))] []])
mkXml2Type opts (CType name vis vars cs) 
  = CFunc (fromXmlName name) (1+length vars) vis
          (xml2typeType vars 
             (CFuncType xmlType (CTCons name (map CTVar vars))))
          (CRules CFlex (map (mkXml2ConsDecl opts $ map (CPVar . renVar) vars) cs))
  
renVar (i,s) = case s of
                ('x':xs) -> (i,'t':xs)
                _ -> (i,s)

xml2typeType vars t 
  = foldr CFuncType t (map (\x->CFuncType xmlType (CTVar x)) vars)  

mkXml2ConsDecl opts patVars (CCons name arity _ args)
  = CRule (newPatVars++[pxml opts (tagNameForCons name) [] (pVars arity)])
          [noGuard (app (sym name) (map callXml2 (zip args [0..])))] []
  where
    newPatVars = renameUnused (map renVar $ concatMap allTVars args) patVars 

renameUnused _ [] = []
renameUnused usedVars (CPVar (i,v):vs) 
      | elem (i,v) usedVars = CPVar (i,v) : renameUnused usedVars vs
      | otherwise = CPVar (i,"_") : renameUnused usedVars vs


pxml opts name attrs elems 
  = CPComb ("XML","XElem") [pString (tag opts name),pList attrs,pList elems]

callXml2 (t,i) = CApply (callXml2Type t) (toVar i)

callXml2Type (CTVar v) = CVar (renVar v)
callXml2Type (CTCons name args)
  | snd name=="[]" && args==[CTCons ("Prelude","Char") []]
  = sym (fromXmlName ("Prelude","String"))
  | otherwise 
  = app (CSymbol $ fromXmlName name) (map callXml2Type args)
callXml2Type (CFuncType _ _) = error "unable to transform functions from XML"

-----------------------------
-- treat imported data types
-----------------------------

importTypes m ts = do
  let imps = nub (map importType ts)
  let specials = if isPrelude m then ["Read","ReadShowTerm"] else []
  imessage imps
  return (imps++specials)
 
imessage [] = done
imessage [m] = putStrLn $ "You also need to generate the module "++m
imessage (m:m':ms) =
  putStrLn $ "You also need to generate the modules "++(unwords $ m:m':ms)
                 
importType (m,f)
  | isPrelude m && elem f ["String","[]","Char","Int","Float"] 
  = "PreludeDataToXml"
  | isPrelude m && f == "IO"
  = error "unable to transform I/O actions to XML"
  | isPrelude m && f == "Success"
  = error "unable to transform constraints to XML"
  | otherwise = m++"DataToXml"

-----------------------------------------
-- treat special prelude types
-----------------------------------------

specialNames = ["Int","Float","String","Char","IO","Success","[]","()","(,)"]

filterSpecials 
  = filter ((\ (m,n) -> not (isPrelude m && elem n specialNames)) . typeName)

specialFuncs opts =
  [mkList2xml opts,mkXml2List opts] ++
  concatMap (\tname -> [baseType2xml opts tname, baseTypeXml2 opts tname])
            ["String","Int","Float","Char"] ++
  concatMap (\tdecl -> [mkType2Xml opts tdecl, mkXml2Type opts tdecl])
            (map mkTupleType (0:[2..12]))

-- make tuple type of arity n:
mkTupleType n =
  CType ("Prelude",tcons) Public tvars
        [CCons ("Prelude",tcons) n Public (map CTVar tvars)]
 where tcons = "(" ++ take (n-1) (repeat ',') ++ ")"
       tvars = map (\i -> (i,'a':show i)) [1..n]

mkList2xml opts = 
   CFunc (toXmlName ("Prelude","[]")) 2 Public
     (CFuncType (CFuncType (CTVar (0,"a")) xmlType)
                (CFuncType (CTCons ("Prelude","[]") [CTVar (0,"a")]) xmlType))
     (CRules CFlex 
        [CRule (pVars 2)
             [noGuard (app (sym ("XML","XElem")) [cString (listTag opts),nil, 
                            app (app (preSym "map") [toVar 0]) [toVar 1]])] []])

mkXml2List opts = 
   CFunc (fromXmlName ("Prelude","[]")) 2 Public
     (CFuncType (CFuncType xmlType (CTVar (0,"a")))
                (CFuncType xmlType (CTCons ("Prelude","[]") [CTVar (0,"a")])))
     (CRules CFlex 
        [CRule [x, CPComb ("XML","XElem") [pString (listTag opts),pNil,y]]
             [noGuard  (app (preSym "map") [toVar 0,toVar 1])] []])
  where
    [x,y] = pVars 2

baseType2xml opts s 
  = CFunc (toXmlName ("Prelude",s)) 1 Public
     (CFuncType (CTCons ("Prelude",s) []) xmlType)
     (CRules CFlex 
       [CRule (pVars 1) 
         [noGuard (xml opts s [] [writeFun s])] []])

baseTypeXml2 opts s 
  = CFunc (fromXmlName ("Prelude",s)) 1 Public
     (CFuncType xmlType (CTCons ("Prelude",s) []))
     (CRules CFlex 
        (CRule [pxml opts s [] [CPComb ("XML","XText") (pVars 1)]]
           [noGuard (readFun s)] []:
         if s=="String" 
           then [CRule [pxml opts s [] []]
                       [noGuard (cString "")] []]
           else []))

readFun "Int"    = app (sym ("Read","readInt")) [toVar 0]
readFun "Char"   = app (preSym "head") [toVar 0]
readFun "Float"  = app (sym ("ReadShowTerm","readQTerm")) [toVar 0]
readFun "String" = toVar 0

writeFun s = case s of
              "String" -> app (CSymbol ("XML","xtxt")) [toVar 0]
              _        -> app (CSymbol ("XML","xtxt")) [cShow (toVar 0)]




