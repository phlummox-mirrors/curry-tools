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
--- @version February 2015
------------------------------------------------------------------------------

{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Data2Xml where

import AbstractCurry
import AbstractCurryGoodies
import AbstractCurryPrinter
import Char
import FileGoodies
import List
import System
 
data Options = LowCase | FileName String
  deriving Eq
  
main :: IO ()
main = do
  args <- getArgs
  let opts = reverse (argsToOptions args)
  if null opts then putStrLn usageMsg
               else derive opts

usageMsg :: String
usageMsg = "Usage: data2xml [-low] <filename>\n"++
           "Options:\n" ++
           "-low: make all tags lowercase"

argsToOptions :: [String] -> [Options]
argsToOptions args = case args of
  "-low":opts -> LowCase : argsToOptions opts
  [s]         -> [FileName (stripSuffix s)]
  _           -> []

derive :: [Options] -> IO ()
derive (FileName fn:opts) = do
  CurryProg modName  _ ts _ _ <- readCurry fn
  let (specials,types) = if isPrelude modName 
                           then (specialFuncs opts,filterSpecials ts)
                           else ([],ts)
      progName = transModName fn
      impTypes = maybeString $ nub $ filter ((/=modName) .fst)
                             $ concatMap requiredTypesTypeDecl types
      ignTypes = ignoredTypes types
      xmltypes  = filter (\t -> typeName t `notElem` ignTypes) types
  unless (null ignTypes) $
    putStrLn ("WARNING: the following types are not translated:\n"++
              unwords (map snd ignTypes))
  imports <- importTypes modName impTypes
  writeFile (progName++".curry") $ showProg $
   CurryProg progName (nub $ ["XML",fn] ++ imports) [] 
             (map (mkType2Xml opts) xmltypes ++
              map (mkXml2Type opts) xmltypes ++ specials)
             []
  putStrLn ("You can now import "++progName)

maybeString :: [QName] -> [QName]
maybeString xs 
  = if notElem (pre "String") xs && elem (pre "[]") xs && elem (pre "Char") xs
    then (pre "String" : xs)
    else xs

-- Transform original module name into name of the transformation module:
transModName :: String -> String
transModName mn = mn ++ "DataToXml"

----------------------------
-- naming the new functions
----------------------------

toXmlName :: QName -> QName
toXmlName (m,s) = case (isPrelude m,s,isTupleName s) of 
  (True,"[]",_)  -> (nm,"list_To_Xml")
  (True,"()",_)  -> (nm,"unitToXml")
  (True,_,True)  -> (nm,"tuple"++show (length s-1)++"ToXml")
  (_,c:cs,_)     -> (nm,toLower c:cs ++ "ToXml")
 where nm = transModName m
                  
 
fromXmlName :: QName -> QName
fromXmlName (m,s) = case (isPrelude m,s,isTupleName s) of
  (True,"[]",_)  -> (nm,"xml_To_List")
  (True,"()",_)  -> (nm,"xmlToUnit")
  (True,_,True)  -> (nm,"xmlToTuple"++show (length s-1))
  _              -> (nm,"xmlTo"++s)
 where nm = transModName m
                 

listTag :: [Options] -> String
listTag opts = tag opts "List"

isTupleName :: String -> Bool
isTupleName (n:name) = n=='(' && isTuple name
  where
    isTuple ""  = False
    isTuple ")" = True
    isTuple (c:c':cs) = c==',' && isTuple (c':cs)
    
----------------------------
-- generating tags
----------------------------

tag :: [Options] -> String -> String
tag opts s = if elem LowCase opts then map toLower s else s

tagNameForCons :: QName -> String
tagNameForCons (mname,cname)
  | isTupleName cname = "Tuple" ++ show (length cname - 1)
  | isPrelude mname   = cname
  | otherwise         = mname++"_"++cname

-------------------------------------------------
-- make functions to transform data terms to xml
-------------------------------------------------
 
mkType2Xml :: [Options] -> CTypeDecl -> CFuncDecl
mkType2Xml _ (CTypeSyn name vis vars t) =
  CFunc (toXmlName name) 1 vis
        (CFuncType (CTCons name (map CTVar vars)) xmlType)
        (CRules CFlex [CRule [CPVar (0,"x0")] 
                             [noGuard (call2xml (t,0))] []])
mkType2Xml opts (CType name vis vars cs) =
  CFunc (toXmlName name) (1+length vars) vis
        (type2XmlType vars 
             (CFuncType (CTCons name (map CTVar vars)) xmlType))
        (CRules CFlex (map (mkConsDecl2Xml opts $ 
                              map (CPVar . renVar) vars) cs))
  
mkConsDecl2Xml :: [Options] -> [CPattern] -> CConsDecl -> CRule
mkConsDecl2Xml opts patVars (CCons name arity _ args)
  = CRule (newPatVars++[CPComb name (pVars arity)] )
          [noGuard 
             (xml opts (tagNameForCons name) []
                  (map call2xml (zip args [0..])))] []
  where
    newPatVars = renameUnused (map renVar $ concatMap tvarsOfType args) patVars

type2XmlType :: [(Int,String)] -> CTypeExpr -> CTypeExpr
type2XmlType vars t 
  = foldr CFuncType t (map (\x->CFuncType (CTVar x) xmlType) vars)  

call2xml :: (CTypeExpr,Int) -> CExpr
call2xml (t,i) = CApply (call2xmlType t) (toVar i)

call2xmlType :: CTypeExpr -> CExpr
call2xmlType (CTVar v) = CVar (renVar v)
call2xmlType (CTCons name args) 
  | snd name == "[]" &&  args==[CTCons (pre "Char") []]
  = CSymbol $ toXmlName (pre "String")
  | otherwise 
  = applyF (toXmlName name) (map call2xmlType args)
call2xmlType (CFuncType _ _) = error "unable to transform functions to XML"

-- Compute the types that cannot be converted to XML.
-- These are the types containing functional arguments or depending
-- on such higher-order types.
ignoredTypes :: [CTypeDecl] -> [QName]
ignoredTypes = computeIgnoredTypes []
 where
  computeIgnoredTypes its types =
    let newits = map typeName (filter (containsFuncTypes its) types)
     in if null newits
        then its
        else computeIgnoredTypes (newits++its)
               (filter (\t -> typeName t `notElem` newits) types)

  containsFuncTypes its (CTypeSyn _ _ _ t) = containsFuncType its t
  containsFuncTypes its (CType _ _ _ cs) =
    any (\ (CCons _ _ _ cts) -> any (containsFuncType its) cts) cs
  
  containsFuncType _ (CTVar _)        = False
  containsFuncType _ (CFuncType _ _)  = True
  containsFuncType its (CTCons cn targs) =
    cn `elem` its || any (containsFuncType its) targs
  
xml :: [Options] -> String -> [CExpr] -> [CExpr] -> CExpr
xml opts name attrs elems 
  = applyF ("XML","XElem")
           [string2ac (tag opts name), list2ac attrs, list2ac elems]

xmlType :: CTypeExpr
xmlType = CTCons ("XML","XmlExp") []

-------------------------------------------------
-- make functions to transform xml to data terms 
-------------------------------------------------

mkXml2Type :: [Options] -> CTypeDecl -> CFuncDecl
mkXml2Type _ (CTypeSyn name vis vars t) =
  CFunc (fromXmlName name) 1 vis
        (CFuncType xmlType (CTCons name (map CTVar vars)))
        (CRules CFlex [CRule [CPVar (0,"x0")] 
                             [noGuard (callXml2 (t,0))] []])
mkXml2Type opts (CType name vis vars cs) =
  CFunc (fromXmlName name) (1+length vars) vis
        (xml2typeType vars 
           (CFuncType xmlType (CTCons name (map CTVar vars))))
        (CRules CFlex (map (mkXml2ConsDecl opts $ map (CPVar . renVar) vars) cs))
  
renVar :: (a,String) -> (a,String)
renVar (i,s) = case s of
                ('x':xs) -> (i,'t':xs)
                _ -> (i,s)

xml2typeType :: [(Int,String)] -> CTypeExpr -> CTypeExpr
xml2typeType vars t 
  = foldr CFuncType t (map (\x->CFuncType xmlType (CTVar x)) vars)  

mkXml2ConsDecl :: [Options] -> [CPattern] -> CConsDecl -> CRule
mkXml2ConsDecl opts patVars (CCons name arity _ args)
  = CRule (newPatVars++[pxml opts (tagNameForCons name) [] (pVars arity)])
          [noGuard (applyF name (map callXml2 (zip args [0..])))] []
  where
    newPatVars = renameUnused (map renVar $ concatMap tvarsOfType args) patVars 

renameUnused :: [(Int,String)] -> [CPattern] -> [CPattern]
renameUnused _ [] = []
renameUnused usedVars (CPVar (i,v):vs) 
      | elem (i,v) usedVars = CPVar (i,v) : renameUnused usedVars vs
      | otherwise = CPVar (i,"_") : renameUnused usedVars vs


pxml :: [Options] -> String -> [CPattern] -> [CPattern] -> CPattern
pxml opts name attrs elems 
  = CPComb ("XML","XElem")
           [stringPattern (tag opts name), listPattern attrs, listPattern elems]

callXml2 :: (CTypeExpr,Int) -> CExpr
callXml2 (t,i) = CApply (callXml2Type t) (toVar i)

callXml2Type :: CTypeExpr -> CExpr
callXml2Type (CTVar v) = CVar (renVar v)
callXml2Type (CTCons name args)
  | snd name=="[]" && args==[CTCons (pre "Char") []]
  = CSymbol (fromXmlName (pre "String"))
  | otherwise 
  = applyF (fromXmlName name) (map callXml2Type args)
callXml2Type (CFuncType _ _) = error "unable to transform functions from XML"

-----------------------------
-- treat imported data types
-----------------------------

importTypes :: String -> [QName] -> IO ([String])
importTypes m ts = do
  let imps = nub (map importType ts)
  let specials = if isPrelude m then ["Read","ReadShowTerm"] else []
  imessage imps
  return (imps++specials)
 
imessage :: [String] -> IO ()
imessage [] = done
imessage [m] = putStrLn $ "You also need to generate the module "++m
imessage (m:m':ms) =
  putStrLn $ "You also need to generate the modules "++(unwords $ m:m':ms)
                 
importType :: QName -> String
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

specialNames :: [String]
specialNames = ["Int","Float","String","Char","IO","Success","[]","()","(,)"]

filterSpecials :: [CTypeDecl] -> [CTypeDecl]
filterSpecials 
  = filter ((\ (m,n) -> not (isPrelude m && elem n specialNames)) . typeName)

specialFuncs :: [Options] -> [CFuncDecl]
specialFuncs opts =
  [mkList2xml opts,mkXml2List opts] ++
  concatMap (\tname -> [baseType2xml opts tname, baseTypeXml2 opts tname])
            ["String","Int","Float","Char"] ++
  concatMap (\tdecl -> [mkType2Xml opts tdecl, mkXml2Type opts tdecl])
            (map mkTupleType (0:[2..12]))

-- make tuple type of arity n:
mkTupleType :: Int -> CTypeDecl
mkTupleType n =
  CType (pre tcons) Public tvars
        [CCons (pre tcons) n Public (map CTVar tvars)]
 where tcons = "(" ++ take (n-1) (repeat ',') ++ ")"
       tvars = map (\i -> (i,'a':show i)) [1..n]

mkList2xml :: [Options] -> CFuncDecl
mkList2xml opts = 
   CFunc (toXmlName (pre "[]")) 2 Public
     (CFuncType (CFuncType (CTVar (0,"a")) xmlType)
                (CFuncType (CTCons (pre "[]") [CTVar (0,"a")]) xmlType))
     (CRules CFlex 
        [CRule (pVars 2)
               [noGuard (applyF ("XML","XElem")
	                   [string2ac (listTag opts), list2ac [], 
                            applyE (applyF (pre "map") [toVar 0]) [toVar 1]])]
	       []])

mkXml2List :: [Options] -> CFuncDecl
mkXml2List opts = 
   CFunc (fromXmlName (pre "[]")) 2 Public
     (CFuncType (CFuncType xmlType (CTVar (0,"a")))
                (CFuncType xmlType (CTCons (pre "[]") [CTVar (0,"a")])))
     (CRules CFlex 
        [CRule [x, CPComb ("XML","XElem") [stringPattern (listTag opts),pNil,y]]
             [noGuard  (applyF (pre "map") [toVar 0,toVar 1])] []])
  where
    [x,y] = pVars 2

baseType2xml :: [Options] -> String -> CFuncDecl
baseType2xml opts s 
  = CFunc (toXmlName (pre s)) 1 Public
     (CFuncType (CTCons (pre s) []) xmlType)
     (CRules CFlex 
       [CRule (pVars 1) 
         [noGuard (xml opts s [] [writeFun s])] []])

baseTypeXml2 :: [Options] -> String -> CFuncDecl
baseTypeXml2 opts s 
  = CFunc (fromXmlName (pre s)) 1 Public
     (CFuncType xmlType (CTCons (pre s) []))
     (CRules CFlex 
        (CRule [pxml opts s [] [CPComb ("XML","XText") (pVars 1)]]
           [noGuard (readFun s)] []:
         if s=="String" 
           then [CRule [pxml opts s [] []]
                       [noGuard (string2ac "")] []]
           else []))

readFun :: String -> CExpr
readFun "Int"    = applyF ("Read","readInt") [toVar 0]
readFun "Char"   = applyF (pre "head") [toVar 0]
readFun "Float"  = applyF ("ReadShowTerm","readQTerm") [toVar 0]
readFun "String" = toVar 0

writeFun :: String -> CExpr
writeFun s = case s of
  "String" -> applyF ("XML","xtxt") [toVar 0]
  "Char"   -> applyF ("XML","xtxt") [list2ac [toVar 0]]
  _        -> applyF ("XML","xtxt") [applyF (pre "show") [toVar 0]]


---------------------------------------------------------------------
-- Auxiliaries:

--- yield list of all types the given type depends on
requiredTypesTypeDecl :: CTypeDecl -> [QName]
requiredTypesTypeDecl (CTypeSyn _ _ _ e) = requiredTypesTypeExpr e
requiredTypesTypeDecl (CType _ _ _ cs) = concatMap requiredTypesConsDecl cs

requiredTypesConsDecl :: CConsDecl -> [QName]
requiredTypesConsDecl (CCons _ _ _ ts) = concatMap requiredTypesTypeExpr ts

requiredTypesTypeExpr :: CTypeExpr -> [QName]
requiredTypesTypeExpr (CTVar _) = []
requiredTypesTypeExpr (CFuncType e1 e2) 
  = requiredTypesTypeExpr e1 ++ requiredTypesTypeExpr e2
requiredTypesTypeExpr (CTCons name ts) 
  | name==(pre "[]") && ts == [CTCons (pre "Char") []]
  = [pre "String"]
  | otherwise
  = name : concatMap requiredTypesTypeExpr ts

---------------------------------------------------------------------

