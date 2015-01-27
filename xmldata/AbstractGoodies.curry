module AbstractGoodies where
import AbstractCurry

--- test if type declaration introduces data type
isDataTypeDecl :: CTypeDecl -> Bool
isDataTypeDecl (CTypeSyn _ _ _ _) = False
isDataTypeDecl (CType _ _ _ _) = True

--- test if type expression is a base type, i.e., Int, Float, Char
isBaseType :: CTypeExpr -> Bool
isBaseType (CTVar _) = False
isBaseType (CTCons (m,name) _) 
  = isPrelude m && elem name ["Int","Char","Float"]

--- test module for prelude
isPrelude :: String -> Bool
isPrelude m = m=="Prelude"

--- test qualified name for prelude
isFromPrelude :: QName -> Bool
isFromPrelude = isPrelude . fst

--- for given n yield a list of n PVars starting from 0
pVars :: Int -> [CPattern]
pVars n = [CPVar (i,"x"++show i) | i<-[0..n-1]] 

--- yield a standard variable name 
varName :: Int -> String
varName i = "x"++show i

--- yield a standard variable id
varId :: Int -> CVarIName
varId i = (i,varName i)

--- yield a standard variable
toVar :: Int -> CExpr
toVar i = CVar (varId i)


--- transform a string into the abstract curry representation of a string
cString :: String -> CExpr
cString [] = nil 
cString (x:xs) = app cons [cChar x,cString xs]

--- transform a string into the abstract curry representation of a string pattern
pString :: String -> CPattern
pString [] = pNil 
pString (x:xs) = pCons [pChar x,pString xs]

--- yield abstract representation of a given character
cChar :: Char -> CExpr
cChar x = CLit (CCharc x)
--- yield abstract representation of the pattern of a given character
pChar :: Char -> CPattern
pChar x = CPLit (CCharc x)

--- transform a list into the abstract curry representation of a list
cList :: [CExpr] -> CExpr
cList [] = nil 
cList (x:xs) = app cons [x,cList xs]

--- transform a list into the abstract curry representation of a list pattern
pList :: [CPattern] -> CPattern
pList [] = pNil 
pList (x:xs) =  pCons [x,pList xs]

--- abstract representation of 2-tuples
cPair :: (CExpr,CExpr) -> CExpr
cPair (x,y) = app (CSymbol ("Prelude","(,)")) [x,y]

--- abstract representation of an empty list
nil :: CExpr
nil = CSymbol ("Prelude","[]")
pNil :: CPattern
pNil = CPComb ("Prelude","[]") []

--- abstract representation of the list constructor
cons :: CExpr
cons = CSymbol ("Prelude",":")
pCons :: [CPattern] -> CPattern
pCons = CPComb ("Prelude",":") 

--- given e yield a right hand side without guard expression
noGuard :: CExpr -> (CExpr,CExpr)
noGuard e = (CSymbol ("Prelude","success"), e)

--- abstract representation of applying f to a number of arguments 
app :: CExpr -> [CExpr] -> CExpr
app f args = foldl CApply f args

--- shortcut for calling show
cShow :: CExpr -> CExpr
cShow e = app (preSym "show") [e]

--- ease using prelude symbols
preSym :: String -> CExpr
preSym s = CSymbol ("Prelude",s)

--- ease using symbols
sym :: QName -> CExpr
sym = CSymbol

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
  | name==("Prelude","[]") && ts == [CTCons ("Prelude","Char") []]
  = [("Prelude","String")]
  | otherwise
  = name : concatMap requiredTypesTypeExpr ts

--- yield the name of a given type
typeName :: CTypeDecl -> QName
typeName (CTypeSyn n _ _ _) = n
typeName (CType n _ _ _)      = n

allTVars :: CTypeExpr -> [(Int,String)]
allTVars (CTVar v) = [v]
allTVars (CFuncType t1 t2) = allTVars t1 ++ allTVars t2
allTVars (CTCons _ args) = concatMap allTVars args