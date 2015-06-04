----------------------------------------------------------------------
--- Operations to generate documentation in HTML format.
---
--- @author Michael Hanus, Jan Tikovsky
--- @version June 2015
----------------------------------------------------------------------

module CurryDocHtml where

import CurryDocParams
import CurryDocRead
import CurryDocConfig
import TotallyDefined(Completeness(..))
import FilePath
import AbstractCurry
import AbstractCurryGoodies
import qualified FlatCurry as FC
import qualified FlatCurryGoodies as FCG
import HTML
import BootstrapStyle
import List
import Char
import Sort
import Time
import Distribution
import CategorizedHtmlList
import Markdown

infixl 0 `withTitle`

--------------------------------------------------------------------------
-- Generates the documentation of a module in HTML format where the comments
-- are already analyzed.
generateHtmlDocs :: DocParams -> AnaInfo -> String -> String
                 -> [(SourceLine,String)] -> IO String
generateHtmlDocs docparams anainfo modname modcmts progcmts = do
  acyname <- findFileInLoadPath (abstractCurryFileName modname)
  putStrLn $ "Reading AbstractCurry program \""++acyname++"\"..."
  (CurryProg _ imports types functions ops) <- readAbstractCurryFile acyname
  let exptypes = filter isExportedType types
      expfuns  = filter isExportedFun functions
  mainPage title htmltitle (lefttopmenu types) rightTopMenu 3
           [bold [htxt "Exported names:"],
            genHtmlExportIndex (map tName exptypes)
                               (getExportedCons types)
                               (getExportedFields types)
                               (map fName expfuns),
            anchored "imported_modules" [bold [htxt "Imported modules:"]],
            ulist (map (\i -> [href (getLastName i++".html") [htxt i]])
                       imports) `addClass` "nav nav-list"]
    (genHtmlModule docparams modcmts ++
       ([h2 [htxt "Summary of exported operations:"],
         borderedTable
           (map (genHtmlFuncShort docparams progcmts anainfo) expfuns)] ++
        (maybeGenDoc [] (\tys ->
          [anchoredSection "exported_datatypes"
                               [h2 [htxt "Exported datatypes:"]], hrule] ++
               concatMap (genHtmlType docparams progcmts) tys) exptypes) ++
        [anchoredSection "exported_operations"
                  [h2 [htxt "Exported operations:"]]] ++
        (map (genHtmlFunc docparams modname progcmts anainfo ops) expfuns)))
    False
 where
   title = "Module "++modname

   htmltitle = [h1 [htxt "Module ",
                    href (modname++"_curry.html")
                         [htxt (modname++".curry")]]]

   lefttopmenu types =
     [[href "?" [htxt title]],
      [href "#imported_modules" [htxt "Imports"]]] ++
     (if null types then []
      else [[href "#exported_datatypes" [htxt "Datatypes"]]]) ++
     [[href "#exported_operations" [htxt "Operations"]]]


--- Translate a documentation comment to HTML and use markdown translation
--- if necessary
--- @return: either a paragraph (`<p>`) element or an empty list.
docComment2HTML :: DocParams -> String -> [HtmlExp]
docComment2HTML opts cmt
  | null cmt          = []
  | withMarkdown opts = markdownText2HTML (replaceIdLinks cmt)
  | otherwise         = [par [HtmlText (replaceIdLinks cmt)]]

-- replace identifier hyperlinks in a string (i.e., enclosed in single quotes)
-- by HTML hyperrefences:
replaceIdLinks :: String -> String
replaceIdLinks str = case str of
  [] -> []
  ('\\':'\'':cs) -> '\'' : replaceIdLinks cs
  (c:cs) -> if c=='\'' then tryReplaceIdLink [] cs
                       else c : replaceIdLinks cs
 where
  tryReplaceIdLink ltxt [] = '\'' : reverse ltxt
  tryReplaceIdLink ltxt (c:cs)
   | isSpace c = '\'' : reverse ltxt ++ c : replaceIdLinks cs -- no space in id
   | c == '\'' = checkId (reverse ltxt) ++ replaceIdLinks cs
   | otherwise = tryReplaceIdLink (c:ltxt) cs

  checkId s =
    if ' ' `elem` s
    then '\'' : s ++ ['\'']
    else let (md,dotfun) = break (=='.') s
          in "<code><a href=\"" ++
             (if null dotfun then '#':s else md++".html#"++tail dotfun) ++
             "\">"++s++"</a></code>"

-- generate HTML index for all exported names:
genHtmlExportIndex :: [String] -> [String] -> [String] -> [String] -> HtmlExp
genHtmlExportIndex exptypes expcons expfields expfuns =
  HtmlStruct "ul" [("class","nav nav-list")]
    (concatMap (\ (htmlnames,cattitle) ->
                 if null htmlnames
                 then []
                 else HtmlStruct "li" [("class","nav-header")] [htxt cattitle] :
                      map (HtmlStruct "li" []) htmlnames)
            [(htmltypes,"Datatypes:"),
             (htmlcons ,"Constructors:"),
             (htmlfields,"Fields:"),
             (htmlfuns ,"Operations:")])
 where
  htmltypes  = map (\n->[href ('#':n) [htxt n]])
                   (nub (sortStrings exptypes))
  htmlcons   = map (\n->[href ('#':n++"_CONS") [htxt n]])
                   (nub (sortStrings expcons))
  htmlfields = map (\n->[href ('#':n++"_FIELD") [htxt n]])
                  (nub (sortStrings expfields))
  htmlfuns   = map (\n->[href ('#':n) [htxt n]])
                   (nub (sortStrings expfuns))

tName :: CTypeDecl -> String
tName = snd . typeName

fName :: CFuncDecl -> String
fName = snd . funcName

cName :: CConsDecl -> String
cName = snd . consName

fldName :: CFieldDecl -> String
fldName (CField (_,name) _ _) = name

isExportedType :: CTypeDecl -> Bool
isExportedType = (== Public) . typeVis

isExportedCons :: CConsDecl -> Bool
isExportedCons = (== Public) . consVis

isExportedFun :: CFuncDecl -> Bool
isExportedFun = (== Public) . funcVis

isExportedField :: CFieldDecl -> Bool
isExportedField (CField _ vis _) = vis == Public

-- extract the names of all exported constructors
getExportedCons :: [CTypeDecl] -> [String]
getExportedCons = map cName . filter isExportedCons . concatMap typeCons

-- extract the names of all exported fields
getExportedFields :: [CTypeDecl] -> [String]
getExportedFields = map fldName . filter isExportedField . concatMap getFields
                  . concatMap typeCons
 where
  getFields (CCons   _ _ _ ) = []
  getFields (CRecord _ _ fs) = fs


--- generate HTML documentation for a module:
genHtmlModule :: DocParams -> String -> [HtmlExp]
genHtmlModule docparams modcmts =
  let (maincmt,avcmts) = splitComment modcmts
   in docComment2HTML docparams maincmt ++
      map (\a->par [bold [htxt "Author: "], htxt a])
          (getCommentType "author" avcmts) ++
      map (\a->par [bold [htxt "Version: "], htxt a])
          (getCommentType "version" avcmts)

ulistOrEmpty :: [[HtmlExp]] -> [HtmlExp]
ulistOrEmpty items | null items = []
                   | otherwise  = [ulist items]

-- generate the html documentation for given comments ("param", "return",...)
maybeGenDoc :: [HtmlExp] -> ([a] -> [HtmlExp]) -> [a] -> [HtmlExp]
maybeGenDoc def genDoc cmt
  | null cmt  = def
  | otherwise = genDoc cmt

--- generate HTML documentation for a datatype if it is exported:
genHtmlType :: DocParams -> [(SourceLine,String)] -> CTypeDecl -> [HtmlExp]
genHtmlType docparams progcmts (CType (_,tcons) _ tvars constrs) =
  let (datacmt,consfldcmts) = splitComment (getDataComment tcons progcmts)
   in    [ anchored tcons [style "typeheader" [htxt tcons]] ]
      ++ docComment2HTML docparams datacmt
      ++ [par [explainCat "Constructors:"]]
      ++ ulistOrEmpty (map (genHtmlCons docparams consfldcmts tcons tvars fldCons)
                           (filter isExportedCons constrs))
      ++ [hrule]
 where
  expFields = [f | CRecord _ _ fs <- constrs, f <- fs, isExportedField f]
  fldCons   = [ (fn,cn) | f@(CField (_,fn) _ _) <- expFields
              , CRecord (_,cn) _ fs <- constrs, f `elem` fs
              ]
genHtmlType docparams progcmts (CTypeSyn (tcmod,tcons) _ tvars texp) =
  let (typecmt,_) = splitComment (getDataComment tcons progcmts)
   in    [ anchored tcons [style "typeheader" [htxt tcons]] ]
      ++ docComment2HTML docparams typecmt
      ++ [ par [explainCat "Type synonym:"
         , nbsp
         ,
            if tcons=="String" && tcmod=="Prelude"
            then code [htxt "String = [Char]"]
            else code [HtmlText
                        (tcons ++ concatMap (\(i,_) -> [' ',chr (97+i)]) tvars ++
                         " = " ++ showType tcmod False texp)]]
         , hrule
         ]
genHtmlType docparams progcmts t@(CNewType (_,tcons) _ tvars constr) =
  let (datacmt,consfldcmts) = splitComment (getDataComment tcons progcmts)
   in if isExportedCons constr
        then    [anchored tcons [style "typeheader" [htxt tcons]] ]
             ++ docComment2HTML docparams datacmt
             ++ [par [explainCat "Constructor:"]]
             ++ ulistOrEmpty [genHtmlCons docparams consfldcmts tcons tvars fldCons constr]
             ++ [hrule]
        else []
 where
  cn      = cName constr
  fldCons = map (\fn -> (fn,cn)) (getExportedFields [t])

--- generate HTML documentation for a constructor if it is exported:
genHtmlCons :: DocParams -> [(String,String)] -> String -> [CTVarIName]
             -> [(String,String)] -> CConsDecl -> [HtmlExp]
genHtmlCons docparams consfldcmts tcons tvars _ (CCons (cmod,cname) _ argtypes) =
    anchored (cname ++ "_CONS")
      [code [opnameDoc [htxt cname],
             HtmlText (" :: " ++
                       concatMap (\t -> " "++showType cmod True t++" -> ")
                                 argtypes ++
                       tcons ++ concatMap (\(i,_) -> [' ',chr (97+i)]) tvars)]] :
      maybe []
            (\ (_,cmt) -> htxt " : " : removeTopPar (docComment2HTML docparams
                                                    (removeDash cmt)))
            (getConsComment conscmts cname)
 where
  conscmts = getCommentType "cons" consfldcmts
genHtmlCons docparams consfldcmts tcons tvars fldCons (CRecord (cmod,cname) _ fields) =
    anchored (cname ++ "_CONS")
      [code [opnameDoc [htxt cname],
             HtmlText (" :: " ++
                       concatMap (\t -> " "++showType cmod True t++" -> ")
                                 argtypes ++
                       tcons ++ concatMap (\(i,_) -> [' ',chr (97+i)]) tvars)]] :
      (maybe []
            (\ (_,cmt) -> htxt " : " : removeTopPar (docComment2HTML docparams
                                                    (removeDash cmt)))
            (getConsComment conscmts cname)) ++
      par [explainCat "Fields:"] :
      ulistOrEmpty (map (genHtmlField docparams fldcmts cname fldCons)
                        (filter isExportedField fields))
 where
  argtypes = map (\(CField _ _ t) -> t) fields
  conscmts = getCommentType "cons" consfldcmts
  fldcmts  = getCommentType "field" consfldcmts

-- generate HTML documentation for record fields
genHtmlField :: DocParams -> [String] -> String -> [(String,String)]
             -> CFieldDecl -> [HtmlExp]
genHtmlField docparams fldcmts cname fldCons (CField (fmod,fname) _ ty)
  | withAnchor fname = [anchored (fname ++ "_FIELD") html]
  | otherwise        = html
 where
  withAnchor f = maybe False (== cname) (lookup f fldCons)
  html         = [ code [opnameDoc [htxt fname]
                 , HtmlText (" :: " ++ showType fmod True ty)]
                 ] ++ maybe []
                            (\ (_,cmt) -> htxt " : " : removeTopPar
                               (docComment2HTML docparams (removeDash cmt)))
                            (getConsComment fldcmts fname)

-- generate short HTML documentation for a function:
genHtmlFuncShort :: DocParams -> [(SourceLine,String)] -> AnaInfo -> CFuncDecl -> [[HtmlExp]]
genHtmlFuncShort docparams progcmts anainfo
                 (CFunc (fmod,fname) _ _ ftype _) =
 [[code [opnameDoc
            [anchored (fname ++ "_SHORT")
                      [href ('#':fname) [htxt (showId fname)]]],
         HtmlText (" :: " ++ showType fmod False ftype)],
     nbsp, nbsp]
     ++ genFuncPropIcons anainfo (fmod,fname) ++
  [breakline] ++
   removeTopPar
      (docComment2HTML docparams
         (firstSentence (fst (splitComment
                                (getFuncComment fname progcmts)))))]
genHtmlFuncShort docparams progcmts anainfo (CmtFunc _ n a vis ftype rules) =
  genHtmlFuncShort docparams progcmts anainfo (CFunc n a vis ftype rules)

-- generate HTML documentation for a function:
genHtmlFunc :: DocParams -> String -> [(SourceLine,String)] -> AnaInfo
            -> [COpDecl] -> CFuncDecl -> HtmlExp
genHtmlFunc docparams modname progcmts anainfo ops (CmtFunc _ n a vis ftype rules) =
  genHtmlFunc docparams modname progcmts anainfo ops (CFunc n a vis ftype rules)
genHtmlFunc docparams modname progcmts anainfo ops
            (CFunc (fmod,fname) _ _ ftype rules) =
  let (funcmt,paramcmts) = splitComment (getFuncComment fname progcmts)
   in anchored fname
       [borderedTable [[
         [par $
           [code [opnameDoc
                   [href (modname++"_curry.html#"++fname)
                         [htxt (showId fname)]],
                  HtmlText (" :: "++ showType fmod False ftype)],
            nbsp, nbsp] ++
           genFuncPropIcons anainfo (fmod,fname)] ++
         docComment2HTML docparams funcmt ++
         genParamComment paramcmts ++
         -- show further infos for this function, if present:
         (if furtherInfos == []
          then []
          else [dlist [([explainCat "Further infos:"],
                        [ulist furtherInfos])]] )]]]
 where
  furtherInfos = genFuncPropComments anainfo (fmod,fname) rules ops

  genParamComment paramcmts =
    let params = map (span isIdChar) (getCommentType "param" paramcmts)
        ret    = getCommentType "return" paramcmts
     in  maybeGenDoc [] (\parCmts ->
          [ par [ explainCat "Example call:", nbsp
                , code [htxt (showCall fname (map fst params))]
                ]
          , par [explainCat "Parameters:"]
          , ulist (map (\(parid,parcmt) -> [code [htxt parid], htxt " : "]
             ++ removeTopPar (docComment2HTML docparams (removeDash parcmt))) parCmts)
          ]) params
      ++ maybeGenDoc [] (\retCmt -> [dlist (map (\rescmt ->
          ([explainCat "Returns:"],
           removeTopPar (docComment2HTML docparams rescmt))) retCmt)]) ret

  showCall f params =
    if isAlpha (head f) || length params /= 2
    then "(" ++ showId f ++ concatMap (" "++) params ++ ")"
    else "(" ++ params!!0 ++ " " ++ f ++ " " ++ params!!1 ++ ")"

-- remove initial dash sign (of a parameter comment)
removeDash :: String -> String
removeDash s = let ds = dropWhile isSpace s in
  if take 2 ds == "- " then dropWhile isSpace (drop 2 ds)
                       else ds

-- remove a single top-level paragraph in HTML expressions:
removeTopPar :: [HtmlExp] -> [HtmlExp]
removeTopPar hexps = case hexps of
  [HtmlStruct "p" [] hs] -> hs
  _ -> hexps

--------------------------------------------------------------------------
--- Generates icons for particular properties of functions.
genFuncPropIcons :: AnaInfo -> QName -> [HtmlExp]
genFuncPropIcons anainfo fname =
   [detPropIcon, nbsp]
 where
   --(non)deterministically defined property:
   detPropIcon =
    if getNondetInfo anainfo fname
    then href "index.html#nondet_explain" [nondetIcon]
    else href "index.html#det_explain"    [detIcon]

--------------------------------------------------------------------------
--- Generates further textual infos about particular properties
--- of a function. The result is a list of HTML expressions to be
--- formatted (if not empty) as some HTML list.
genFuncPropComments :: AnaInfo -> QName -> [CRule] -> [COpDecl] -> [[HtmlExp]]
genFuncPropComments anainfo fname rules ops =
   filter (/=[]) [genFixityInfo fname ops,
                  completenessInfo,
                  indeterminismInfo,
                  opcompleteInfo,
                  externalInfo rules]
 where
   -- comment about the definitional completeness of a function:
   completenessInfo = let ci = getCompleteInfo anainfo fname in
     if ci == Complete
     then []
     else [htxt (if ci == InComplete
                 then "partially defined"
                 else
             "partially defined in each disjunction (but might be complete)")]

   -- comment about the indeterminism of a function:
   indeterminismInfo = if getIndetInfo anainfo fname
                       then [htxt "might behave indeterministically"]
                       else []

   -- comment about the indeterminism of a function:
   opcompleteInfo =
      if getOpCompleteInfo anainfo fname
      then [htxt "solution complete, i.e., able to compute all solutions"]
      else []

   -- comment about the external definition of a function:
   externalInfo []    = [htxt "externally defined"]
   externalInfo (_:_) = []


--- Generates a comment about the associativity and precedence
--- if the name is defined as an infix operator.
genFixityInfo :: QName -> [COpDecl] -> [HtmlExp]
genFixityInfo fname ops =
    concatMap (\(COp n fix prec)->
                  if n == fname
                  then [htxt ("defined as "++showFixity fix++
                              " infix operator with precedence "++show prec)]
                  else [])
              ops
 where
  showFixity CInfixOp  = "non-associative"
  showFixity CInfixlOp = "left-associative"
  showFixity CInfixrOp = "right-associative"


--------------------------------------------------------------------------
-- Pretty printer for types in Curry syntax:
-- second argument is True iff brackets must be written around complex types
showType :: String -> Bool -> CTypeExpr -> String
showType _ _ (CTVar (i,_)) = [chr (97+i)] -- TODO: use name given in source program instead?
showType mod nested (CFuncType t1 t2) =
   brackets nested
    (showType mod (isFunctionalType t1) t1 ++ " -&gt; " ++ showType mod False t2)
showType mod nested (CTCons tc ts)
 | ts==[]  = showTypeCons mod tc
 | tc==("Prelude","[]") && (head ts == CTCons ("Prelude","Char") [])
   = "String"
 | tc==("Prelude","[]")
   = "[" ++ showType mod False (head ts) ++ "]" -- list type
 | take 2 (snd tc) == "(,"                      -- tuple type
   = "(" ++ concat (intersperse "," (map (showType mod False) ts)) ++ ")"
 | otherwise
   = brackets nested
      (showTypeCons mod tc ++ " " ++
       concat (intersperse " " (map (showType mod True) ts)))

showTypeCons :: String -> QName -> String
showTypeCons mod (mtc,tc) =
  if mtc == "Prelude"
  then tc --"<a href=\"Prelude.html#"++tc++"\">"++tc++"</a>"
  else
    if mod == mtc
    then "<a href=\"#"++tc++"\">"++tc++"</a>"
    else "<a href=\""++mtc++".html#"++tc++"\">"++tc++"</a>"


--------------------------------------------------------------------------
-- translate source file into HTML file with syntax coloring
translateSource2ColoredHtml :: String -> String -> IO ()
translateSource2ColoredHtml docdir modname = do
    let output = docdir </> modname++"_curry.html"         
    putStrLn ("Writing source file as HTML to \""++output++"\"...") 
    callFrontendWithParams HTML
      (setQuiet True (setHtmlDir docdir defaultParams)) modname

-- translate source file into HTML file with anchors for each function:
translateSource2AnchoredHtml :: String -> String -> IO ()
translateSource2AnchoredHtml docdir modname =
 do putStrLn ("Writing source file as HTML to \""++docdir++"/"++modname++"_curry.html\"...")
    prog <- readFile (modname++".curry")
    writeFile (docdir </> modname++"_curry.html")
              (showPageWithDocStyle (modname++".curry")
                  [HtmlStruct "pre" []
                     [HtmlText (addFuncAnchors [] (lines prog))]])

-- add the anchors to the classified lines and translate back:
-- first argument: list of already added anchors
-- second argument: list of source lines
addFuncAnchors :: [String] -> [String] -> String
addFuncAnchors _ [] = ""
addFuncAnchors ancs (sl : sls) = let id1 = getFirstId sl in
  if id1=="" ||
     id1 `elem` ["data","type","import","module","infix","infixl","infixr"]
  then htmlQuote (sl++"\n") ++ addFuncAnchors ancs sls
  else if id1 `elem` ancs
       then (sl++"\n") ++ addFuncAnchors ancs sls
       else "<a name=\""++id1++"\"></a>"
            ++ htmlQuote (sl++"\n")
            ++ addFuncAnchors (id1:ancs) sls


--------------------------------------------------------------------------
-- generate the index page for the documentation directory:
genMainIndexPage :: String -> [String] -> IO ()
genMainIndexPage docdir modnames =
 do putStrLn ("Writing index page to \""++docdir++"/index.html\"...")
    simplePage "Documentation of Curry modules"
      (Just $
       if length modnames == 1
       then [htxt "Documentation of the Curry program ",
             href (head modnames++".html") [htxt (head modnames++".curry")]]
       else [htxt "Documentation of Curry programs"])
      allConsFuncsMenu (indexPage modnames)
     >>= writeFile (docdir++"/index.html")

allConsFuncsMenu :: [[HtmlExp]]
allConsFuncsMenu =
  [[href "findex.html" [htxt "All operations"]],
   [href "cindex.html" [htxt "All constructors"]]]

indexPage :: [String] -> [HtmlExp]
indexPage modnames =
  (if length modnames == 1
   then []
   else [ulist (map (\m->[href (m++".html") [htxt (m++".curry ")]])
                    (mergeSort leqStringIgnoreCase modnames))]) ++
  [bold [htxt "Explanations of the icons used in the documentation:"],
   table
     [[[anchor "det_explain" [detIcon]],[nbsp],
       [htxt " Operation is deterministic, i.e., defined by exclusive rules",
        htxt " and depend only on deterministic operations"]]
     ,[[anchor "nondet_explain" [nondetIcon]],[nbsp],
       [htxt " Operation might be non-deterministic, i.e., it is defined by",
        htxt " overlapping rules or depend on non-deterministic operations"]]
--      ,[[anchor "rigid_explain" [rigidIcon]],[nbsp],
--        [htxt " Operation is rigid"]]
--      ,[[anchor "flex_explain" [flexibleIcon]],[nbsp],
--        [htxt " Operation is flexible"]]
--      ,[[anchor "flexrigid_explain" [flexrigidIcon]],[nbsp],
--        [htxt " Operation is partially flexible and partially rigid"]]
     ]
   ]

detIcon :: HtmlExp
detIcon       = italic [] `addClass` "fa fa-long-arrow-down"
                  `withTitle` "This operation is deterministic"
nondetIcon :: HtmlExp
nondetIcon    = italic [] `addClass` "fa fa-arrows-alt"
                  `withTitle` "This operation might be non-deterministic"
-- rigidIcon :: HtmlExp
-- rigidIcon     = italic [] `addClass` "fa fa-cogs"
--                   `withTitle` "This operation is rigid"
-- flexibleIcon :: HtmlExp
-- flexibleIcon  = italic [] `addClass` "fa fa-pagelines"
--                   `withTitle` "This operation is flexible"
-- flexrigidIcon :: HtmlExp
-- flexrigidIcon = italic [] `addClass` "fa fa-exclamation-triangle"
--     `withTitle` "This operation is partially flexible and partially rigid"

withTitle :: HtmlExp -> String -> HtmlExp
withTitle he t = he `addAttr` ("title",t)

--------------------------------------------------------------------------
-- generate the function index page for the documentation directory:
genFunctionIndexPage :: String -> [FC.FuncDecl] -> IO ()
genFunctionIndexPage docdir funs = do
  putStrLn ("Writing operation index page to \""++docdir++"/findex.html\"...")
  simplePage "Index to all operations" Nothing allConsFuncsMenu
             (htmlFuncIndex (sortNames expfuns))
    >>= writeFile (docdir++"/findex.html")
 where
   expfuns = map FCG.funcName $ filter ((== FC.Public) . FCG.funcVisibility) funs

htmlFuncIndex :: [QName] -> [HtmlExp]
htmlFuncIndex qnames = categorizeByItemKey (map showModNameRef qnames)
   
showModNameRef :: QName -> (String,[HtmlExp])
showModNameRef (modname,name) =
  (name,
   [href (modname++".html#"++name) [htxt name], nbsp, nbsp,
    htxt "(", href (getLastName modname++".html") [htxt modname], htxt ")"]
  )

sortNames :: [(a,String)] -> [(a,String)]
sortNames names = mergeSort (\(_,n1) (_,n2)->leqStringIgnoreCase n1 n2) names


--------------------------------------------------------------------------
-- generate the constructor index page for the documentation directory:
genConsIndexPage :: String -> [FC.TypeDecl] -> IO ()
genConsIndexPage docdir types = do
  putStrLn ("Writing constructor index page to \""++docdir++"/cindex.html\"...")
  simplePage "Index to all constructors" Nothing allConsFuncsMenu
             (htmlConsIndex (sortNames expcons))
    >>= writeFile (docdir++"/cindex.html")
 where
   consDecls (FC.Type    _ _ _ cs) = cs
   consDecls (FC.TypeSyn _ _ _ _ ) = []
   expcons = map FCG.consName $ filter ((== FC.Public) . FCG.consVisibility) $
     concatMap consDecls types

htmlConsIndex :: [QName] -> [HtmlExp]
htmlConsIndex qnames = categorizeByItemKey (map showModNameRef qnames)

--------------------------------------------------------------------------
-- generate the index page categorizing all system libraries of PAKCS/KICS2
genSystemLibsPage :: String -> [Category] -> [[ModInfo]] -> IO ()
genSystemLibsPage docdir cats modInfos = do
  putStrLn $ "Writing main index page for " ++ currySystem ++
             " to \"" ++ fname ++ "\"..."
  mainPage (currySystem ++ " Libraries")
           [h1 [htxt $ currySystem ++ ": System Libraries"]]
           syslibsLeftTopMenu
           syslibsRightTopMenu
           3
           (syslibsSideMenu cats)
           ([infoTxt, hrule] ++ genHtmlLibCats modInfos)
           True
   >>= writeFile fname
 where
  fname = docdir ++ "/" ++ currySystem ++ "_libs.html"

syslibsLeftTopMenu :: [[HtmlExp]]
syslibsLeftTopMenu =
  [ [href baseURL [htxt currySystem]]
  , [href (baseURL ++ "/Manual.pdf") [htxt "Manual (PDF)"]]
  , [href (baseURL ++ "/lib/") [htxt "Libraries"]]
  , [ehref currygleURL [extLinkIcon, htxt " API Search"]]
  , [href (baseURL ++ "/download.html") [htxt "Download"]]
  ]

syslibsRightTopMenu :: [[HtmlExp]]
syslibsRightTopMenu =
  [ curryHomeItem
  , [ehref (curryHomeURL ++ "/documentation/report")
           [extLinkIcon, htxt " Curry Report"]]
  ]

syslibsSideMenu :: [Category] -> [HtmlExp]
syslibsSideMenu cats = map par $
     [[ehref currygleURL [extLinkIcon, htxt " Search with Curr(y)gle"]]]
  ++ [[href ("#" ++ genCatLink c) [ htxt (showCategory c)]] | c <- cats]
  ++ [ [href "findex.html" [htxt "Index to all library functions"]]
     , [href "cindex.html" [htxt "Index to all library constructors"]]
     ]

infoTxt :: HtmlExp
infoTxt = par
  [ htxt "Here is the collection of libraries contained in the distribution of "
  , href baseURL [htxt currySystem]
  , htxt $ ". Most of these libraries have been implemented during the "
        ++ "development of larger Curry applications. If you have suggestions "
        ++ "for changes/improvements or if you want to contribute your own "
        ++ "library, please contact "
  , href "http://www.informatik.uni-kiel.de/~mh/" [htxt "Michael Hanus"]
  , htxt "."
  ]

-- Generate links for a category (system libraries page)
genCatLink :: Category -> String
genCatLink cat = getCategoryID cat

genHtmlLibCats :: [[ModInfo]] -> [HtmlExp]
genHtmlLibCats [] = []
genHtmlLibCats (cat:cats) = case cat of
  []          -> []
  ((c,_,_):_) ->
       [anchoredSection (getCategoryID c) [h2 [htxt (showCategory c ++ ":")]]]
    ++ genHtmlLibCat cat
    ++ genHtmlLibCats cats

genHtmlLibCat :: [ModInfo] -> [HtmlExp]
genHtmlLibCat category =
  [dlist [(genHtmlName modname,[htxt modcmt]) | (_,modname,modcmt) <- category ]]
 where
  genHtmlName modname = [code [href (modname ++ ".html") [htxt modname]]]

--------------------------------------------------------------------------
-- Auxiliary operation for general page style.

--- Generate the main page with the default documentation style.
--- @param title - the title of the page
--- @param htmltitle - the title in HTML format (shown as h1)
--- @param lefttopmenu - the menu shown at left of the top
--- @param righttopmenu - the menu shown at right of the top
--- @param columns - number of columns for the left-side menu
--- @param sidemenu - the menu shown at the left-hand side
--- @param maindoc - the main contents of the page
--- @param isIndex - flag to generate libs index page
mainPage :: String -> [HtmlExp] -> [[HtmlExp]] -> [[HtmlExp]] -> Int
         -> [HtmlExp] -> [HtmlExp] -> Bool -> IO String
mainPage title htmltitle lefttopmenu righttopmenu columns sidemenu maindoc isIndex = do
    time <- getLocalTime
    return $ showHtmlPage $
      bootstrapPage baseURL cssIncludes
                    title lefttopmenu righttopmenu columns sidemenu htmltitle maindoc
                    (curryDocFooter time) isIndex

cssIncludes :: [String]
cssIncludes = ["bootstrap","bootstrap-responsive","font-awesome.min","currydoc"]

--- Generate a page with the default documentation style.
--- @param title - the title of the page
--- @param body  - the main contents of the page
showPageWithDocStyle :: String -> [HtmlExp] -> String
showPageWithDocStyle title body =
  showHtmlPage $
    HtmlPage title
             (map (\f -> pageCSS $ baseURL++"/css/"++f++".css") cssIncludes)
             body

--- The standard right top menu.
rightTopMenu :: [[HtmlExp]]
rightTopMenu =
  [ curryHomeItem
  , [ehref (baseURL++"/lib/")
           [extLinkIcon, htxt $ " "++currySystem++" Libraries"]]
  , [ehref (curryHomeURL ++ "/tools/currydoc")
           [extLinkIcon, htxt " About CurryDoc"]]
  ]

extLinkIcon :: HtmlExp
extLinkIcon = italic [] `addClass` "fa fa-external-link"

-- Standard footer information for generated web pages:
curryDocFooter :: CalendarTime -> [HtmlExp]
curryDocFooter time =
  [italic [htxt "Generated by ",
           bold [htxt "CurryDoc"],
           htxt (" ("++currydocVersion++") at "),
           htxt (calendarTimeToString time)]]

curryHomeItem :: [HtmlExp]
curryHomeItem = [ehref curryHomeURL [extLinkIcon, htxt " Curry Homepage"]]

--- Generate a simple page with the default documentation style.
--- @param title - the title of the page
--- @param htmltitle - maybe a specific title for h1 header
--- @param lefttopmenu - the menu shown at left of the top
--- @param doc - the main contents of the page
simplePage :: String -> Maybe [HtmlExp] -> [[HtmlExp]] -> [HtmlExp] -> IO String
simplePage title htmltitle lefttopmenu maindoc = do
    time <- getLocalTime
    return $ showHtmlPage $
      bootstrapPage baseURL cssIncludes
                    title lefttopmenu rightTopMenu 0 []
                    [h1 (maybe [htxt title] id htmltitle)]
                    maindoc
                    (curryDocFooter time)
                    False

--- An anchored section in the document:
anchoredSection :: String -> [HtmlExp] -> HtmlExp
anchoredSection tag doc = HtmlStruct "section" [("id",tag)] doc

--- An anchored element in the document:
anchored :: String -> [HtmlExp] -> HtmlExp
anchored tag doc = style "anchored" doc `addAttr` ("id",tag)

--- A bordered table:
borderedTable :: [[[HtmlExp]]] -> HtmlExp
borderedTable rows = table rows `addClass` "table table-bordered table-hover"

--- An external reference
ehref :: String -> [HtmlExp] -> HtmlExp
ehref url desc = href url desc `addAttr` ("target","_blank")

--------------------------------------------------------------------------
-- auxiliaries:

-- style for explanation categories, like "Constructors:", "Parameters:",...
explainCat :: String -> HtmlExp
explainCat s = textstyle "explaincat" s

-- style for function/constructor name shown in the documentation part:
opnameDoc :: [HtmlExp] -> HtmlExp
opnameDoc = style "opname"

-- Sorts a list of strings.
sortStrings :: [String] -> [String]
sortStrings strings = mergeSort leqStringIgnoreCase strings

-- Returns the first sentence in a string:
firstSentence :: String -> String
firstSentence s = let (fs,ls) = break (=='.') s in
  if null ls
  then fs
  else if tail ls /= "" && isWhiteSpace (head (tail ls))
       then fs ++ "."
       else fs ++ "." ++ firstSentence (tail ls)

firstPassage :: String -> String
firstPassage = unlines . takeWhile (\s -> s /= "" && not (all isWhiteSpace s))
             . lines

--------------------------------------------------------------------------
