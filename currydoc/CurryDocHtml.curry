----------------------------------------------------------------------
--- Functions to generate documentation in HTML format.
---
--- @author Michael Hanus
--- @version March 2013
----------------------------------------------------------------------

module CurryDocHtml where

import CurryDocParams
import CurryDocRead
import CurryDocConfig
import TotallyDefined(Completeness(..))
import FlatCurry
import FlexRigid
import HTML
import BootstrapStyle
import List
import Char
import Sort
import Time
import Distribution
import CategorizedHtmlList
import Markdown

-- Name of style sheet for documentation files:
currydocCSS = "currydoc.css"

--------------------------------------------------------------------------
-- Generates the documentation of a module in HTML format where the comments
-- are already analyzed.
generateHtmlDocs :: DocParams -> AnaInfo -> String -> String
                 -> [(SourceLine,String)] -> IO String
generateHtmlDocs docparams anainfo progname modcmts progcmts = do
  let fcyname = flatCurryFileName progname
  putStrLn $ "Reading FlatCurry program \""++fcyname++"\"..."
  (Prog _ imports types functions ops) <- readFlatCurryFile fcyname
  let exptypes = filter isExportedType types
      expfuns  = filter isExportedFun functions
  mainPage title htmltitle (lefttopmenu types)
           [bold [htxt "Exported names:"],
            genHtmlExportIndex (map typeName exptypes)
                               (getExportedCons types)
                               (map funcName expfuns),
            anchored "imported_modules" [bold [htxt "Imported modules:"]],
            ulist (map (\i -> [href (getLastName i++".html") [htxt i]])
                       imports) `addClass` "nav nav-list"]
    (genHtmlModule docparams modcmts ++
       ([h2 [htxt "Summary of exported functions:"],
         borderedTable
           (map (genHtmlFuncShort docparams progcmts anainfo) expfuns)] ++
        (if null types
         then []
         else [anchoredSection "exported_datatypes"
                               [h2 [htxt "Exported datatypes:"]], hrule] ++
               concatMap (genHtmlType docparams progcmts) exptypes) ++
        [anchored "exported_functions"
                  [h2 [htxt "Exported functions:"]]] ++
        (map (genHtmlFunc docparams progname progcmts anainfo ops) expfuns)))
 where
   title = "Module "++getLastName progname

   htmltitle = [h1 [htxt "Module ",
                    href (getLastName progname++"_curry.html")
                         [htxt (getLastName progname++".curry")]]]

   lefttopmenu types =
     [[href "?" [htxt title]],
      [href "#imported_modules" [htxt "Imports"]]] ++
     (if null types then []
      else [[href "#exported_datatypes" [htxt "Datatypes"]]]) ++
     [[href "#exported_functions" [htxt "Functions"]]]


--- Translate a documentation comment to HTML and use markdown translation
--- if necessary.
docComment2HTML :: DocParams -> String -> [HtmlExp]
docComment2HTML docparams cmt =
  if withMarkdown docparams
  then markdownText2HTML (replaceIdLinks cmt)
  else [HtmlText (replaceIdLinks cmt)]

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
genHtmlExportIndex :: [String] -> [String] -> [String] -> HtmlExp
genHtmlExportIndex exptypes expcons expfuns =
  HtmlStruct "ul" [("class","nav nav-list")]
    (concatMap (\ (htmlnames,cattitle) ->
                 if null htmlnames
                 then []
                 else HtmlStruct "li" [("class","nav-header")] [htxt cattitle] :
                      map (HtmlStruct "li" []) htmlnames)
            [(htmltypes,"Datatypes:"),
             (htmlcons ,"Constructors:"),
             (htmlfuns ,"Functions:")])
 where
  htmltypes = map (\n->[href ('#':n) [htxt n]])
                  (nub (sortStrings exptypes))
  htmlcons  = map (\n->[href ('#':n++"_CONS") [htxt n]])
                  (nub (sortStrings expcons))
  htmlfuns  = map (\n->[href ('#':n) [htxt n]])
                  (nub (sortStrings expfuns))


typeName :: TypeDecl -> String
typeName (Type    (_,name) _ _ _) = name
typeName (TypeSyn (_,name) _ _ _) = name

isExportedType :: TypeDecl -> Bool
isExportedType (Type    _ vis _ _) = vis==Public
isExportedType (TypeSyn _ vis _ _) = vis==Public

-- extract the names of all exported constructors
getExportedCons :: [TypeDecl] -> [String]
getExportedCons types =
   map (\(Cons (_,name) _ _ _)->name)
       (filter (\(Cons _ _ vis _)->vis==Public) (concatConsDecls types))
 where
   concatConsDecls [] = []
   concatConsDecls (TypeSyn _ _ _ _ : ts) = concatConsDecls ts
   concatConsDecls (Type _ _ _ cdcls : ts) = cdcls ++ concatConsDecls ts

funcName :: FuncDecl -> String
funcName (Func (_,name) _ _ _ _) = name

isExportedFun :: FuncDecl -> Bool
isExportedFun (Func _ _ vis _ _) = vis==Public


--- generate HTML documentation for a module:
genHtmlModule :: DocParams -> String -> [HtmlExp]
genHtmlModule docparams modcmts =
  let (maincmt,avcmts) = splitComment modcmts
   in [par (docComment2HTML docparams maincmt)] ++
      map (\a->par [bold [htxt "Author: "], htxt a])
          (getCommentType "author" avcmts) ++
      map (\a->par [bold [htxt "Version: "], htxt a])
          (getCommentType "version" avcmts)

--- generate HTML documentation for a datatype if it is exported:
genHtmlType :: DocParams -> [(SourceLine,String)] -> TypeDecl -> [HtmlExp]
genHtmlType docparams progcmts (Type (_,tcons) _ tvars constrs) =
  let (datacmt,conscmts) = splitComment (getDataComment tcons progcmts)
   in [anchored tcons [style "typeheader" [htxt tcons]],
       par (docComment2HTML docparams datacmt),
       par [explainCat "Constructors:"]] ++
      concatMap (genHtmlCons (getCommentType "cons" conscmts)) constrs ++
      [hrule]
 where
  genHtmlCons conscmts (Cons (cmod,cname) _ cvis argtypes) =
    if cvis==Public
    then [style "anchored"
           [code [opnameDoc [htxt cname],
                  HtmlText (" :: " ++
                            concatMap (\t->" "++showType cmod True t++" -> ")
                                      argtypes ++
                            tcons ++ concatMap (\i->[' ',chr (97+i)]) tvars)]]
            `addAttr` ("id",cname++"_CONS"),
           maybe (par [])
                 (\ (call,cmt) ->
                    par ([code [htxt call], htxt " : "] ++
                         removeTopPar (docComment2HTML docparams
                                                       (removeDash cmt)))
                      `addClass` "conscomment")
                (getConsComment conscmts cname)]
    else []

genHtmlType docparams progcmts (TypeSyn (tcmod,tcons) _ tvars texp) =
  let (typecmt,_) = splitComment (getDataComment tcons progcmts)
   in [anchored tcons [style "typeheader" [htxt tcons]],
       par (docComment2HTML docparams typecmt),
       par [explainCat "Type synonym:", nbsp,
            if tcons=="String" && tcmod=="Prelude"
            then code [htxt "String = [Char]"]
            else code [HtmlText
                        (tcons ++ concatMap (\i->[' ',chr (97+i)]) tvars ++
                         " = " ++ showType tcmod False texp)]],
       hrule]

-- generate short HTML documentation for a function:
genHtmlFuncShort docparams progcmts anainfo
                 (Func (fmod,fname) _ _ ftype rule) =
 [[code [opnameDoc
            [anchor (fname++"_SHORT")
                    [href ('#':fname) [htxt (showId fname)]]],
           HtmlText (" :: " ++ showType fmod False ftype)],
     nbsp, nbsp]
     ++ genFuncPropIcons anainfo (fmod,fname) rule ++
  [breakline] ++
   removeTopPar
      (docComment2HTML docparams
         (firstSentence (fst (splitComment
                                (getFuncComment fname progcmts)))))]

-- generate HTML documentation for a function:
genHtmlFunc :: DocParams -> String -> [(SourceLine,String)] -> AnaInfo
            -> [OpDecl] -> FuncDecl -> HtmlExp
genHtmlFunc docparams progname progcmts anainfo ops
            (Func (fmod,fname) _ _ ftype rule) =
  let (funcmt,paramcmts) = splitComment (getFuncComment fname progcmts)
   in style "anchored"
       [borderedTable [[
         [par $
           [code [opnameDoc
                   [href (getLastName progname++"_curry.html#"++fname)
                         [htxt (showId fname)]],
                  HtmlText (" :: "++ showType fmod False ftype)],
            nbsp, nbsp] ++
           genFuncPropIcons anainfo (fmod,fname) rule] ++
         docComment2HTML docparams funcmt ++
         genParamComment paramcmts ++
         -- show further infos for this function, if present:
         (if furtherInfos == []
          then []
          else [dlist [([explainCat "Further infos:"],
                        [ulist furtherInfos])]] )]]]
       `addAttr` ("id",fname)
 where
  furtherInfos = genFuncPropComments anainfo (fmod,fname) rule ops

  genParamComment paramcmts =
    let params = map (span isIdChar) (getCommentType "param" paramcmts)
     in (if params==[]
         then []
         else [par [explainCat "Example call:", nbsp,
                    code [htxt (showCall fname (map fst params))]],
               dlist ([([explainCat "Parameters:"],[])] ++
                      map (\(parid,parcmt)->
                            ([],[code [htxt parid], htxt " : "] ++
                                removeTopPar (docComment2HTML docparams
                                                       (removeDash parcmt))))
                          params)
              ]) ++
         [dlist (map (\rescmt ->
                         ([explainCat "Returns:"],
                          removeTopPar (docComment2HTML docparams rescmt)))
                     (getCommentType "return" paramcmts))
         ]

  showCall f params =
    if isAlpha (head f) || length params /= 2
    then "(" ++ showId f ++ concatMap (" "++) params ++ ")"
    else "(" ++ params!!0 ++ " " ++ f ++ " " ++ params!!1 ++ ")"

-- remove initial dash sign (of a parameter comment)
removeDash s = let ds = dropWhile isSpace s in
  if take 2 ds == "- " then dropWhile isSpace (drop 2 ds)
                       else ds

-- remove a single top-level paragraph in HTML expressions:
removeTopPar hexps = case hexps of
  [HtmlStruct "p" [] hs] -> hs
  _ -> hexps

--------------------------------------------------------------------------
--- Generates icons for particular properties of functions.
genFuncPropIcons anainfo fname rule =
   [detIcon, nbsp, flexRigidIcon rule]
 where
   --(non)deterministically defined property:
   detIcon =
    if getNondetInfo anainfo fname
    then href "index.html#nondet_explain"
              [addIconParams $ image "nondet.gif" "non-deterministic"]
    else href "index.html#det_explain"
              [addIconParams $ image "det.gif" "deterministic"]

   -- icon for rigid/flexible:
   flexRigidIcon (External _) = htxt ""
   flexRigidIcon (Rule _ rhs) = imageEvalAnnot (getFlexRigid rhs)
    where
      imageEvalAnnot ConflictFR = -- mixed rigid flexible
          href "index.html#flexrigid_explain"
               [addIconParams $ image "flexrigid.gif" "flexible+rigid"]
      imageEvalAnnot UnknownFR  = htxt ""
      imageEvalAnnot KnownRigid =
          href "index.html#rigid_explain"
               [addIconParams $ image "rigid.gif" "rigid"]
      imageEvalAnnot KnownFlex  =
          href "index.html#flex_explain"
               [addIconParams $ image "flex.gif" "flexible"]

addIconParams hicon = hicon `addAttr` ("align","middle")
                            `addAttr` ("border","0")

--------------------------------------------------------------------------
--- Generates further textual infos about particular properties
--- of a function. The result is a list of HTML expressions to be
--- formatted (if not empty) as some HTML list.
genFuncPropComments anainfo fname rule ops =
   filter (/=[]) [genFixityInfo fname ops,
                  completenessInfo,
                  indeterminismInfo,
                  opcompleteInfo,
                  externalInfo rule]
 where
   -- comment about the definitional completeness of a function:
   completenessInfo = let ci = getCompleteInfo anainfo fname in
     if ci==Complete
     then []
     else [htxt (if ci==InComplete
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
   externalInfo (External _) = [htxt "externally defined"]
   externalInfo (Rule _ _)   = []


--- Generates a comment about the associativity and precedence
--- if the name is defined as an infix operator.
genFixityInfo fname ops =
    concatMap (\(Op n fix prec)->
                  if n==fname
                  then [htxt ("defined as "++showFixity fix++
                              " infix operator with precedence "++show prec)]
                  else [])
              ops
 where
  showFixity InfixOp  = "non-associative"
  showFixity InfixlOp = "left-associative"
  showFixity InfixrOp = "right-associative"


--------------------------------------------------------------------------
-- Pretty printer for types in Curry syntax:
-- second argument is True iff brackets must be written around complex types
showType :: String -> Bool -> TypeExpr -> String
showType _ _ (TVar i) = [chr (97+i)]
showType mod nested (FuncType t1 t2) =
   brackets nested
    (showType mod (isFunctionType t1) t1 ++ " -&gt; " ++ showType mod False t2)
showType mod nested (TCons tc ts)
 | ts==[]  = showTypeCons mod tc
 | tc==("Prelude","[]") && (head ts == TCons ("Prelude","Char") [])
   = "String"
 | tc==("Prelude","[]")
   = "[" ++ showType mod False (head ts) ++ "]" -- list type
 | take 2 (snd tc) == "(,"                      -- tuple type
   = "(" ++ concat (intersperse "," (map (showType mod False) ts)) ++ ")"
 | otherwise
   = brackets nested
      (showTypeCons mod tc ++ " " ++
       concat (intersperse " " (map (showType mod True) ts)))

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
translateSource2ColoredHtml docdir progname = do
    let output = docdir++"/"++getLastName progname++"_curry.html"         
    putStrLn ("Writing source file as HTML to \""++output++"\"...") 
    callFrontendWithParams HTML
      (setQuiet True (setOutfile output defaultParams)) progname

-- translate source file into HTML file with anchors for each function:
translateSource2AnchoredHtml :: String -> String -> IO ()
translateSource2AnchoredHtml docdir progname =
 do putStrLn ("Writing source file as HTML to \""++docdir++"/"++getLastName progname++"_curry.html\"...")
    prog <- readFile (progname++".curry")
    writeFile (docdir++"/"++getLastName progname++"_curry.html")
              (showPageWithDocStyle (progname++".curry")
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

allConsFuncsMenu =
  [[href "findex.html" [htxt "All functions"]],
   [href "cindex.html" [htxt "All constructors"]]]

indexPage modnames =
  (if length modnames == 1
   then []
   else [ulist (map (\m->[href (m++".html") [htxt (m++".curry ")]])
                    (mergeSort leqStringIgnoreCase modnames))]) ++
  [bold [htxt "Explanations of the icons used in the documentation:"],
   par [anchor "det_explain" [image "det.gif" "deterministic"],
        htxt " Function is deterministic, i.e., defined by exclusive rules",
        htxt " and depend only on deterministic functions"],
   par [anchor "nondet_explain" [image "nondet.gif" "non-deterministic"],
        htxt " Function might be non-deterministic, i.e., it is defined by",
        htxt " overlapping rules or depend on non-deterministic functions"],
   par [anchor "rigid_explain" [image "rigid.gif" "rigid"],
        htxt " Function is rigid"],
   par [anchor "flex_explain" [image "flex.gif" "flexible"],
        htxt " Function is flexible"],
   par [anchor "flexrigid_explain" [image "flexrigid.gif" "flexible+rigid"],
        htxt " Function is partially flexible and partially rigid"]
   --par [image "impl.gif" "implementation",
   --     htxt " Reference to the implementation of the module or function"]
  ]
   

--------------------------------------------------------------------------
-- generate the function index page for the documentation directory:
genFunctionIndexPage docdir funs = do
  putStrLn ("Writing function index page to \""++docdir++"/findex.html\"...")
  simplePage "Index to all functions" Nothing allConsFuncsMenu
             (htmlFuncIndex (sortNames expfuns))
    >>= writeFile (docdir++"/findex.html")
 where
   expfuns = map (\(Func name _ _ _ _)->name)
                 (filter (\(Func _ _ vis _ _)->vis==Public) funs)

htmlFuncIndex :: [(String,String)] -> [HtmlExp]
htmlFuncIndex qnames = categorizeByItemKey (map showModNameRef qnames)
   
showModNameRef :: (String,String) -> (String,[HtmlExp])
showModNameRef (modname,name) =
  (name,
   [href (modname++".html#"++name) [htxt name], nbsp, nbsp,
    htxt "(", href (getLastName modname++".html") [htxt modname], htxt ")"]
  )

sortNames names = mergeSort (\(_,n1) (_,n2)->leqStringIgnoreCase n1 n2) names


--------------------------------------------------------------------------
-- generate the constructor index page for the documentation directory:
genConsIndexPage docdir types = do
  putStrLn ("Writing constructor index page to \""++docdir++"/cindex.html\"...")
  simplePage "Index to all constructors" Nothing allConsFuncsMenu
             (htmlConsIndex (sortNames expcons))
    >>= writeFile (docdir++"/cindex.html")
 where
   expcons = map (\(Cons name _ _ _)->name)
                 (filter (\(Cons _ _ vis _)->vis==Public)
                         (concatMap getCons types))

   getCons (Type _ _ _ cdecls) = cdecls
   getCons (TypeSyn _ _ _ _) = []

htmlConsIndex qnames = categorizeByItemKey (map showModNameRef qnames)


--------------------------------------------------------------------------
-- Auxiliary operation for general page style.

--- Generate the main page with the default documentation style.
--- @param title - the title of the page
--- @param htmltitle - the title in HTML format (shown as h1)
--- @param lefttopmenu - the menu shown at left of the top
--- @param sidemenu - the menu shown at the left-hand side
--- @param doc - the main contents of the page
mainPage :: String -> [HtmlExp] -> [[HtmlExp]] -> [HtmlExp] -> [HtmlExp]
         -> IO String
mainPage title htmltitle lefttopmenu sidemenu maindoc = do
    time <- getLocalTime
    return $ showHtmlPage $
      bootstrapPage baseURL
                    ["bootstrap","bootstrap-responsive","currydoc"]
                    title lefttopmenu rightTopMenu 3 sidemenu htmltitle maindoc
                    (curryDocFooter time)

--- Generate a page with the default documentation style.
--- @param title - the title of the page
--- @param body  - the main contents of the page
showPageWithDocStyle :: String -> [HtmlExp] -> String
showPageWithDocStyle title body =
  showHtmlPage $
    HtmlPage title
             (map (\f -> pageCSS $ baseURL++"/css/"++f++".css")
                  ["bootstrap","bootstrap-responsive","currydoc"])
             body

--- The standard right top menu.
rightTopMenu =
  [[ehref "http://www.curry-language.org/" [htxt "Curry Homepage"]],
   [ehref (baseURL++"/lib/") [htxt $ currySystem++" Libraries"]],
   [ehref "http://www.curry-language.org/tools/currydoc"
         [htxt "About CurryDoc"]]]
  

-- Standard footer information for generated web pages:
curryDocFooter time =
  [italic [htxt "Generated by ",
           bold [htxt "CurryDoc"],
           htxt (" ("++currydocVersion++") at "),
           htxt (calendarTimeToString time)]]

--- Generate a simple page with the default documentation style.
--- @param title - the title of the page
--- @param htmltitle - maybe a specific title for h1 header
--- @param lefttopmenu - the menu shown at left of the top
--- @param doc - the main contents of the page
simplePage :: String -> Maybe [HtmlExp] -> [[HtmlExp]] -> [HtmlExp] -> IO String
simplePage title htmltitle lefttopmenu maindoc = do
    time <- getLocalTime
    return $ showHtmlPage $
      bootstrapPage baseURL
                    ["bootstrap","bootstrap-responsive","currydoc"]
                    title lefttopmenu rightTopMenu 0 []
                    [h1 (maybe [htxt title] id htmltitle)]
                    maindoc
                    (curryDocFooter time)

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
firstSentence s = let (fs,ls) = break (=='.') s in
  if ls==""
  then fs
  else if tail ls /= "" && isWhiteSpace (head (tail ls))
       then fs ++ "."
       else fs ++ "." ++ firstSentence (tail ls)

--------------------------------------------------------------------------
