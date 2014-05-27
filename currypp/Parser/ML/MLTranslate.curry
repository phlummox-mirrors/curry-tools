------------------------------------------------------------------------------
--- Function to translate Markup Language strings into Curry code strings.
---
--- @author Max Deppert
--- @version March 2014
------------------------------------------------------------------------------
module MLTranslate (translate) where

import ParseError
import ParseTypes
import MLParser
import MLTypes

import List

translate :: String -> LangParser

showText :: Text -> String
showText (Raw s) = show s
showText (Exp s) = "(" ++ s ++ ")"

showData :: [Text] -> String
showData [] = "\"\""
showData ds@(_:_) = "(" ++ intercalate "++" (map showText ds) ++ ")"

showAttr :: Attribute -> String
showAttr (s,ds) = "(" ++ show s ++ "," ++ showData ds ++ ")"

showAttrs :: [Attribute] -> String
showAttrs xs = "[" ++ (intercalate "," (map showAttr xs)) ++ "]"

-- translates a HTML string to a Curry string
translate "html" start s = return $ (warnOKPM (showStringList (map showTree trees)) ws)
  where (trees,ws) = parse H s (toSimplePos start,0)
        showTree :: Tree -> String
        showTree (Tree (Content ds) _) = "HtmlText " ++ showData ds
        showTree (Tree (Element a par) ys) =
          "HtmlStruct " ++ show a ++ " " ++ showAttrs par ++ " " ++ showStringList (map showTree ys)

-- translates a XML string to a Curry string
translate "xml" start s = return $ (warnOKPM (showStringList (map showTree trees)) ws)
  where (trees,ws) = parse X s (toSimplePos start,0)
        showTree :: Tree -> String
        showTree (Tree (Content ds) _) = "XText " ++ showData ds
        showTree (Tree (Element a par) ys) =
          "XElem " ++ show a ++ " " ++ showAttrs par ++ " " ++ showStringList (map showTree ys)

-- 
showStringList :: [String] -> String
showStringList xs = "[" ++ (intercalate "," xs) ++ "]"
