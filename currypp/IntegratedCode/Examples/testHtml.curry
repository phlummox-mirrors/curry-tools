{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
--- This program contains tests for integrated code to support
--- easy writing of HTML code in Curry programs.
------------------------------------------------------------------------------

import HTML
import Test.EasyCheck

htmlTest1 :: String -> [HtmlExp]
htmlTest1 name = ``html
 <html>

  <head>
   <title>Simple Test

  <body>
   <h1>Hello {name}!</h1>
    <p>
     Bye!
    <p>Bye!
   <h2>{reverse name}
   Bye!''

htmlDoc1 :: [HtmlExp]
htmlDoc1 =
  [HtmlStruct "html" []
    [HtmlStruct "head" []
      [HtmlStruct "title" [] [HtmlText "Simple Test\n"]],
     HtmlStruct "body" []
      [HtmlStruct "h1" []
        [HtmlText "Hello ", HtmlText "Joe", HtmlText "!"],
       HtmlStruct "p" [] [HtmlText "Bye!\n"],
       HtmlStruct "p" [] [HtmlText "Bye!\n"],
       HtmlStruct "h2" []
        [HtmlText "eoJ", HtmlText "\n"],
       HtmlText "Bye!"]]]

------------------------------------------------------------------------------
-- Partial equality on HTML documents for testing.
eqHtml hexp1 hexp2 = case hexp1 of
  HtmlText s -> case hexp2 of HtmlText t -> s == t
                              _          -> False
  HtmlStruct t ats hes -> case hexp2 of
             HtmlStruct t' ats' hes' -> t==t' && ats==ats' && eqHtmls hes hes'
             _                       -> False
  _ -> error "HTML.==: cannot compare cgi refs or handlers"

eqHtmls hes1 hes2 = all (uncurry eqHtml) (zip hes1 hes2)

------------------------------------------------------------------------------

test_Html_code = always (eqHtmls (htmlTest1 "Joe") htmlDoc1)
