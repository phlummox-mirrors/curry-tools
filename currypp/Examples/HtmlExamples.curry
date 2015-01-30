{-# OPTIONS_CYMAKE -F --pgmF=currypp #-}

------------------------------------------------------------------------------
--- This program contains examples for integrated code to support
--- easy writing of HTML code in Curry programs.
------------------------------------------------------------------------------

import HTML

test1 :: String -> IO ()
test1 name = putStrLn (showHtmlExps ``html
 <html>

  <head>
   <title>Simple Test

  <body>
   <h1>Hello {name}!</h1>
    <p>
     Sorry, i've got nothing to say...but:
   <h2>{(reverse sey)++"we "++"can!"}
   Have a wonderful time!'')

sey = " seY"
