----------------------------------------------------------------------
--- This module contains some configuration parameters for
--- the CurryDoc tool.
---
--- @author Michael Hanus, Jan Tikovsky
--- @version June 2015
----------------------------------------------------------------------

module CurryDocConfig where

import Distribution(curryCompiler)

--- Version of currydoc
currydocVersion :: String
currydocVersion = "Version 0.8.0 of June 4, 2015"

--- The URL of the base directory containing the styles, images, etc.
baseURL :: String
baseURL = if curryCompiler=="pakcs"
          then "http://www.informatik.uni-kiel.de/~pakcs"
          else "http://www-ps.informatik.uni-kiel.de/kics2"

--- The name of this Curry system.
currySystem :: String
currySystem = if curryCompiler=="pakcs" then "PAKCS" else "KiCS2"

--- The URL of the API search
currygleURL :: String
currygleURL = "https://www-ps.informatik.uni-kiel.de/kics2/currygle/"

--- The URL of the Curry homepage
curryHomeURL :: String
curryHomeURL = "http://www.curry-language.org"
