----------------------------------------------------------------------
--- This module contains some configuration parameters for
--- the CurryDoc tool.
---
--- @author Michael Hanus
--- @version January 2015
----------------------------------------------------------------------

module CurryDocConfig where

import Distribution(curryCompiler)

--- Version of currydoc
currydocVersion :: String
currydocVersion = "Version 0.7.3 of January 27, 2015"

--- The URL of the base directory containing the styles, images, etc.
baseURL :: String
baseURL = if curryCompiler=="pakcs"
          then "http://www.informatik.uni-kiel.de/~pakcs"
          else "http://www-ps.informatik.uni-kiel.de/kics2"

--- The name of this Curry system.
currySystem :: String
currySystem = if curryCompiler=="pakcs" then "PAKCS" else "KiCS2"
