----------------------------------------------------------------------
--- This module contains some configuration parameters for
--- the CurryDoc tool.
---
--- @author Michael Hanus
--- @version January 2014
----------------------------------------------------------------------

module CurryDocConfig where

import Distribution(curryCompiler)

--- Version of currydoc
currydocVersion = "Version 0.7.2 of January 21, 2014"

--- The URL of the base directory containing the styles, images, etc.
baseURL = if curryCompiler=="pakcs"
          then "http://www.informatik.uni-kiel.de/~pakcs"
          else "http://www-ps.informatik.uni-kiel.de/kics2"

--- The name of this Curry system.
currySystem = if curryCompiler=="pakcs" then "PAKCS" else "KiCS2"
