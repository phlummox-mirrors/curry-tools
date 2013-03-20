----------------------------------------------------------------------
--- This module contains some configuration parameters for
--- the CurryDoc tool.
---
--- @author Michael Hanus
--- @version February 2013
----------------------------------------------------------------------

module CurryDocConfig where

import Distribution(curryCompiler)

--- Version of currydoc
currydocVersion = "Version 0.7.1 of March 20, 2013"

--- The URL of the base directory containing the styles, images, etc.
baseURL = if curryCompiler=="pakcs"
          then "http://www.informatik.uni-kiel.de/~pakcs"
          else "http://www-ps.informatik.uni-kiel.de/kics2"

--- The name of this Curry system.
currySystem = if curryCompiler=="pakcs" then "PAKCS" else "KiCS2"
