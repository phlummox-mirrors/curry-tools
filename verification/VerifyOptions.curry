-------------------------------------------------------------------------
--- The options of the Curry->Verifier translation tool.
---
--- @author Michael Hanus
--- @version May 2016
-------------------------------------------------------------------------

module VerifyOptions where

import AbstractCurry.Types
import Char              (toLower)
import GetOpt
import ReadNumeric       (readNat)
import GenericProgInfo
import Deterministic     (Deterministic(..))

-------------------------------------------------------------------------
-- Representation of command line options and information relevant
-- for the translation process.
data Options = Options
  { optHelp    :: Bool
  , optVerb    :: Int
  , optStore   :: Bool    -- store result in file?
  , optTarget  :: String
  , isPrimFunc :: (QName -> Bool) -- primitive function? (not translated)
  , primTypes  :: [QName] -- primitive types (not translated)
  , detInfos   :: ProgInfo Deterministic -- info about deteterministic funcs
  , totInfos   :: ProgInfo Bool          -- info about totally defined funcs
  }

-- Default command line options.
defaultOptions :: Options
defaultOptions = Options
  { optHelp    = False
  , optVerb    = 1
  , optStore   = True
  , optTarget  = "agda"
  , isPrimFunc = isUntranslatedFunc
  , primTypes  = defPrimTypes
  , detInfos   = emptyProgInfo
  , totInfos   = emptyProgInfo
  }

-- Primitive functions that are not extracted and translated to the verifier.
isUntranslatedFunc :: QName -> Bool
isUntranslatedFunc qn =
  qn `elem` [pre "?", pre "==", pre "+", pre "*", pre "length"] ||
  fst qn `elem` ["Test.Prop","Test.EasyCheck"]

-- Primitive functions that are not extracted and translated to the verifier.
defPrimTypes :: [QName]
defPrimTypes = [ pre "[]", pre "Bool", pre "Maybe", ("Nat","Nat")
               , ("Test.Prop","Prop"), ("Test.EasyCheck","Prop")]

-- Definition of actual command line options.
options :: [OptDescr (Options -> Options)]
options =
  [ Option "h?" ["help"]  (NoArg (\opts -> opts { optHelp = True }))
           "print help and exit"
  , Option "q" ["quiet"] (NoArg (\opts -> opts { optVerb = 0 }))
           "run quietly (no output, only exit code)"
  , Option "v" ["verbosity"]
            (OptArg (maybe (checkVerb 2) (safeReadNat checkVerb)) "<n>")
            "verbosity level:\n0: quiet (same as `-q')\n1: show progress (default)\n2: show generated output (same as `-v')\n3: show generated output"
  , Option "n" ["nostore"]
           (NoArg (\opts -> opts { optStore = False }))
           "do not store translation (show only)"
  , Option "t" ["target"]
           (ReqArg checkTarget "<t>")
           "translation target:\nAgda (default)"
  ]
 where
  safeReadNat opttrans s opts =
   let numError = error "Illegal number argument (try `-h' for help)" in
    maybe numError
          (\ (n,rs) -> if null rs then opttrans n opts else numError)
          (readNat s)

  checkVerb n opts = if n>=0 && n<4
                     then opts { optVerb = n }
                     else error "Illegal verbosity level (try `-h' for help)"

  checkTarget s opts = if map toLower s `elem` ["agda"]
                       then opts { optTarget = map toLower s }
                       else error "Illegal target (try `-h' for help)"

-------------------------------------------------------------------------
