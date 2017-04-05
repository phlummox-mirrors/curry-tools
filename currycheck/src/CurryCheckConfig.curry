module CurryCheckConfig where
import Distribution(installDir)
import FilePath(combine)
packageVersion :: String
packageVersion = "1.0.1"
packagePath :: String
packagePath = combine installDir (combine "currytools" "currycheck")
