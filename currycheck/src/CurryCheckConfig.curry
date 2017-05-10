module CurryCheckConfig where
import Distribution(installDir)
import FilePath(combine)
packageVersion :: String
packageVersion = "2.0.0"
packagePath :: String
packagePath = combine installDir (combine "currytools" "currycheck")
