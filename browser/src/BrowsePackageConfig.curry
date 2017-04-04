module BrowsePackageConfig where
import Distribution(installDir)
import FilePath(combine)
packageVersion :: String
packageVersion = "0.5.0"
packagePath :: String
packagePath = combine installDir (combine "currytools" "browser")
