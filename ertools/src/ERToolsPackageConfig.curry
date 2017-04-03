module ERToolsPackageConfig where
import Distribution(installDir)
import FilePath(combine)
packageVersion :: String
packageVersion = "1.0.0"
packagePath :: String
packagePath = installDir ++ combine "currytools" "ertools"
