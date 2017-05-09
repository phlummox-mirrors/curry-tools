module CASS.PackageConfig where

import Distribution(installDir)
import FilePath((</>))

--- Package version as a string.
packageVersion :: String
packageVersion = "2.0.0"

--- Package location.
packagePath :: String
packagePath =
  installDir </> "currytools" </> "cpm" </> "vendor" </> "cass"

--- Location of the executable installed by this package.
packageExecutable :: String
packageExecutable = ""
