module CASS.PackageConfig where

import Distribution(installDir)
import FilePath((</>))

--- Package version as a string.
packageVersion :: String
packageVersion = "0.0.1"

--- Package location.
packagePath :: String
packagePath =
  installDir </> "currytools" </> "optimize" </> ".cpm" </> "packages" </> "cass"

--- Location of the executable installed by this package.
packageExecutable :: String
packageExecutable = ""
