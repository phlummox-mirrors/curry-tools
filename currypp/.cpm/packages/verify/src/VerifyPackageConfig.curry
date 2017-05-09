module VerifyPackageConfig where

import Distribution(installDir)
import FilePath((</>))

--- Package version as a string.
packageVersion :: String
packageVersion = "0.0.2"

--- Package location.
packagePath :: String
packagePath =
  installDir </> "currytools" </> "currypp" </> ".cpm" </> "packages" </> "verify"

--- Location of the executable installed by this package.
packageExecutable :: String
packageExecutable = ""
