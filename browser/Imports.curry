-----------------------------------------------------------------
-- A library to get the import structures of a program.

module Imports(getImportedInterfaces,moduleImports,readFlatCurryFileInLoadPath,
               InterfaceOrFlatProg(..),ifOrProg,progOfIFFP)
  where

import FlatCurry
import FlatCurryGoodies
import FlatCurryRead
import FileGoodies
import Distribution(getLoadPathForModule)
import Directory
import Maybe

--- Get all interfaces (i.e., main and all indirectly imported modules)
--- of a program:
getImportedInterfaces :: String -> IO [(String,InterfaceOrFlatProg)]
getImportedInterfaces mod = do
  imps <- readFlatCurryIntWithImports mod
  return (map (\prog -> (progName prog, IF prog)) imps)

-- Extract a module and its imports:
moduleImports (Prog mod imps _ _ _) = (mod,imps)

-----------------------------------------------------------------------------
-- Unione type to distinguish between interface and FlatCurry program:
data InterfaceOrFlatProg = IF Prog | FP Prog

ifOrProg :: (Prog->a) -> (Prog->a) -> InterfaceOrFlatProg -> a
ifOrProg iffun _ (IF prog) = iffun prog
ifOrProg _ fpfun (FP prog) = fpfun prog

progOfIFFP (IF prog) = prog
progOfIFFP (FP prog) = prog

--------------------------------------------------------------------------
-- Read an existing(!) FlatCurry file w.r.t. current load path:
readFlatCurryFileInLoadPath prt mod loadpath = do
  mbfcyfile <- lookupFileInPath (flatCurryFileName mod) [""] loadpath
  maybe (error $ "FlatCurry file of module "++mod++" not found!")
        (readFlatCurryFileAndReport prt mod)
        mbfcyfile

readFlatCurryFileAndReport prt mod filename = do
  size <- fileSize filename
  prt $ "Reading FlatCurry file of module '"++mod++"' ("++show size++" bytes)..."
  prog <- readFlatCurryFile filename
  seq (prog==prog) (return prog)

--------------------------------------------------------------------------
