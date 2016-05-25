------------------------------------------------------------------------
--- This module contains some operations to access and check theorems
--- attached to Curry program.
---
--- Current assumptions:
--- * A theorem is represented in a source file by the prefix
---   `theorem`, e.g.:
---
---       theorem'sortlength xs = length xs -=- length (sort xs)
---
--- * A theorem is considered as proven and, thus, not be checked
---   by CurryCheck or the contract wrapper (see `currypp`), if there exists
---   a file named with prefix "Proof" and the name of the theorem, e.g.,
---   `Proof-sortlength.agda`. The name is not case sensitive, the file name
---   extension is arbitrary and the special characters in the name are
---   ignored. Hence, a proof for `sortlength` could be also stored in
---   a file named `PROOF_sort-length.smt`.
---
---  * A proof that some operation `f` is deterministic has the name
---    `fIsDeterministic` so that a proof for `last` can be stored in
---    `proof-last-is-deterministic.agda` (and also in some other files).
---
--- @author Michael Hanus
--- @version May 2016
------------------------------------------------------------------------

module TheoremUsage
  ( isTheoremFunc, isTheoremName, fromTheoremName, determinismTheoremFor
  , getProofFiles, existsProofFor, isProofFileName, isProofFileNameFor
  )  where

import AbstractCurry.Types
import AbstractCurry.Select
import Char
import Directory
import FilePath (dropExtension)
import List

------------------------------------------------------------------------
-- Operations for proof names:

--- Is this function a theorem?
isTheoremFunc :: CFuncDecl -> Bool
isTheoremFunc = isTheoremName . snd . funcName

--- Is this the name of a theorem?
isTheoremName :: String -> Bool
isTheoremName f = "theorem'" `isPrefixOf` f

--- Drop the default rule prefix "theorem'" from the name:
fromTheoremName :: String -> String
fromTheoremName f = drop (if take 8 f == "theorem'" then 8 else 0) f

--- The name of a proof of a determinism annotation for the operation
--- given as the argument.
determinismTheoremFor :: String -> String
determinismTheoremFor funcname = funcname ++ "isdeterministic"

------------------------------------------------------------------------
-- Operations for proof files:

--- Get the names of all files in the directory (first argument) containing
--- proofs of theorems.
getProofFiles :: String -> IO [String]
getProofFiles dir = do
  files <- getDirectoryContents dir
  return (filter isProofFileName files)

--- Does the list of file names (second argument) contain a proof of the
--- theorem given as the first argument?
existsProofFor :: String -> [String] -> Bool
existsProofFor thmname filenames =
  any (isProofFileNameFor thmname) filenames

--- Is this a file name with a proof, i.e., start it with `proof`?
isProofFileName :: String -> Bool
isProofFileName fn = "proof" `isPrefixOf` (map toLower fn)

--- Is this the file name of a proof of theorem `thm`?
isProofFileNameFor :: String -> String -> Bool
isProofFileNameFor tn thm =
  let lthm = map toLower thm
   in if "proof" `isPrefixOf` lthm
      then deleteNonAlphanNum (drop 5 lthm) == deleteNonAlphanNum tn
      else False

--- Delete non alphanumeric characters
deleteNonAlphanNum :: String -> String
deleteNonAlphanNum s = filter isAlphaNum (dropExtension s)

------------------------------------------------------------------------
