-----------------------------------------------------------------------
--- This module defines a datatype to represent the analysis information.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version March 2013
-----------------------------------------------------------------------

module GenericProgInfo where

import Configuration(debugMessageLevel)
import FiniteMap
import FlatCurry
import XML

--- Type to represent analysis information.
--- The first component are public declarations, the second the private ones.
data ProgInfo a = ProgInfo (FM QName a) (FM QName a)

emptyProgInfo:: ProgInfo a
emptyProgInfo = ProgInfo (emptyFM (<)) (emptyFM (<))

--- Gets the information about an entity.
lookupProgInfo:: QName -> ProgInfo a -> Maybe a
lookupProgInfo key (ProgInfo map1 map2) =
 case lookupFM map1 key of
  Just x -> Just x
  Nothing ->  lookupFM map2 key

--- Combines two analysis informations.
combineProgInfo :: ProgInfo a -> ProgInfo a -> ProgInfo a
combineProgInfo (ProgInfo x1 x2) (ProgInfo y1 y2) =
  ProgInfo (plusFM x1 y1) (plusFM x2 y2)

--- Converts a public and a private analysis list into a program info.
lists2ProgInfo :: ([(QName,a)],[(QName,a)]) -> ProgInfo a
lists2ProgInfo (xs,ys) = ProgInfo (listToFM (<) xs) (listToFM (<) ys)

--- Returns the infos of public operations as a list.
publicListFromProgInfo:: ProgInfo a -> [(QName,a)]
publicListFromProgInfo (ProgInfo fm1 _) = fmToList fm1

progInfo2Lists :: ProgInfo a -> ([(QName,a)],[(QName,a)])
progInfo2Lists (ProgInfo map1 map2)= (fmToList map1,fmToList map2)

--- Transforms analysis information into XML format.
progInfo2XML :: ProgInfo String -> ([XmlExp],[XmlExp])
progInfo2XML (ProgInfo map1 map2) = 
  (foldFM entry2xml [] map1, foldFM entry2xml [] map2)
 where
  entry2xml (mname,name) value xmlList =
    (xml "operation" [xml "module" [xtxt mname],
                      xml "name"   [xtxt name],
                      xml "result" [xtxt value]]) : xmlList 

mapProgInfo:: (a->b) -> ProgInfo a -> ProgInfo b
mapProgInfo func (ProgInfo map1 map2) = 
  ProgInfo (mapFM (\_ b->func b) map1) (mapFM (\_ b->func b) map2)

publicProgInfo :: ProgInfo a -> ProgInfo a
publicProgInfo (ProgInfo pub _) = ProgInfo pub (emptyFM (<))

--- Writes a ProgInfo into a file.
writeAnalysisFiles :: String -> ProgInfo _ -> IO ()
writeAnalysisFiles basefname (ProgInfo pub priv) = do
  writeFile (basefname++".priv") (showFM priv)
  writeFile (basefname++".pub")  (showFM pub)
  debugMessageLevel 3 $ "Analysis file '"++basefname++"' written."

--- Reads a ProgInfo from the analysis files where the base file name is given.
readAnalysisFiles :: String -> IO (ProgInfo _)
readAnalysisFiles basefname = do
  debugMessageLevel 3 $ "Reading analysis files '"++basefname++"'..."
  pubcont  <- readFile (basefname++".pub")
  privcont <- readFile (basefname++".priv")
  return (ProgInfo (readFM (<) pubcont) (readFM (<) privcont))

--- Reads the public ProgInfo from the public analysis file.
readAnalysisPublicFile :: String -> IO (ProgInfo _)
readAnalysisPublicFile fname = do
  debugMessageLevel 3 $ "Reading public analysis file '"++fname++"'..."
  fcont <- readFile fname
  return (ProgInfo (readFM (<) fcont) (emptyFM (<)))

--- Show a ProgInfo as a string (used for debugging only).
showProgInfo :: ProgInfo _ -> String
showProgInfo (ProgInfo fm1 fm2) =
  "Public: "++showFM fm1++"\nPrivate: "++showFM fm2

-- Equality on ProgInfo
equalProgInfo :: ProgInfo a -> ProgInfo a -> Bool
equalProgInfo (ProgInfo pi1p pi1v) (ProgInfo pi2p pi2v) =
  eqFM pi1p pi2p && eqFM pi1v pi2v
