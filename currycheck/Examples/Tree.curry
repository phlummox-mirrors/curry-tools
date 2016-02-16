-- Some examples for the use of CurryCheck with user-defined data

import SearchTreeGenerators
import Test.EasyCheck

-- A general tree type:
data Tree a = Leaf a | Node [Tree a]

leaves (Leaf x) = [x]
leaves (Node ts) = concatMap leaves ts

mirror (Leaf x) = Leaf x
mirror (Node ts) = Node (reverse (map mirror ts))

-- To use CurryCheck with PAKCS, we have to define our own
-- generator for Nat values (this is not necessary for KiCS2):
genTree gena =  genCons1 Leaf gena
            ||| genCons1 Node (genList (genTree gena))

-- Property: double mirroring is the identity
mirror_mirror t = mirror (mirror t) -=- t

-- Property: the leaves of a mirrored are in reverse order
leaves_mirror t = leaves t -=- reverse (leaves (mirror t))
