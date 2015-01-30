{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=-o --optF=-v #-}

------------------------------------------------------------------------------
--- This program contains examples for integrated code to support
--- easy writing of XML expressions.
------------------------------------------------------------------------------

import XML

test1 :: IO ()
test1 = putStrLn $ showXmlDoc $ head ``xml
 <contact>
  <entry>
   <phone>+49-431-8807271
   <name>Hanus
   <first>Michael
   <email>mh@informatik.uni-kiel.de
   <email>hanus@email.uni-kiel.de
   
  <entry>
   <name>Smith
   <first>Bill
   <phone>+1-987-742-9388
 ''
