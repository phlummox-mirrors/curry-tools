{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

-- Operation to delete suffixes of the fornm ".0"
fix_int (s++".0") = s
fix_int'default s = s

main = map fix_int ["1.3","1.0","42"]
