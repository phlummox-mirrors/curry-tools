{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

-- Example: predicate to check for float strings

import Char(isDigit)

-- Is the argument a non-negative float string (without exponent)?
-- Our desired notation:
isNNFloat :: String -> Bool
isNNFloat (f1 ++ "." ++ f2) | (all isDigit f1 && all isDigit f2) = True
isNNFloat'default _ = False

main = map isNNFloat ["3.14","314"]
