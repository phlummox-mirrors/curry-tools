{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=--seqrules #-}
-- Bubble sort formulation with default rules

import SetFunctions

sort (xs++[x,y]++ys) | x>y = sort (xs++[y,x]++ys)
sort xs = xs

mainnd = sort [7,1,6,3,5,4,2]

-- Compute only first value:
main = selectValue (set0 mainnd)
