{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

-- Bubble sort formulation with default rule

import SetFunctions

sort :: [Int] -> [Int]
sort (xs++[x,y]++ys) | x>y = sort (xs++[y,x]++ys)
sort'default xs = xs

mainnd = sort [7,1,6,3,5,4,2]

-- Compute only first value:
main = selectValue (set0 mainnd)
