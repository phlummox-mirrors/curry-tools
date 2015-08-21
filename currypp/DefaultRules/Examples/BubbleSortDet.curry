{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

-- Bubble sort formulation with default rule as deterministic function

sort :: [Int] -> DET [Int]
sort (xs++[x,y]++ys) |  x>y = sort (xs++[y,x]++ys)
sort'default xs = xs

main = sort [7,1,6,3,5,4,2]

mainN n = sort [n,n-1 .. 1]
