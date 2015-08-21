{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}
{-# OPTIONS_CYMAKE -Wnone #-}

-- Reverse a list if it has exactly two elements:
rev2 :: [a] -> [a]
rev2 [x,y] = [y,x]
rev2'default xs = xs

main :: [[Int]]
main = map rev2 (map (\n->[1..n]) [0..4])

