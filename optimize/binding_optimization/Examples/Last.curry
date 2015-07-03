-- The classical last element of a list:

last :: [a] -> a
last xs | _ ++ [x] == xs = x  where x free

main :: Int
main = last [1,2,3]
