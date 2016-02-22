-- A specification of sorting a list and an implementation based
-- on the quicksort algorithm.
-- CurryCheck generates and checks properties which states
-- that the implementation is satisfies the specification
-- and the post-condition.

perm []     = []
perm (xs++[x]++ys) = x : perm (xs++ys)

sorted []       = True
sorted [_]      = True
sorted (x:y:ys) | x<=y && sorted (y:ys) = True

-- Postcondition: input and output lists should have the same length
sort'post xs ys = length xs == length ys

-- Specification of sort:
-- A list is a sorted result of an input if it is a permutation and sorted.
sort'spec :: [Int] -> [Int]
sort'spec x | y == perm x && sorted y = y  where y free

-- An implementation of sort with quicksort:
sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = sort (filter (<x) xs) ++ [x] ++ sort (filter (>=x) xs)
