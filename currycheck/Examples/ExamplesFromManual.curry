import Test.EasyCheck

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

-- Unit tests:
revNull = rev []      -=- []
rev123  = rev [1,2,3] -=- [3,2,1]

-- Property: reversing two times is the identity:
revRevIsId xs = rev (rev xs) -=- xs


-- Non-deterministic list insertion:
insert :: a -> [a] -> [a]
insert x xs     = x : xs
insert x (y:ys) = y : insert x ys

insertAsFirstOrLast :: Int -> [Int] -> Prop
insertAsFirstOrLast x xs = insert x xs ~> (x:xs ? xs++[x])

-- A permutation of a list:
perm :: [a] -> [a]
perm []     = []
perm (x:xs) = insert x (perm xs)

permLength :: [Int] -> Prop
permLength xs = length (perm xs) <~> length xs

sorted :: [Int] -> Bool
sorted []       = True
sorted [_]      = True
sorted (x:y:zs) = x<=y && sorted (y:zs)

permIsEventuallySorted :: [Int] -> Prop
permIsEventuallySorted xs = eventually $ sorted (perm xs)

-- Permutation sort:
psort :: [Int] -> [Int]
psort xs | sorted ys = ys
 where ys = perm xs

psortIsAlwaysSorted xs = always $ sorted (psort xs)

psortKeepsLength xs = length (psort xs) <~> length xs

-- Quicksort:
qsort :: [Int] -> [Int] 
qsort []     = []
qsort (x:l)  = qsort (filter (<x) l) ++ x : qsort (filter (>=x) l)

qsortIsSorting xs = qsort xs <~> psort xs
