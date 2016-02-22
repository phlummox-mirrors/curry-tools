-- Examples for problems to be detected by CurryCheck:

import SetFunctions

test1 x | 3 =:<= x = True  -- not allowed!

test2 = set2 (++) [] [42]  -- ok

test3 = set0 ([]++[42])    -- illegal!

test4 = set0 failed        -- ok

test5 = set1 (\x->x) (1?2) -- unintended!

test6 x = set1 f x         -- not allowed since f is not a top-level function
 where f y = y
