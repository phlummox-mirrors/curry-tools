{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}
{-# OPTIONS_CYMAKE -Wnone #-}

-- Examples for multiple rules with guards:

-- Non-deterministic guard: the non-determinism is encapsulated
zero x | x<0 ? x>0 = "Not zero"
default_zero x = "Zero"

main1 = map zero [-1, 0, 1]


f True _    z | z <= 1 = 1
f _    True z | z > -1 = 2
default_f _ _ _ = 3

main2 = [f True  True  (-1)
        ,f True  False (-1)
        ,f True  True  0
        ,f True  False 0
        ,f True  True  2
        ,f False True  0
        ,f False True  2
	]


g x | x==0 = 0
    | x==1 = 1
default_g x = 2
