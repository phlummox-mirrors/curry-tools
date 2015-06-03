{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=--seqrules #-}
{-# OPTIONS_CYMAKE -Wnone #-}

-- Examples for multiple rules with guards:

-- Non-deterministic guard: the non-determinism is encapsulated
zero x | x<0 ? x>0 = "Not zero"
zero x = "Zero"

main1 = map zero [-1, 0, 1]


f True _    z | z <= 1 = 1
f _    True z | z > -1 = 2

main2 = [f True  True  (-1)
        ,f True  False (-1)
        ,f True  True  0
        ,f True  False 0
        ,f True  True  2
        ,f False True  0
        ,f False True  2
	]
