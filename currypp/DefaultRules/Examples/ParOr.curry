{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}
{-# OPTIONS_CYMAKE -Wnone #-}

-- Parallel or with default rules:
por True  _     = True
por _     True  = True
por'default _ _ = False

main = [por x y | x <- [True,False], y <- [True,False]]
