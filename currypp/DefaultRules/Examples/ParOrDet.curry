{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

-- Parallel or declared as a deterministic function:
por :: Bool -> Bool -> DET Bool
por True  _     = True
por _     True  = True
por False False = False

main = por True True
