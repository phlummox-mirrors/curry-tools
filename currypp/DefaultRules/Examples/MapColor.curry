{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}
{-# OPTIONS_CYMAKE -Wnone #-}

-- The implementation of coloring a map using a default rule
-- (by Sergio Antoy).

data State = WA | OR | ID | BC

states = [WA,OR,ID,BC]

adjacent = [(WA,OR),(WA,ID),(WA,BC),(OR,ID),(ID,BC)]

data Color = Red | Green | Blue

color x = (x, Red ? Green ? Blue)

solve (_++[(s1,c)]++_++[(s2,c)]++_) (_++[(s1,s2)]++_) = failed
default_solve x _ = x

main = solve (map color states) adjacent
