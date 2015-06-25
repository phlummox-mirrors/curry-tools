{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules --optF=-o #-}
{-# OPTIONS_CYMAKE -Wnone #-}

-- This example shows that optimal evaluation is still possible
-- with default rules.

f 0 1 = 0
f _ 2 = 1
default_f _ x = x
-- does not yet work with current transformation scheme, better:
-- rename original rules and introduce single rule for orig. function:
-- f x y = f' x y ? f_default x y

loop = loop

main = [f loop 2, f loop 3]
