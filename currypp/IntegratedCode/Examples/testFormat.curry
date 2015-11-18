{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
--- This program contains some tests for integrated code to format strings.
---
--- The format specification C specification for `printf` formatting.
--- This specification may be found at
--- <http://pubs.opengroup.org/onlinepubs/009695399/functions/fprintf.html>
------------------------------------------------------------------------------

import Assertion
import Format -- required in the pre-processed program

-- Format a string and an integer:
ex1 :: String -> Int -> String
ex1 name age = ``format "Hello %s. Age %i!",name,age''

test1 = assertEqual "ex1" (ex1 "World" 42) "Hello World. Age 42!"

-- Various integer formats:
test2 = assertEqual "intsigned" ``format "%+i",42'' "+42"

test3 = assertEqual "int5" ``format "%.5i",42'' "00042"

test4 = assertEqual "intsigned5" ``format "%+.5d",42'' "+00042"

test5 = assertEqual "intfixedsigned5" ``format "%+10.5i",42'' "    +00042"

-- Integer and character formatting:
test6 = assertEqual "intfixedsignedchar"
                    (let c='%' in ``format "%+5d%c",42,c'')
                    "  +42%"

-- Format a string with a given width and maximal length:
test7 = assertEqual "stringlength"
          (let s = "Hello!" in ``format "This is a string: %08.4s",s'')
          "This is a string:     Hell"

-- Format with passing expressions:
ex8 :: Int -> Int -> String
ex8 n1 n2 = ``format "The sum of %+.5d and %+5i is %+6i.\n",n1,n2,n1+n2''

test9 = assertEqual "intexp" (ex8 42 2143)
                    "The sum of +00042 and +2143 is  +2185.\n"

-- Format a float with a given width and precision:
test10 = assertEqual "floatfixprec3"
                     (let f = 3.14159 in ``format "%+8.3f",f'')
                     "  +3.142"

-- Format a float with an exponent:
test11 = assertEqual "floatexp"
                     (let f = 314.159 in ``format "% .4E",f'')
                     " 3.1416E+02"
