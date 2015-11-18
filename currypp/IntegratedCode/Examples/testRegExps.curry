{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
--- This program contains some tests for integrated code to support
--- regular expression matching.
--- The syntax of regular expressions is similar to
--- POSIX extended regular expressions
------------------------------------------------------------------------------

import Assertion
import RegExp -- required in the pre-processed program

test1 = assertTrue "abc" ("abc" ``regex abc'')

test2 = assertTrue "aba*c" ("abaaaaaaaaaaaaac" ````regex aba*c'''')

test3 = assertTrue "(a|(bc*))+" ("aabcccaba" ``regex (a|(bc*))+'')

test4 = assertTrue "[:alpha:] 1" ("a" ``regex [:alpha:]'')

test5 = assertTrue "[:alpha:] 2" (not ("4" ``regex [:alpha:]''))

test6 = assertTrue "[:alpha:]* 1" ("Abc" ``regex [:alpha:]*'')

test7 = assertTrue "[:alpha:]* 2" (not ("ab9c" ``regex [:alpha:]*''))

test8 = assertTrue "[a-z]+ 1" ("abc" ``regex [a-z]+'')

test9 = assertTrue "[a-z]+ 2" (not ("Abc" ``regex [a-z]+''))

-- Examples with parameterized regular expressions:

pregexp1 :: [a] -> a -> a -> Bool
pregexp1 s v1 v2 = s ``regex [<v1>-<v2>]*''

test10 = assertTrue "para a-c 1" (pregexp1 "abccba" 'a' 'c')

test11 = assertTrue "para a-c 2" (not (pregexp1 "abcdcba" 'a' 'c'))

pregexp2 :: [a] -> a -> a -> Bool
pregexp2 s v1 v2 = s ``regex (<v1>|<v2>)*''

test12 = assertTrue "para (0|1)* 1" (pregexp2 [0,1,1,0,0] 0 1)

test13 = assertTrue "para (0|1)* 2" (not (pregexp2 [0,1,2,0,0] 0 1))

-- A regular expression containing a complex Curry expression:
test14 = assertTrue "complexexp" ("a" ``regex <((\x -\> x) 'a')>'')


-- Email address matching:
isEmail :: String -> Bool
isEmail s = s ``regex
  [a-zA-Z0-9]([a-zA-Z0-9\._])*
  @
  [a-zA-Z0-9][a-zA-Z0-9\-]*\.
  ([:alnum:][a-zA-Z0-9\-]*\.)*
  [a-zA-Z]{2,4}''

test20 = assertTrue "Email1" (isEmail "pakcs@curry-language.org")

test21 = assertTrue "Email2" (not (isEmail "pa%kcs@curry-language.org"))

