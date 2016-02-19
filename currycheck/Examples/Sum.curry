-- Add numbers and define a specific generator for non-negative integers

import SearchTreeGenerators
import Test.EasyCheck

sumUp n = if n==0 then 0 else n + sumUp (n-1)

sumUpIsCorrect n = n>=0 ==> sumUp n -=- n * (n+1) `div` 2

genInt = genCons0 0 ||| genCons1 (\n -> 2*(n+1)) genInt
                    ||| genCons1 (\n -> 2*n+1)   genInt
