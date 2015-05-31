------------------------------------------------------------------------------
--- Module defining a predicate to select non-deterministic operations,
--- i.e., operations that are not defined by inductively sequential rules.
--- 
--- @author Lasse Folger (with changes by Michael Hanus)
--- @version June 2015
------------------------------------------------------------------------------

module Selection where

import AbstractCurry
{- In diesem Modul ist ein Prädikat definiert, welches überprüft
ob eine Funktion deterministisch ist. Alle weiten Funktionen
sind dabei unterstützdend. 
-}


--Dieses Prädikat testet, ob eine Funktion deterministisch ist.
isnondeterministic :: CFuncDecl -> Bool
isnondeterministic (CFunc _ _ _ _ rulel)
  | length (prefilter rulel) > 1 = checkIArgs ([],(prefilter rulel))
  | otherwise                    = False
isnondeterministic (CmtFunc _ _ _ _ _ rulel)
  | length (prefilter rulel) > 1 = checkIArgs ([],(prefilter rulel))
  | otherwise                    = False

--Diese Funktion such rekursiv nach induktiven Argumenten, wenn keine gefunden
--werden, gibt wird True zurückgegeben, also wenn die Funktion nicht-deterministisch ist.
checkIArgs :: ([[CPattern]],[[CPattern]]) -> Bool
checkIArgs (_,[])         = True
checkIArgs t@(i,ls@(_:_)) = if (all isCons ls) 
                                then continue (conca ls i) 
                                else checkIArgs (cutnext t)   
  where continue (x:xs) | (length (x:(filter (equal x) xs))) > 1 = checkIArgs ([],(stepin (x:(filter (equal x) xs)))) 
                                                                   || continue (filter (diffrent x) xs)
                        | otherwise                              = continue xs
        continue []                                              = False
        stepin l                                                 = map stepin' l
        stepin' ((CPComb _ x):xs)                                = x ++ xs
        stepin' ((CPLit _) :xs)                                  = xs 
        equal ((CPComb a _) : _) ((CPComb b _) : _) | a == b     = True
                                                    | otherwise  = False
        equal ((CPLit a) : _) ((CPLit b) : _)      | a == b      = True
                                                   | otherwise   = False
        diffrent a b                                             = not (equal a b)
        isCons []                                                = False        
        isCons (x:_)                                             = case x of CPComb _ _ -> True
                                                                             CPLit  _   -> True
                                                                             _          -> False

--Zusammenführung von Listen von Listen.
conca :: [[a]] -> [[a]] -> [[a]]        
conca (a:as) (l:ls)     = (a ++ l) : (conca as ls)
conca []       ls@(_:_) = ls
conca as@(_:_) []       = as
conca []       []       = []  

--Das jeweils erste Element der "rechten" Listen wird an die
--linken Listen gehängt. 
cutnext :: ([[CPattern]],[[CPattern]]) -> ([[CPattern]],[[CPattern]]) 
cutnext (ys,xs) = ((conca ys a),b) 
        where (a,b)                           = cutfirst xs     
              cutfirst cps                    = foldr f ([],[]) cps
              f (cp:cps') (fs,rs) | null cps' = ([cp]:fs,rs)
                                  | otherwise = ([cp]:fs,cps':rs)
              f []        (fs,rs)             = (fs,rs) 

  
--Reduzierung der Regeln auf ihre Pattern.
prefilter :: [CRule] -> [[CPattern]]
prefilter patl = map (map removeAsP) (map prefilter' patl)
 where
  prefilter' (CRule pl _) = pl

  removeAsP x = case x of (CPAs _ p) -> p
                          (CPLazy p) -> p
                          _          -> x    





