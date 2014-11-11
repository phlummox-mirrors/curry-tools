------------------------------------------------------------------------------
--- A printf-like format expression parser
--- ======================================
--- A format expression is in the form
---
---     "(Specifier|NonSpecifier)*"(,Var)*
---
--- A specifier is in the form
---
---     %[flags] [width] [.precision] type
---
--- Allowed Flags: '-' '+' '0' ' ' '#'
---
--- Width and Precision are either integers or '*'.
---
--- Types: 'c' 'd' 'i' 'o' 'x' 'X' 'e' 'E' 'f' 's'
---
--- For explanation on semantics see
--- <http://pubs.opengroup.org/onlinepubs/009695399/functions/fprintf.html>
---
--- __For further informations see the Format library.
--- Not all parsable expressions are usable.__
---
--- @author Jasper Sikorra
--- @version January 2014
------------------------------------------------------------------------------

{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module FormatParser(parse) where

import Parser
import Char
import ReadNumeric
import AllSolutions

import ParseTypes

type Expression = ([Either String Specifier],[Variable])
data Specifier  = Spec (Maybe Flags) (Maybe Width) (Maybe Precision) Type
data SpecifierWVar =
  SpecV
    (Maybe Flags)
    (Maybe WidthV)
    (Maybe PrecisionV)
    Type
    Variable
type Flags = String
type Width = Either Int Char
type WidthV = Int
type Precision = Either Int Char
type PrecisionV = Int
type Type = Char
type Variable = String

--- Possible flags
flags      = ['-','+','0',' ','#']
--- Possible types
types      = ['c','d','i','o','x','X','e','E','f','s']
--- Escapable characters
escapable  = ['a','b','f','n','r','t','v','\"','\'','?','\\','0','x']
--- Possible starting letters for variables
isVarStartLetter c = c /= ','
--- Possible inner letters for variables
isVarInnerLetter c = c /= ','

--- Map each type on a function in the Format library
maptypes :: Char -> String
maptypes c = case c of
  'c' -> "showChar"
  'd' -> "showInt"
  'i' -> "showInt"
  'o' -> "showInt"
  'x' -> "showInt"
  'X' -> "showInt"
  'e' -> "showFloat"
  'E' -> "showFloat"
  'f' -> "showFloat"
  'g' -> "showFloat"
  'G' -> "showFloat"
  's' -> "showString"

--- The function parses and converts a String that is in the format of a C-like
--- printf expression into a Curry Expression that makes use of the Format
--- library.
--- @param showfun - The operation to be applied to the formatted string result
--- @param pos - The position of the expression in the original file
--- @param exp - The expression which should be converted
--- @return A String in Curry Syntax matching exp using the Format
---         library
parse :: String -> LangParser
parse showfun p s = do
  x <- addVarsToSpecs p $ readExpression p s
  return (bindPM (liftPM genString x)
                 (\y -> cleanPM ((++) (showfun++" (") y)))

--- Generate the target code
genString :: [Either String SpecifierWVar] -> String
genString []     = []
genString [x]    = case x of
                    (Left stri) -> "\"" ++ stri ++ "\")"
                    (Right sp) -> specToString sp ++ ")"
genString (x1:x2:xs) = case x1 of
  (Left stri) -> "\"" ++ stri ++ "\" ++ " ++ genString (x2:xs)
  (Right sp) -> specToString sp ++ " ++ " ++ genString (x2:xs)

specToString :: SpecifierWVar -> String
specToString (SpecV f w p t v) =
  "("    ++ maptypes t
  ++ " " ++ show t
  ++ " " ++ show f
  ++ " " ++ show w
  ++ " " ++ show p
  ++ " " ++ v
  ++ ")"

--- Assign variables to the specifiers
addVarsToSpecs :: Pos -> IO (PM Expression)
                  -> IO (PM [Either String SpecifierWVar])
addVarsToSpecs p ioprexp =
  do prexp <- ioprexp
     let prfs = fstPM prexp
         prsn = sndPM prexp
      in do return $ bindPM prfs $ \fs ->
                     bindPM prsn $ \sn ->
                      addVarsToSpecifiers p fs sn

addVarsToSpecifiers :: Pos -> [Either String Specifier] -> [Variable]
                       -> PM [Either String SpecifierWVar]
addVarsToSpecifiers _  [] []              = cleanPM []
addVarsToSpecifiers po [] (_:_)           =
  throwPM po "Too many variables in format expression"
addVarsToSpecifiers po ((Right _):_) []   =
  throwPM po "Too few variables in format expression"
addVarsToSpecifiers po ((Left x):xs) []   =
  liftPM ((:) (Left x)) (addVarsToSpecifiers po xs [])
addVarsToSpecifiers po (q:qs) varis@(v:vs) = case q of
  (Left stri)             -> liftPM ((:) (Left stri))
                                   (addVarsToSpecifiers po qs varis)
  (Right (Spec f w p t)) -> if (isStar w)
      then
        let iv = Just (Left (fst $ maybe failed id $ readNat v))
        in addVarsToSpecifiers po (Right (Spec f iv p t):qs) vs
      else if (isStar p)
        then
          let iv = Just (Left (fst $ maybe failed id $ readNat v))
          in  addVarsToSpecifiers po (Right (Spec f w iv t):qs) vs
        else liftPM ((:) (Right (SpecV f (eE w) (eE p) t v)))
                    (addVarsToSpecifiers po qs vs)
    where
      isStar :: Maybe (Either Int Char) -> Bool
      isStar = maybe
        False
        (\e -> case e of
          (Right '*') -> True
          _           -> False)
      eE :: Maybe (Either Int Char) -> Maybe Int
      eE = maybe Nothing (\x -> Just (either id failed x))

--- Parse a format string expression
readExpression :: Pos -> String -> IO (PM Expression)
readExpression p st =
  do x <- getOneSolution (\a -> expression a st =:= "")
     return $ maybe (throwPM p "Parse error in format expression.") cleanPM x

-- The whole expression
expression   =  quoted q <*> vars v                                             >>> (q,v)                   where q,v free
-- The quote part of the expression
quoted       = terminal '\"' <*> strsAndSpecs s <*> terminal '\"'               >>> s                        where s free
strsAndSpecs = empty                                                            >>> []
          <||> str st                                                           >>> [Left st]
          <||> spec sp <*> strsAndSpecs stsps                                   >>> (Right sp:stsps)
          <||> str st <*> spec sp <*> strsAndSpecs stsps                        >>> (Left st:Right sp:stsps) where st,sp,stsps free
-- -- A normal string
str          = noescp c <*> eorstr st                                           >>> (c:st)
          <||> terminal '\\' <*> escp e <*> eorstr st                           >>> ('\\':e:st)              where c,e,st free
eorstr       = empty                                                            >>> ""
          <||> str s                                                            >>> s                        where s free
noescp       = satisfy (\d -> d /= '\\' && d /= '%' && d /= '\"') c             >>> c                        where c free
escp         = satisfy (\c -> elem c escapable) e                               >>> e                        where e free
-- -- A format specification
spec         = terminal '%' <*> flgs f <*> wid w <*> prc p <*> typ t            >>> (Spec f w p t)
          <||> terminal '%' <*> terminal '%'
            >>> (Spec Nothing Nothing Nothing '%')                                                           where f,w,p,t free
-- -- -- flags
flgs         = empty                                                            >>> Nothing
   <||> flag f <*> someflags fl                                                 >>> (Just (f:fl))            where f,fl free
someflags    = empty                                                            >>> ""
       <||> flag f <*> someflags fl                                             >>> (f:fl)                   where f,fl free
flag         = satisfy (\x -> elem x flags) c                                   >>> c                        where c free
-- -- -- width
wid          = empty                                                            >>> Nothing
          <||> posInt s                                                         >>> (Just (Left (extractNat s)))
          <||> terminal '*'                                                     >>> (Just (Right '*'))       where s free
posInt       = nonzerodigit d <*> digits ds                                     >>> (d:ds)                   where d,ds free
nonzerodigit = satisfy (\c -> isDigit c && c /= '0') d                          >>> d                        where d free
digits       = empty                                                            >>> ""
          <||> digit d <*> digits ds                                            >>> (d:ds)                   where d,ds free
digit        = satisfy (\c -> isDigit c) ch                                     >>> ch                       where ch free
-- -- -- precision
prc          = empty                                                            >>> Nothing
          <||> terminal '.' <*> posZeroInt p                                    >>> (Just (Left (extractNat p)))
          <||> terminal '.'                                                     >>> (Just (Left 0))
          <||> terminal '*' <*> terminal '*'                                    >>> (Just (Right '*'))       where p free
posZeroInt   = digit d <*> digits ds                                            >>> (d:ds)                   where d,ds free
-- -- -- type
typ          = satisfy (\c -> elem c types) t                                   >>> t                        where t free
-- The variables of the expression
vars         = empty                                                            >>> []
          <||> terminal ',' <*> var v <*> vars vs                               >>> (v:vs)                   where v,vs free
var          = posInt i                                                         >>> i
          <||> varStart s <*> varInners i                                       >>> (s:i)                    where s,i free
varStart     = satisfy (\c -> isVarStartLetter c) s                             >>> s                        where s free
varInners    = empty                                                            >>> ""
          <||> varInner v <*> varInners i                                       >>> (v:i)                    where v,i free
varInner     = satisfy (\c -> isVarInnerLetter c) i                             >>> i                        where i free

extractNat :: String -> Int
extractNat s = maybe failed (fst . id) (readNat s)
