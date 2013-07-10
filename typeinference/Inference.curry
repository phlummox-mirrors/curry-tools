------------------------------------------------------------------------------
--- Library to annotate the expressions of a FlatCurry program
--- with type information.
---
--- The type inference works in several steps:
---
---  1. For each known function and constructor, either imported or defined
---     in the module itself, the respective type is inserted into a type
---     environment (type assumption).
---
---  2. Every sub-expression is annotated with a fresh type variable, whereas
---     constructor and function names are annotated with a fresh variant of
---     the type in the type assumption.
---
---  3. Based on FlatCurry's type inference rules, type equations are generated
---     for a function's rule.
---
---  4. The resulting equations are solved using unification and the
---     resulting substituion is applied to the function rule.
---
---  5. The inferred types are then normalized such that for every function
---     rule the type variables start with 0.
---
--- In case of any error, the type inference quits with an error message.
---
--- @author  Jonas Oberschweiber, Björn Peemöller, Michael Hanus
--- @version July 2013
------------------------------------------------------------------------------
module Inference
  ( TypeEnv, getTypeEnv, getTypeEnvFromProgEnv
  , inferProg, inferProgFromProgEnv, inferProgEnv
  , inferFunction, inferFunctionEnv
  ) where

import FiniteMap
import List      (find)

import AFCSubst
import AnnotatedFlatCurry
import AnnotatedFlatCurryGoodies as AFC (annExpr)
import ErrorState
import FlatCurry
import Unification

-- ---------------------------------------------------------------------------
-- public functions
-- ---------------------------------------------------------------------------

--- Infers the type of a whole program.
---
--- @param p - the Prog to infer
--- @return the inferred program or an error
inferProg :: Prog -> IO (Either String (AProg TypeExpr))
inferProg p = getTypeEnv p >>= \te -> return (inferProgEnv te p)

--- Infers the type of a whole program.
---
--- @param p - the Prog to infer
--- @return the inferred program or an error
inferProgFromProgEnv :: [(String, Prog)] -> Prog
                     -> Either String (AProg TypeExpr)
inferProgFromProgEnv env p = case getTypeEnvFromProgEnv env p of
  Left err    -> Left err
  Right tyEnv -> inferProgEnv tyEnv p

--- Infers the type of a whole program.
--- Uses the given type environment instead of generating a new one.
---
--- @param env - the type environment
--- @param p - the Prog to infer
--- @return the inferred program or an error
inferProgEnv :: TypeEnv -> Prog -> Either String (AProg TypeExpr)
inferProgEnv te p = evalES (annProg p >+= inferAProg) (initTIM te)

--- Infers the types of a single function specified by its qualified name.
---
--- @param q - the qualified name of the function
--- @param p - the Prog containing the function
--- @return the inferred function or an error
inferFunction :: QName -> Prog -> IO (Either String (AFuncDecl TypeExpr))
inferFunction f p = getTypeEnv p >>= \te -> return (inferFunctionEnv te f p)

--- Infers the types of a single function specified by its qualified name.
--- Uses the given type environment instead of generating a new one.
---
--- @param env - the type environment
--- @param q - the qualified name of the function
--- @param p - the Prog containing the function
--- @return the inferred function or an error
inferFunctionEnv :: TypeEnv -> QName -> Prog
                 -> Either String (AFuncDecl TypeExpr)
inferFunctionEnv te fun (Prog _ _ _ fd _) = case find (hasName fun) fd of
  Nothing -> Left "No such function"
  Just f  -> evalES (annFunc f >+= inferFunc) (initTIM te)
 where hasName f (Func g _ _ _ _) = f == g

-- ---------------------------------------------------------------------------
-- 1. Type environment
-- ---------------------------------------------------------------------------

--- A type environment.
type TypeEnv = FM QName TypeExpr

--- Looks up a type with a qualified name in a type environment.
---
--- @param env - the type environment
--- @param q - the qualified name to look for
--- @return maybe the type
lookupType :: TypeEnv -> QName -> Maybe TypeExpr
lookupType = lookupFM

--- Extract the type environment from the given Prog.
---
--- @param p - the Prog
--- @return a type environment
getTypeEnv :: Prog -> IO TypeEnv
getTypeEnv p = do
  imps <- extractImported p
  return (extractKnownTypes (p : imps))

--- Reads the interfaces of all modules imported into the given Prog.
---
--- @param p - the Prog whose imports should be read
--- @return the list of interface Progs
extractImported :: Prog -> IO [Prog]
extractImported (Prog _ is _ _ _) = mapIO readFlatCurryInt is

--- Extract the type environment from the given Prog by lookup in a
--- module name -> Prog environment.
---
--- @param env - An environment mapping module names to Progs
--- @param p - the Prog
--- @return a type environment
getTypeEnvFromProgEnv :: [(String, Prog)] -> Prog -> Either String TypeEnv
getTypeEnvFromProgEnv env prog@(Prog _ imps _ _ _) = case extract imps of
  Left err   -> Left err
  Right mods -> Right (extractKnownTypes (prog : mods))
 where
  extract []     = Right []
  extract (i:is) = case lookup i env of
    Nothing -> Left ("getTypeEnvFrom: Could not find module " ++ i)
    Just p  -> case extract is of
      Left err -> Left err
      Right ps -> Right (p : ps)

--- Extracts the type information of all function and datatype
--- declarations from the given list of Progs.
---
--- @param ps - the list of Progs
--- @return a type environment
extractKnownTypes :: [Prog] -> TypeEnv
extractKnownTypes ps = listToFM (<) (concatMap extractProg ps)
 where
  extractProg :: Prog -> [(QName, TypeExpr)]
  extractProg (Prog _ _ td fd _)
    = concatMap extractTypeDecl td ++ map extractFuncDecl fd

  extractFuncDecl :: FuncDecl -> (QName, TypeExpr)
  extractFuncDecl (Func n _ _ ty _) = (n, ty)

  extractTypeDecl :: TypeDecl -> [(QName, TypeExpr)]
  extractTypeDecl (TypeSyn  n _ _ ty) = [(n, ty)]
  extractTypeDecl (Type    n _ vs cs) = map (extractConsDecl ty) cs
    where ty = TCons n (map TVar vs)

  extractConsDecl :: TypeExpr -> ConsDecl -> (QName, TypeExpr)
  extractConsDecl ty (Cons n _ _ tys) = (n, foldr FuncType ty tys)

-- ---------------------------------------------------------------------------
-- Type Inference Monad
-- ---------------------------------------------------------------------------

--- The monad contains an `Int` value for fresh type variable generation
--- and a mapping from variable indices to their associated type
--- variables. It returns a `String` if an error occured.
type TIM a = ES String (TypeEnv, Int, FM Int TypeExpr) a

--- Initial TIM state.
initTIM :: TypeEnv -> (TypeEnv, Int, FM Int TypeExpr)
initTIM te = (te, 0, emptyFM (<))

--- Retrieve the next fresh type variable.
nextTVar :: TIM TypeExpr
nextTVar = gets >+= \ (te, n, var2Ty) ->
           puts (te, n + 1, var2Ty) >+ returnES (TVar n)

--- Intialize the "variable to type variable mapping", i.e., delete all
--- associations.
initVar2TVar :: TIM ()
initVar2TVar = modify $ \ (te, n, _) -> (te, n, emptyFM (<))

--- Insert a new variable/type variable association.
insertVar2TVar :: Int -> TypeExpr -> TIM ()
insertVar2TVar v ty = modify $ \ (te, n, var2Ty) -> (te, n, addToFM var2Ty v ty)

--- Insert a new variable/fresh type variable association.
insertFreshVar :: Int -> TIM ()
insertFreshVar v = nextTVar >+= insertVar2TVar v

--- Look up the type variable associated to a variable.
lookupVar2TVar :: Int -> TIM (Maybe TypeExpr)
lookupVar2TVar v = gets >+= \ (_, _, var2Ty) -> returnES (lookupFM var2Ty v)

--- Looks up a type in a type environment and renames all type variables
--- in the type (replaces them with fresh ones).
---
--- @param env - the type environment
--- @param q - the qualified name of the type to look up
--- @return the found and renamed type or an error
getTypeVariant :: QName -> TIM (QName, TypeExpr)
getTypeVariant f = gets >+= \ (env, _, _) -> case lookupType env f of
  Nothing -> failES $ "Unknown function or constructor: " ++ show f
  Just t  -> freshVariant t >+= \ty -> returnES (f, ty)

--- Renames all TVars inside the given type expression.
---
--- @param ty - the type expression
--- @return The renamed type expression
freshVariant :: TypeExpr -> TIM TypeExpr
freshVariant ty = snd `liftES` rename [] ty
 where
  rename ren (TVar       i) = case lookup i ren of
    Just j  -> returnES (ren, j)
    Nothing -> nextTVar >+= \j -> returnES ((i, j) : ren, j)
  rename ren (FuncType a b) = rename ren  a >+= \ (ren1, a') ->
                              rename ren1 b >+= \ (ren2, b') ->
                              returnES (ren2, FuncType a' b')
  rename ren (TCons  t tys) = mapAccumES rename ren tys >+= \(ren', tys') ->
                              returnES (ren', TCons t tys')

--- Append two lists yielded by monadic computations.
(++=) :: TIM [a] -> TIM [a] -> TIM [a]
(++=) = liftES2 (++)

-- -----------------------------------------------------------------------------
-- 2. Annotation, traversing the AST and inserting fresh type variables
-- -----------------------------------------------------------------------------

--- Converts the Prog to an AProg, inserting TVars into all expressions.
---
--- @param prog - the prog to convert
--- @return an AProg and the next TVar number in an TIM
annProg :: Prog -> TIM (AProg TypeExpr)
annProg (Prog mid is td fd od) =
  (\afd -> AProg mid is td afd od) `liftES` mapES annFunc fd

--- Converts the FuncDecl to an AFuncDecl, inserting TVars into all
--- expressions.
---
--- @return the AFuncDecl and the new next TVar number in an TIM
annFunc ::FuncDecl -> TIM (AFuncDecl TypeExpr)
annFunc (Func qn a v t r)
  = initVar2TVar >+ liftES2 (AFunc qn a v) (freshVariant t) (annRule r)

--- Converts the Rule to an ARule, inserting TVars into all expressions.
---
--- @param n - the first TVar number to use
--- @return the ARule and the new next TVar number in an TIM
annRule :: Rule -> TIM (ARule TypeExpr)
annRule (Rule  vs e) = liftES3 ARule nextTVar (mapES annVar vs) (annExpr e)
annRule (External s) = (\ty -> AExternal ty s) `liftES` nextTVar

--- Converts the Expr to an AExpr, inserting TVars into all sub-expressions.
---
--- @param n - the first TVar number to use
--- @return the AExpr and the new next TVar number in an TIM
annExpr :: Expr -> TIM (AExpr TypeExpr)
annExpr (Var       i) = lookupVar2TVar i >+=
                        maybe (failES err) (\ty -> returnES (AVar ty i))
  where err = "Variable " ++ show i ++ " not initialized with a type!"
annExpr (Lit       l) = nextTVar >+= \ty -> returnES (ALit ty l)
annExpr (Comb t q es) = liftES3 (\ty -> AComb ty t) nextTVar
                                 (getTypeVariant q) (mapES annExpr es)
annExpr (Case t e bs) = liftES3 (\ty -> ACase ty t) nextTVar
                                (annExpr e) (mapES annBranch bs)
annExpr (Or      a b) = liftES3 AOr  nextTVar (annExpr a) (annExpr b)
annExpr (Let    ds e) = liftES3 ALet nextTVar (annBindings ds) (annExpr e)
 where annBindings bs = let (vs, es) = unzip bs in
                        mapES checkVar vs >+= \vs' ->
                        mapES annExpr  es >+= \es' ->
                        returnES (zip vs' es')
       checkVar v     = checkShadowing v >+ insertFreshVar v >+ returnES v
annExpr (Free   vs e) = liftES3 AFree nextTVar (mapES annFree vs) (annExpr e)
  where annFree v     = checkShadowing v >+ annVar v
annExpr (Typed  e ty) = liftES3 ATyped nextTVar (annExpr e) (freshVariant ty)

annVar :: VarIndex -> TIM (VarIndex, TypeExpr)
annVar v = nextTVar >+= \ty -> insertVar2TVar v ty >+ returnES (v, ty)

--- Checks whether a locally introduced variable is already defined in the
--- surrounding scope, which indicates variable shadowing that is not allowed
--- in FlatCurry files. This is our basic assumption in this type inferencer,
--- and must therefore be met. Otherwise, the type inference must be extended.
checkShadowing :: VarIndex -> TIM ()
checkShadowing v = lookupVar2TVar v >+= maybe (returnES ()) (\_ -> failES err)
  where err = "shadowing with variable " ++ show v ++ " occurred!"

--- Converts the BranchExpr to an ABranchExpr, inserting TVars
--- into all expressions
---
--- @param n - the first TVar number to use
--- @return the ABranchExpr and the new next TVar number in an TIM
annBranch :: BranchExpr -> TIM (ABranchExpr TypeExpr)
annBranch (Branch p e) = liftES2 ABranch (annPattern p) (annExpr e)

--- Converts the Pattern into an APattern, inserting a TVar
--- into the pattern
---
--- @param n - the TVar number to use
--- @return the APattern and the new next TVar number in an TIM
annPattern :: Pattern -> TIM (APattern TypeExpr)
annPattern (Pattern c vs) = liftES3 APattern nextTVar (getTypeVariant c)
                                             (mapES annPVar vs)
  where annPVar v = checkShadowing v >+ annVar v
annPattern (LPattern   l) = (\ty -> ALPattern ty l) `liftES` nextTVar

-- ---------------------------------------------------------------------------
-- 3. Type inference
-- ---------------------------------------------------------------------------

--- Infers all types in the given program.
---
--- @param p - the program to infer
--- @param n - the next fresh TVar number
--- @return the inferred program or an error
inferAProg :: AProg TypeExpr -> TIM (AProg TypeExpr)
inferAProg (AProg mid is td fd od)
  = (\fd' -> AProg mid is td fd' od) `liftES` mapES inferFunc fd

--- Infers all types in the given function.
---
--- @param n - the next fresh TVar number
--- @param f - the function
--- @return the inferred function or an error
inferFunc :: AFuncDecl TypeExpr -> TIM (AFuncDecl TypeExpr)
inferFunc func@(AFunc _ _ _ ty r) =
  inferRule   ty r  >+= \ tyEqs ->
  unification tyEqs >+= \ sigma ->
  normFunc $ substFunc sigma func

--- Type equations
type TypeEqs = [(TypeExpr, TypeExpr)]

--- Smart constructor for type equation
(=.=) :: TypeExpr -> TypeExpr -> (TypeExpr, TypeExpr)
ty1 =.= ty2 = (ty1, ty2)

showTypeEqs :: TypeEqs -> String
showTypeEqs = unlines . map showEquation
  where showEquation (l, r) = show l ++ " =.= " ++ show r

--- Infer the type for a rule.
inferRule :: TypeExpr -> ARule TypeExpr -> TIM TypeEqs
inferRule ty (ARule     ty2 vs e)
  =   returnES  ((ty =.= ty2) : matchApp (exprType e) ty (map snd vs))
  ++= inferExpr e
inferRule ty (AExternal ty2    _) = returnES [ty =.= ty2]

--- Matches the given parameter expressions to the given type.
--- Returns a list of equation pairs. The "leftovers" are assigned
--- to the TypeExpr given as the first parameter.
--- May be used on FuncCall, FuncPartCall, ConsCall and ConsPartCall.
---
--- @param t - the type to assign the "leftover" type from the call to
--- @param f - the function type to match to
--- @param ps - the parameter expressions
--- @return a list of equations
matchApp :: TypeExpr -> TypeExpr -> [TypeExpr] -> TypeEqs
matchApp rty a              []     = [rty =.= a]
matchApp rty (FuncType a b) (p:ps) = (p =.= a) : matchApp rty b ps

--- Recursively generate equations for the unifier from an expression.
---
--- @param env - the type environment
--- @param n - the next fresh TVar number
--- @param ex - the expression
--- @return a list of equations or an error inside an TIM carrying
---         the next free TVar number
inferExpr :: AExpr TypeExpr -> TIM TypeEqs
-- No equations to generate.
inferExpr (AVar _  _)       = returnES []
-- The type of the expression is equal to the type of the literal.
inferExpr (ALit ty l)       = returnES [ty =.= literalType l]
-- Recursively generate equations for each argument expression and
-- match the types of the argument expressions to the types expected by
-- the function or constructor.
-- Whatever is left must be the result type of the call.
inferExpr (AComb ty _ (_, fty) es)
  = returnES (matchApp ty fty $ map exprType es) ++= concatMapES inferExpr es
-- Generate equations for the subject and the branches.
inferExpr (ACase ty _ e bs)
  = inferExpr e ++= concatMapES (inferBranch ty e) bs
-- Recursively generate equations for each of the argument expressions.
-- The type of the expression must be equal to the types
-- of both argument expressions.
-- The types of the argument expressions must be equal to each other.
inferExpr (AOr ty a b) = returnES [exprType a =.= ty, exprType b =.= ty]
                          ++= inferExpr a ++= inferExpr b
-- Generate equations for all bound expressions and for the inner expression.
-- Equate the type of each occurence of a bound variable
-- in the inner expression or some bound expression to the type of
-- the expression which the variable is bound to.
-- The type of the expression itself must be equal to the type of the inner
-- expression.
inferExpr (ALet ty bs e)
  = let bvartypes = map (\(v, b) -> (v, exprType b)) bs
    in concatMapES (inferVars bvartypes) (e : map snd bs) ++=
       concatMapES inferExpr (map snd bs) ++=
       inferExpr e ++= returnES [ty =.= exprType e]
-- Generate equations for the inner expression.
-- The type of the expression itself must be equal
-- to the type of the inner expression.
inferExpr (AFree ty _ e)
  = inferExpr e ++= returnES [ty =.= exprType e]
-- Recursively generate equations for each of the argument expression.
-- The type of the expression must be equal to the type of the
-- argument expression. In addition, it must be also equal to the given type.
inferExpr (ATyped ty e ty')
  = inferExpr e ++= returnES [exprType e =.= ty, exprType e =.= ty']

--- Generate equation pairs for a branch.
---
---  This consists of:
---    - generating equations for the branch's expression
---    - equating the type of the branch's expression to the type of the
---      overall case expression
---    - for constructor patterns:
---        - equating all occurences of variables bound by the
---          deconstruction process inside the branch's expression to the
---          corresponding types of the arguments expected by the constructor
---        - equating the type of the pattern to whatever is left after
---          matching the constructor's argument types
---          to the deconstructionvariables
---          (should always be the type of the constructor's datatype)
---    - for literal patterns: equating the type of the pattern to
---      the type of the literal
---    - equating the type of the case's subject to the type of the pattern
---
--- @param ty   - the parent case expression's type
--- @param subj - the case's subject expression
--- @param b    - the branch
--- @return a list of equations
inferBranch :: TypeExpr -> AExpr TypeExpr -> ABranchExpr TypeExpr -> TIM TypeEqs
inferBranch ty e (ABranch p be) = returnES [ty =.= exprType be]
  ++= inferPattern (exprType e) p ++= inferExpr be

inferPattern :: TypeExpr -> APattern TypeExpr -> TIM TypeEqs
inferPattern ty (APattern  pty (_, fty) vs)
  = returnES $ (ty =.= pty) : matchApp pty fty (map snd vs)
inferPattern ty (ALPattern pty           l)
    = returnES [ty =.= pty, pty =.= literalType l]

--- Recursively search the expression and generate an equation for every AVar
--- that we're given a type for in the first parameter.
---
--- @param env - the type environment
--- @param vs - a list of bindings from variables to types
--- @param ex - the expression to search
--- @return a list of type equations or an error inside an TIM carrying
---         the next fresh TVar number
inferVars :: [(VarIndex, TypeExpr)] -> AExpr TypeExpr
            -> TIM TypeEqs
inferVars vs (AComb _ _ _ ps) = concatMapES (inferVars vs) ps
inferVars vs (ACase _ _ s bs) = concatMapES genBranchVarPairs bs
                                ++= inferVars vs s
  where genBranchVarPairs (ABranch _ e) = inferVars vs e
inferVars vs (AVar      ty v) = case lookup v vs of
  Just ty' -> returnES (if ty == ty' then [] else [ty =.= ty'])
  Nothing  -> returnES []
inferVars _  (ALit       _ _) = returnES []
inferVars vs (AOr      _ a b) = inferVars vs a ++= inferVars vs b
inferVars vs (ALet    _ bs e) = concatMapES (inferVars vs) (map snd bs)
                                ++= inferVars vs e
inferVars vs (AFree    _ _ e) = inferVars vs e
inferVars vs (ATyped   _ e _) = inferVars vs e

--- Extract the type of a Literal.
literalType :: Literal -> TypeExpr
literalType (Intc   _) = TCons ("Prelude", "Int"  ) []
literalType (Floatc _) = TCons ("Prelude", "Float") []
literalType (Charc  _) = TCons ("Prelude", "Char" ) []

--- Extract the TypeExpr from an annotated Expr
exprType :: AExpr TypeExpr -> TypeExpr
exprType = AFC.annExpr

-- ---------------------------------------------------------------------------
-- 4. Functions for interfacing with the Unification module
-- ---------------------------------------------------------------------------

--- Call the unification on the given equations.
unification :: TypeEqs -> TIM AFCSubst
unification eqs = case unify (fromTypeEqs eqs) of
  Left  err -> failES $ showUnificationError err
  Right sub -> returnES (mapFM (\_ -> toTypeExpr) sub)

--- Converts a list of type expression equations into term equations.
fromTypeEqs :: TypeEqs -> TermEqs
fromTypeEqs = map (\(a,b) -> (fromTypeExpr a, fromTypeExpr b))

--- Converts a list of term equations into type expression equations.
toTypeEqs :: TermEqs -> TypeEqs
toTypeEqs = map (\(a,b) -> (toTypeExpr a =.= toTypeExpr b))

--- Converts the given type expression into a term for unification.
fromTypeExpr :: TypeExpr -> Term
fromTypeExpr (TVar       n) = TermVar n
fromTypeExpr (TCons   t vs) = TermCons (fromQName t) (map fromTypeExpr vs)
fromTypeExpr (FuncType a b) = TermCons "->" [fromTypeExpr a, fromTypeExpr b]

--- Converts the given unification term into a type expression
toTypeExpr :: Term -> TypeExpr
toTypeExpr (TermVar     n) = TVar n
toTypeExpr (TermCons t vs)
    | t == "->" = FuncType (toTypeExpr (vs !! 0)) (toTypeExpr (vs !! 1))
    | otherwise = TCons (toQName t) (map toTypeExpr vs)

--- Converts a qualified name to a string.
fromQName :: QName -> String
fromQName (mod, typ) = mod ++ ";" ++ typ

--- Converts a string to a qualified name.
toQName :: String -> QName
toQName str = (fst split, snd split)
  where split = splitFirst str ';'

--- Splits a list at the first occurence of a given value.
---
--- @param xs - the list to split
--- @param x - the value to split at
--- @return a tuple of the lists before and after the split
splitFirst :: [a] -> a -> ([a], [a])
splitFirst []     _ = ([], [])
splitFirst (a:as) c
  | a == c    = ([], as)
  | otherwise = (a : fst rest, snd rest)
    where rest = splitFirst as c

--- Formats a unification error with the given message.
showUnificationError :: UnificationError -> String
showUnificationError (Clash      a b)
  = "Clash: " ++ showTypeExpr (toTypeExpr a)
    ++ " = " ++ showTypeExpr (toTypeExpr b)
showUnificationError (OccurCheck v t)
  = "OccurCheck: Variable " ++ showTypeExpr (toTypeExpr (TermVar v))
    ++ " occurs in " ++ showTypeExpr (toTypeExpr t)

--- Generates a string representation from a type expression
showTypeExpr :: TypeExpr -> String
showTypeExpr (TVar          n) = "(TVar " ++ (show n) ++ ")"
showTypeExpr (TCons (m, n) ps) = "(TCons (" ++ m ++ ", " ++ n ++ ") [" ++
                                 (concat $ map showTypeExpr ps) ++ "])"
showTypeExpr (FuncType    a b) = "(FuncType " ++ (showTypeExpr a) ++ ", " ++
                                 (showTypeExpr b) ++ ")"

-- ---------------------------------------------------------------------------
-- 5. Functions for normalization of type variables.
--    Renumbers type variables in a function starting from 0.
-- ---------------------------------------------------------------------------

-- We need to keep the next variable number to assign
-- and a mapping from existing variable numbers to newly assigned ones.
-- Note that we actually do not need the error functionality, but reuse
-- the ES monad to avoid writing our own state monad.
type NormState    = (Int, FM Int Int)
type NormStateM a = ES () NormState a

--- Normalizes the type variable numbers in the given function.
--- The parameters of the function are always the first types to be
--- renumbered so they are assigned the lowest numbers.
---
--- @param func - the function to renumber
--- @return the renumbered function
normFunc :: AFuncDecl TypeExpr -> ES String s (AFuncDecl TypeExpr)
normFunc (AFunc f a v t r) = case evalES norm (0, emptyFM (<)) of
    Left _   -> failES $ "Normalization of function " ++ show f ++ " failed"
    Right fd -> returnES fd
  where norm  = liftES2 (AFunc f a v) (normType t) (normRule r)

--- Recursively normalizes type variable numbers in the given type expression.
--- State is managed using the state monad, see normExpr for details.
---
--- @param type - the type expression to normalize
--- @return the normalized type expression
normType :: TypeExpr -> NormStateM (TypeExpr)
normType (TVar        i) = gets >+= \(n, fm) -> case lookupFM fm i of
  Nothing -> puts (n + 1, addToFM fm i n) >+ returnES (TVar n)
  Just n' -> returnES (TVar n')
normType (TCons   q tys) = TCons q `liftES` mapES normType tys
normType (FuncType  a b) = liftES2 FuncType (normType a) (normType b)

--- Normalize a rule.
normRule :: ARule TypeExpr -> NormStateM (ARule TypeExpr)
normRule (ARule     ty vs e) = liftES3 ARule (normType ty) (mapES normSnd vs)
                                             (normExpr e)
normRule (AExternal ty    s) = (\ty' -> AExternal ty' s) `liftES` normType ty

--- Normalizes type variable numbers in an expression. The next number
--- to assign and a map from existing variable numbers to newly assigned
--- ones are managed using the state monad.
---
--- @param state - the current state
--- @param expr - the expression to normalize
--- @return the new state and normalized expression inside the state monad
normExpr :: AExpr TypeExpr -> NormStateM (AExpr TypeExpr)
normExpr (AVar  t       v) = (\t' -> AVar t' v) `liftES` normType t
normExpr (ALit  t       l) = (\t' -> ALit t' l) `liftES` normType t
normExpr (AComb t ct f es) = liftES3 (\t' -> AComb t' ct) (normType t)
                                     (normSnd f) (mapES normExpr es)
normExpr (ALet  t    ds e) = liftES3 ALet (normType t) (mapES normBinding ds)
                                          (normExpr e)
  where normBinding (v, x) = normExpr x >+= \x' -> returnES (v, x')
normExpr (AOr   t     a b) = liftES3 AOr (normType t) (normExpr a) (normExpr b)
normExpr (ACase t ct e bs) = liftES3 (\t' -> ACase t' ct) (normType t)
                                     (normExpr e) (mapES normBranch bs)
normExpr (AFree t    vs e) = liftES3 AFree  (normType t) (mapES normSnd vs)
                                            (normExpr e)
normExpr (ATyped t   e te) = liftES3 ATyped (normType t) (normExpr e)
                                            (normType te)

normSnd :: (a, TypeExpr) -> NormStateM (a, TypeExpr)
normSnd (a, ty) = normType ty >+= \ty' -> returnES (a, ty')

--- Normalizes type variable numbers in a branch. State is managed
--- using the state monad, see normExpr for details.
---
--- @param state - the current state
--- @param branch - the branch to normalize
--- @return the new state and normalized branch inside the state monad
normBranch :: ABranchExpr TypeExpr -> NormStateM (ABranchExpr TypeExpr)
normBranch (ABranch p e) = liftES2 ABranch (normPattern p) (normExpr e)

normPattern (APattern  t c vs) = liftES3 APattern (normType t) (normSnd c)
                                                  (mapES normSnd vs)
normPattern (ALPattern t    l) = (\t' -> ALPattern t' l) `liftES` normType t
