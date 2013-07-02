------------------------------------------------------------------------------
--- Library to annotate the expressions of a FlatCurry program
--- with type information.
---
--- @author  Jonas Oberschweiber, Björn Peemöller, Michael Hanus
--- @version July 2013
------------------------------------------------------------------------------
module Inference
  ( TypeEnv, getTypeEnv, getTypeEnvFromProgEnv
  , inferProg, inferProgFromProgEnv
  , inferProgEnv, inferFunction, inferFunctionEnv
  ) where

import FiniteMap
import List (find)

import AnnotatedFlatCurry
import FlatCurry
import Unification

-- ---------------------------------------------------------------------------
-- public functions
-- ---------------------------------------------------------------------------

--- Extract the type environment from the given Prog.
---
--- @param p - the Prog
--- @return a type environment
getTypeEnv :: Prog -> IO TypeEnv
getTypeEnv p = do
  imps <- extractImported p
  return (extractKnownTypes (p : imps))

--- Infers the type of a whole program.
---
--- @param p - the Prog to infer
--- @return the inferred program or an error
inferProg :: Prog -> IO (Either String (AProg TypeExpr))
inferProg p = getTypeEnv p >>= \te -> return (inferProgEnv te p)

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
inferProgEnv te p = evalES (annProg p >+= inferAProg te) initTIM

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
  Just f  -> evalES (annFunc f >+= inferFunc te) initTIM
 where hasName f (Func g _ _ _ _) = f == g

-- ---------------------------------------------------------------------------
-- Type environment
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

--- Reads the interfaces of all modules imported into the given Prog.
---
--- @param p - the Prog whose imports should be read
--- @return the list of interface Progs
extractImported :: Prog -> IO [Prog]
extractImported (Prog _ is _ _ _) = mapIO readFlatCurryInt is

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
-- A combination of Error and state monad like `ErrorT State` in Haskell
-- ---------------------------------------------------------------------------

--- Error state monad.
type ES e s a = s -> Either e (a, s)

--- Evaluate an `ES` monad
evalES :: ES e s a -> s -> Either e a
evalES m s = case m s of
  Left  e      -> Left e
  Right (x, _) -> Right x

--- Lift a value into the `ES` monad
returnES :: a -> ES e s a
returnES x s = Right (x, s)

--- Bind of the `ES` monad
(>+=) :: ES e s a -> (a -> ES e s b) -> ES e s b
(m >+= f) s = case m s of
  Left  e       -> Left e
  Right (x, s') -> f x s'

--- Sequence operator of the `ES` monad
(>+) :: ES e s a -> ES e s b -> ES e s b
m >+ n = m >+= \_ -> n

--- Failing computation in the `ES` monad
failES :: e -> ES e s a
failES e _ = Left e

--- Retrieve the current state
gets :: ES e s s
gets s = Right (s, s)

--- Replace the current state
puts :: s -> ES e s ()
puts s _ = Right ((), s)

--- Lift the given unary function into the monad.
liftES :: (a -> b) -> ES e s a -> ES e s b
liftES f m = m >+= (returnES . f)

--- Lift the given binary function into the monad.
liftES2 :: (a -> b -> c) -> ES e s a -> ES e s b -> ES e s c
liftES2 f m n = m >+= \x -> n >+= \y -> returnES (f x y)

--- Append two lists yielded by monadic computations.
(++=) :: TIM [a] -> TIM [a] -> TIM [a]
(++=) = liftES2 (++)

--- Map a monadic function on all elements of a list by sequencing
--- the effects.
mapES :: (a -> ES e s b) -> [a] -> ES e s [b]
mapES _ []       = returnES []
mapES f (x : xs) = f x        >+= \y  ->
                   mapES f xs >+= \ys ->
                   returnES (y:ys)

--- Same as `concatMap`, but for a monadic function.
concatMapES :: (a -> ES e s [b]) -> [a] -> ES e s [b]
concatMapES f xs = concat `liftES` mapES f xs

--- Same as `mapES` but with an additional accumulator threaded.
mapAccumES :: (a -> b -> ES e s (a, c)) -> a -> [b] -> ES e s (a, [c])
mapAccumES _ s []       = returnES (s, [])
mapAccumES f s (x : xs) = f s x >+= \(s', y) ->
                          mapAccumES f s' xs >+= \(s'', ys) ->
                          returnES (s'', y:ys)

--- Type Inference Monad.
--- The monad contains an `Int` value for fresh type variable generation
--- and a mapping from variable indices to their associated type
--- variables. It returns a `String` if an error occured
type TIM a = ES String (Int, FM Int TypeExpr) a

--- Initial TIM state.
initTIM = (0, emptyFM (<))

--- Retrieve the next fresh type variable.
nextTVar :: TIM TypeExpr
nextTVar = gets >+= \ (n, var2tvar) ->
           puts (n + 1, var2tvar) >+ returnES (TVar n)

--- Intialize the "variable to type variable mapping", i.e., delete all
--- associations.
initVar2TVar :: TIM ()
initVar2TVar = gets >+= \ (n,_) -> puts (n, emptyFM (<))

--- Insert a new variable/type variable association.
insertVar2TVar :: Int -> TypeExpr -> TIM ()
insertVar2TVar var tvar =
  gets >+= \ (n,var2tvar) -> puts (n, addToFM var2tvar var tvar)

--- Look up the type variable associated to a variable.
lookupVar2TVar :: Int -> TIM (Maybe TypeExpr)
lookupVar2TVar var =
  gets >+= \ (_,var2tvar) -> returnES (lookupFM var2tvar var)

-- ---------------------------------------------------------------------------
-- Annotation functions, traversing the AST and inserting fresh type variables
-- ---------------------------------------------------------------------------

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
annFunc (Func qn a v t r) = AFunc qn a v t `liftES` annRule r

--- Converts the Rule to an ARule, inserting TVars into all expressions.
---
--- @param n - the first TVar number to use
--- @return the ARule and the new next TVar number in an TIM
annRule :: Rule -> TIM (ARule TypeExpr)
annRule (Rule  vs e) =
  ARule vs `liftES` (initVar2TVar >+ mapES initVarType vs >+ annExpr e)
 where initVarType v = nextTVar >+= \ty -> insertVar2TVar v ty
annRule (External e) = returnES (AExternal e)

--- Converts the Expr to an AExpr, inserting TVars into all
--- expressions
---
--- @param n - the first TVar number to use
--- @return the AExpr and the new next TVar number in an TIM
annExpr :: Expr -> TIM (AExpr TypeExpr)
annExpr (Var       i) =
  lookupVar2TVar i >+=
  maybe (error $ "annExpr: variable "++show i++" not initialized with a type!")
        (\ty -> returnES (AVar ty i))
annExpr (Lit       l) = nextTVar >+= \ty -> returnES (ALit ty l)
annExpr (Comb t q es) = nextTVar >+= \ty ->
                        mapES annExpr es >+= \aes ->
                        returnES (AComb ty t q aes)
annExpr (Case t e bs) = nextTVar  >+= \ty ->
                        annExpr e >+= \e' ->
                        mapES annBranch bs >+= \bs' ->
                        returnES (ACase ty t e' bs')
annExpr (Or      a b) = nextTVar  >+= \ty ->
                        annExpr a >+= \ a' ->
                        annExpr b >+= \ b' ->
                        returnES (AOr ty a' b')
annExpr (Let    ds e) = mapES annLVar vs >+
                        mapES annExpr bes >+= \baes ->
                        nextTVar  >+= \ty ->
                        annExpr e >+= \e' ->
                        returnES (ALet ty (zip vs baes) e')
 where (vs,bes) = unzip ds
       annLVar v = checkShadowing v >+ nextTVar >+= insertVar2TVar v
annExpr (Free   vs e) = nextTVar  >+= \ty ->
                        mapES annFree vs >+= \vs' ->
                        annExpr e >+= \e' ->
                        returnES (AFree ty vs' e')
  where annFree v = checkShadowing v >+
                    nextTVar >+= \ty -> insertVar2TVar v ty >+
                    returnES (v, ty)
annExpr (Typed   e t) = nextTVar  >+= \ty ->
                        annExpr e >+= \ e' ->
                        returnES (ATyped ty e' t)

--- Checks whether a local variable is already defined which indicates
--- variable shadowing that is not allowed in FlatCurry files.
--- This is our basic assumption in this type inferencer, otherwise it
--- must be extended.
checkShadowing v =
  lookupVar2TVar v >+=
  maybe (returnES ())
        (error $ "annExpr: shadowing with variable "++show v++" occurred!")

--- Converts the BranchExpr to an ABranchExpr, inserting TVars
--- into all expressions
---
--- @param n - the first TVar number to use
--- @return the ABranchExpr and the new next TVar number in an TIM
annBranch :: BranchExpr -> TIM (ABranchExpr TypeExpr)
annBranch (Branch p e) = annPattern p >+= \p' ->
                         annExpr    e >+= \e' ->
                         returnES (ABranch p' e')

--- Converts the Pattern into an APattern, inserting a TVar
--- into the pattern
---
--- @param n - the TVar number to use
--- @return the APattern and the new next TVar number in an TIM
annPattern :: Pattern -> TIM (APattern TypeExpr)
annPattern (Pattern c vs) = mapES annPVar vs >+
                            nextTVar  >+= \ty ->
                            returnES (APattern ty c vs)
 where annPVar v = checkShadowing v >+
                   nextTVar >+= \ty -> insertVar2TVar v ty
annPattern (LPattern   l) = nextTVar  >+= \ty ->
                            returnES (ALPattern ty l)

-- ---------------------------------------------------------------------------
-- Type inference
-- ---------------------------------------------------------------------------

--- Infers all types in the given program.
---
--- @param p - the program to infer
--- @param env - the type environment to use for type lookups
--- @param n - the next fresh TVar number
--- @return the inferred program or an error
inferAProg :: TypeEnv -> AProg TypeExpr -> TIM (AProg TypeExpr)
inferAProg env (AProg mid is td fd od)
  = inferFuncs env fd >+= \fd' -> returnES (AProg mid is td fd' od)

--- Infers all types in the given functions.
---
--- @param fs - the function declarations
--- @param env - the type environment to use for type lookups
--- @param n - the next fresh TVar number
--- @return the inferred functions or an error
inferFuncs :: TypeEnv -> [AFuncDecl TypeExpr] -> TIM [AFuncDecl TypeExpr]
inferFuncs env fs = mapES (inferFunc env) fs

--- Infers all types in the given function.
---
--- @param env - the type environment to use for type lookups
--- @param n - the next fresh TVar number
--- @param f - the function
--- @return the inferred function or an error
inferFunc :: TypeEnv -> AFuncDecl TypeExpr -> TIM (AFuncDecl TypeExpr)
inferFunc env (AFunc f a v ty r@(ARule vs e)) =
  inferRule env r >+= \r' ->
  extractFuncType vs e >+= \ty2 ->
  renameTVars ty >+= \ty' ->
  checkInstance ty' ty2 >+
  returnES (normFunc $ AFunc f a v ty r')
inferFunc _ a@(AFunc _ _ _ _ (AExternal _)) = returnES a

--- Check whether the first type is an instance of the second type.
--- @param ty1 - Specific type
--- @param ty2 - General type
checkInstance :: TypeExpr -> TypeExpr -> TIM ()
checkInstance ty1 ty2 = checkFailure (unify (fromTypeEqs [ty1 =.= ty2]))
                        >+ returnES ()

--- Infer the type for a rule.
inferRule :: TypeEnv -> ARule TypeExpr -> TIM (ARule TypeExpr)
inferRule env (ARule    vs e) = ARule vs `liftES` infer env e
inferRule _   e@(AExternal _) = returnES e

--- Infers the types in the given expression.
---
--- @param env - the type environment to use for type lookups
--- @param ex - the expression
--- @param n - the next fresh TVar number
--- @return the inferred expression or an error inside an TIM
---         carrying the next fresh TVar number
infer :: TypeEnv -> AExpr TypeExpr -> TIM (AExpr TypeExpr)
infer k e = genTypeEqs k e >+= \t ->
            checkFailure (unify (fromTypeEqs t)) >+= \ sigma ->
            returnES (substExpr sigma e)

--- Converts a unification error into a string error.
checkFailure :: Either UnificationError Subst -> TIM Subst
checkFailure (Left  err) = failES (showUnificationError err)
checkFailure (Right sub) = returnES sub

--- Generates a function type from the given rule. Traverses the list of
--- parameter variable indices in order and searches for the variable's
--- type in the rule's expression. If a type is found, it becomes the type
--- of that parameter. If no type is found, the parameter is given a fresh
--- type variable as its type (hence the maximum type variable index). The
--- top level expression's type becomes the function's return type.
---
--- @param n - the maximum type variable index found in the rule's expression,
---            plus one
--- @param rule - the rule to generate a function type for
--- @return the function type
extractFuncType :: [VarIndex] -> AExpr TypeExpr -> TIM TypeExpr
extractFuncType []     e = returnES $ exprType e
extractFuncType (v:vs) e = case extractVarType v e of
  Nothing -> nextTVar >+= \tv -> FuncType tv `liftES` extractFuncType vs e
  Just t  -> FuncType t `liftES` extractFuncType vs e

--- Searches the expression for the type of the variable with the given
--- index.
---
--- @param n - the index of the variable to look for
--- @param e - the expression to search in
--- @return the variable's type or Nothing
extractVarType :: VarIndex -> AExpr TypeExpr -> Maybe TypeExpr
extractVarType n (AVar       t i) | n == i    = Just t
                                  | otherwise = Nothing
extractVarType _ (ALit       _ _) = Nothing
extractVarType n (AComb _ _ _ es) = foldr (<|>) Nothing
                                  $ map (extractVarType n) es
extractVarType n (ALet    _ bs e) = foldr (<|>) (extractVarType n e)
                                  $ map (extractVarType n . snd) bs
extractVarType n (AFree   _ vs e) = lookup n vs <|> extractVarType n e
extractVarType n (AOr      _ a b) = extractVarType n a <|> extractVarType n b
extractVarType n (ACase _ _ e bs) = foldr (<|>) (extractVarType n e)
                                  $ map extractVarTypeBranch bs
 where extractVarTypeBranch (ABranch _ ex) = extractVarType n ex
extractVarType n (ATyped   _ e _) = extractVarType n e

(<|>) :: Maybe a -> Maybe a -> Maybe a
Nothing    <|> r = r
l@(Just _) <|> _ = l

-- ---------------------------------------------------------------------------
-- Functions for normalization of type variables.
-- Renumbers type variables in a function starting from 0.
-- ---------------------------------------------------------------------------

-- We need to keep the next variable number to assign
-- and a mapping from existing variable numbers to newly assigned ones.
type NormState    = (Int, FM Int Int)
type NormStateM a = ES () NormState a

--- Normalizes the type variable numbers in the given function.
--- The parameters of the function are always the first types to be
--- renumbered so they are assigned the lowest numbers.
---
--- @param func - the function to renumber
--- @return the renumbered function
normFunc :: AFuncDecl TypeExpr -> AFuncDecl TypeExpr
normFunc (AFunc f a v t (ARule  vs e)) = case evalES norm (0, emptyFM (<)) of
  Left _   -> error "Inference.normFunc"
  Right fd -> fd
 where norm = normType t >+= \t' ->
              normExpr e >+= \e' ->
              returnES (AFunc f a v t' (ARule vs e'))
normFunc a@(AFunc _ _ _ _ (AExternal _)) = a

--- Recursively normalizes type variable numbers in the given type expression.
--- State is managed using the state monad, see normExpr for details.
---
--- @param type - the type expression to normalize
--- @return the normalized type expression
normType :: TypeExpr -> NormStateM (TypeExpr)
normType (TVar        i)
  = gets >+= \(n, fm) -> case lookupFM fm i of
    Nothing -> puts (n + 1, addToFM fm i n) >+ returnES (TVar n)
    Just n' -> returnES (TVar n')
normType (TCons   q tys) = TCons q `liftES` mapES normType tys
normType (FuncType  a b) = liftES2 FuncType (normType a) (normType b)

--- Normalizes type variable numbers in an expression. The next number
--- to assign and a map from existing variable numbers to newly assigned
--- ones are managed using the state monad.
---
--- @param state - the current state
--- @param expr - the expression to normalize
--- @return the new state and normalized expression inside the state monad
normExpr :: AExpr TypeExpr -> NormStateM (AExpr TypeExpr)
normExpr (AVar  t       v) = normType t >+= \t' -> returnES (AVar t' v)
normExpr (ALit  t       l) = normType t >+= \t' -> returnES (ALit t' l)
normExpr (AComb t ct f es) = normType t       >+= \t' ->
                             mapES normExpr es >+= \es' ->
                             returnES (AComb t' ct f es')
normExpr (ALet  t    ds e) = normType t          >+= \t' ->
                             mapES normBinding ds >+= \ds' ->
                             normExpr e          >+= \e' ->
                             returnES (ALet t' ds' e')
  where normBinding (v, x) = normExpr x >+= \x' -> returnES (v, x')
normExpr (AOr   t     a b) = normType t >+= \t' ->
                             normExpr a >+= \a' ->
                             normExpr b >+= \b' ->
                             returnES (AOr t' a' b')
normExpr (ACase t ct e bs) = normType t         >+= \t' ->
                             normExpr e         >+= \e' ->
                             mapES normBranch bs >+= \bs' ->
                             returnES (ACase t' ct e' bs')
normExpr (AFree t    vs e) = normType t >+= \t' ->
                             mapES normVar vs >+= \vs' ->
                             normExpr e >+= \e' ->
                             returnES (AFree t' vs' e')
  where normVar (v, tv) =  normType tv >+= \t' -> returnES (v, t')
normExpr (ATyped t   e te) = normType t >+= \t' ->
                             normExpr e >+= \e' ->
                             returnES (ATyped t' e' te)

--- Normalizes type variable numbers in a branch. State is managed
--- using the state monad, see normExpr for details.
---
--- @param state - the current state
--- @param branch - the branch to normalize
--- @return the new state and normalized branch inside the state monad
normBranch :: ABranchExpr TypeExpr -> NormStateM (ABranchExpr TypeExpr)
normBranch (ABranch p e) = liftES2 ABranch (normPattern p) (normExpr e)
 where
  normPattern (APattern  t c vs) = normType t >+= \t' ->
                                   returnES (APattern t' c vs)
  normPattern (ALPattern t    l) = normType t >+= \t' ->
                                   returnES (ALPattern t' l)

-- ---------------------------------------------------------------------------
-- Functions for generating type equations
-- ---------------------------------------------------------------------------

--- Type equations
type TypeEqs = [(TypeExpr, TypeExpr)]

--- Smart constructor for type equation
(=.=) :: TypeExpr -> TypeExpr -> (TypeExpr, TypeExpr)
ty1 =.= ty2 = (ty1, ty2)

showTypeEqs :: TypeEqs -> String
showTypeEqs = unlines . map showEquation
  where showEquation (l, r) = show l ++ " =.= " ++ show r

--- Recursively generate equations for the unifier from an expression.
---
--- @param env - the type environment
--- @param n - the next fresh TVar number
--- @param ex - the expression
--- @return a list of equations or an error inside an TIM carrying
---         the next free TVar number
genTypeEqs :: TypeEnv -> AExpr TypeExpr -> TIM TypeEqs
-- Recursively generate equations for each argument expression and
-- match the types of the argument expressions to the types expected by
-- the function or constructor.
-- Whatever is left must be the result type of the call.
genTypeEqs env (AComb ty _ f es)
  = concatMapES (genTypeEqs env) es ++=
    (lookupTypeRename env f >+= \t -> returnES (matchArgs ty t es))
-- Generate equations for the subject and the branches.
genTypeEqs env (ACase ty _ e bs)
  = concatMapES (genBranchTypeEqs env ty e) bs ++= genTypeEqs env e
-- No equations to generate.
genTypeEqs _   (AVar _ _)        = returnES []
-- The type of the expression is equal to the type of the literal.
genTypeEqs _   (ALit ty l)       = returnES [ty =.= literalType l]
-- Recursively generate equations for each of the argument expressions.
-- The type of the expression must be equal to the types
-- of both argument expressions.
-- The types of the argument expressions must be equal to each other.
genTypeEqs env (AOr ty a b)
  = genTypeEqs env a ++= genTypeEqs env b ++=
    returnES [exprType a =.= ty, exprType b =.= ty]
-- Generate equations for all bound expressions and for the inner expression.
-- Equate the type of each occurence of a bound variable
-- in the inner expression or some bound expression to the type of
-- the expression which the variable is bound to.
-- The type of the expression itself must be equal to the type of the inner
-- expression.
genTypeEqs env (ALet ty bs e)
  = let bvartypes = map (\(v, b) -> (v, exprType b)) bs
    in concatMapES (genVarPairs env bvartypes) (e : map snd bs) ++=
       concatMapES (genTypeEqs env) (map snd bs) ++=
       genTypeEqs env e ++= returnES [ty =.= exprType e]
-- Generate equations for the inner expression.
-- The type of the expression itself must be equal
-- to the type of the inner expression.
genTypeEqs env (AFree ty _ e)
  = genTypeEqs env e ++= returnES [ty =.= exprType e]
-- Recursively generate equations for each of the argument expression.
-- The type of the expression must be equal to the type of the
-- argument expression. In addition, it must be also equal to the given type.
genTypeEqs env (ATyped ty e ty')
  = genTypeEqs env e ++= returnES [exprType e =.= ty, exprType e =.= ty']

--- Matches the given parameter expressions to the given type.
--- Returns a list of equation pairs. The "leftovers" are assigned
--- to the TypeExpr given as the first parameter.
--- May be used on FuncCall, FuncPartCall, ConsCall and ConsPartCall.
---
--- @param t - the type to assign the "leftover" type from the call to
--- @param f - the function type to match to
--- @param ps - the parameter expressions
--- @return a list of equations
matchArgs :: TypeExpr -> TypeExpr -> [AExpr TypeExpr] -> TypeEqs
matchArgs e a              []     = [e =.= a]
matchArgs e (FuncType a b) (p:ps) = (exprType p =.= a) : matchArgs e b ps

--- Generate equation pairs for a branch.
---
---  This consists of:
---    - generating equations for the branch's expresson
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
--- @param env - the type environment
--- @param ct - the parent case expression's type
--- @param subj - the case's subject expression
--- @param b - the branch
--- @return a list of equations or an error inside an TIM carrying
---         the next fresh TVar number
genBranchTypeEqs :: TypeEnv -> TypeExpr -> AExpr TypeExpr
                 -> ABranchExpr TypeExpr -> TIM TypeEqs
genBranchTypeEqs env ty e (ABranch p@(APattern pt f _) be) =
  lookupTypeRename env f >+= \t ->
  genVarPairs env (patternVarTypes t p) be ++=
  genTypeEqs env be ++= returnES [exprType e =.= patternType t p
    , ty =.= exprType be, pt =.= patternType t p]
genBranchTypeEqs env ty e (ABranch (ALPattern pt l) be) =
  genTypeEqs env be ++= returnES [exprType e =.= literalType l
    , ty =.= exprType be, pt =.= literalType l]

--- Extract the "type" of the pattern.
patternType :: TypeExpr -> APattern TypeExpr -> TypeExpr
patternType t (APattern _ _ ps) = fst $ matchPatternType t ps

--- Extract the mapping of variable indices to types as bound by
--- the pattern (the types of the deconstruction variables).
---
--- @param t - the type of the pattern's constructor
--- @param p - the pattern
--- @return a list of bindings from variable indices to types
patternVarTypes :: TypeExpr -> APattern TypeExpr -> [(VarIndex, TypeExpr)]
patternVarTypes t (APattern _ _ ps) = snd $ matchPatternType t ps

--- Match the given variable indices to the given TypeExpr.
--- Returns a list of variable index/TypeExpr pairs and an extra TypeExpr
--- representing the "leftovers".
---
--- @param t - the function type of the constructor used in the pattern
--- @param vis - the list of variable indices used for deconstruction
--- @return a tuple of the "leftover" type from the matching
---         (i.e. the type of the datatype the pattern matches against)
---         and a list of mappings from variable indices to types
---         (the types of the parts of the deconstruction)
matchPatternType :: TypeExpr -> [VarIndex]
                 -> (TypeExpr, [(VarIndex, TypeExpr)])
matchPatternType e              []     = (e, [])
matchPatternType (FuncType a b) (p:ps) = (fst rest, (p, a) : (snd rest))
  where rest = matchPatternType b ps

--- Recursively search the expression and generate an equation for every AVar
--- that we're given a type for in the first parameter.
---
--- @param env - the type environment
--- @param vs - a list of bindings from variables to types
--- @param ex - the expression to search
--- @return a list of type equations or an error inside an TIM carrying
---         the next fresh TVar number
genVarPairs :: TypeEnv -> [(VarIndex, TypeExpr)] -> AExpr TypeExpr
            -> TIM TypeEqs
genVarPairs env vs (AComb _ _ _ ps) = concatMapES (genVarPairs env vs) ps
genVarPairs env vs (ACase _ _ s bs)
  = concatMapES genBranchVarPairs bs ++= genVarPairs env vs s
  where genBranchVarPairs (ABranch _ e) = genVarPairs env vs e
genVarPairs _   vs (AVar      ty v) = case lookup v vs of
  Just ty' -> returnES (if ty==ty' then [] else [ty =.= ty'])
  Nothing  -> returnES []
genVarPairs _   _  (ALit       _ _) = returnES []
genVarPairs env vs (AOr      _ a b)
  = genVarPairs env vs a ++= genVarPairs env vs b
genVarPairs env vs (ALet    _ bs e)
  = concatMapES (genVarPairs env vs) (map snd bs) ++= genVarPairs env vs e
genVarPairs env vs (AFree    _ _ e)
  = genVarPairs env vs e
genVarPairs env vs (ATyped   _ e _)
  = genVarPairs env vs e

--- Extract the type of a Literal.
literalType :: Literal -> TypeExpr
literalType (Intc   _) = TCons ("Prelude", "Int"  ) []
literalType (Floatc _) = TCons ("Prelude", "Float") []
literalType (Charc  _) = TCons ("Prelude", "Char" ) []

--- Extract the TypeExpr from an annotated Expr
exprType :: AExpr TypeExpr -> TypeExpr
exprType = exprAnnotation

--- Looks up a type in a type environment and renames all type variables
--- in the type (replaces them with fresh ones).
---
--- @param env - the type environment
--- @param q - the qualified name of the type to look up
--- @return the found and renamed type or an error
lookupTypeRename :: TypeEnv -> QName -> TIM TypeExpr
lookupTypeRename env f = case lookupType env f of
  Nothing -> failES ("Unknown type: " ++ show f)
  Just t  -> renameTVars t

--- Renames all TVars inside the given type expression.
---
--- @param ty - the type expression
--- @return The renamed type expression
renameTVars :: TypeExpr -> TIM TypeExpr
renameTVars ty = snd `liftES` rename [] ty
 where
  rename ren (TVar       i) = case lookup i ren of
    Just j  -> returnES (ren, j)
    Nothing -> nextTVar >+= \j -> returnES ((i, j) : ren, j)
  rename ren (FuncType a b) = rename ren  a >+= \ (ren1, a') ->
                              rename ren1 b >+= \ (ren2, b') ->
                              returnES (ren2, FuncType a' b')
  rename ren (TCons  t tys) = mapAccumES rename ren tys >+= \(ren', tys') ->
                              returnES (ren', TCons t tys')

-- ---------------------------------------------------------------------------
-- Functions for applying substitutions to expressions
-- ---------------------------------------------------------------------------

--- Applies a substitution to a type expression.
---
--- @param sub - the substitution
--- @param ex - the expression
--- @return the expression with the substitution applied
substExpr :: Subst -> AExpr TypeExpr -> AExpr TypeExpr
substExpr sub (AComb ty t f ps) = AComb (subst sub ty) t f
                                        (map (substExpr sub) ps)
substExpr sub (AVar  ty      k) = AVar  (subst sub ty) k
substExpr sub (ACase ty t e bs) = ACase (subst sub ty) t (substExpr sub e)
                                        (map (substBranch sub) bs)
substExpr sub (ALit  ty      l) = ALit  (subst sub ty) l
substExpr sub (AOr   ty    a b) = AOr   (subst sub ty) (substExpr sub a)
                                                       (substExpr sub b)
substExpr sub (ALet  ty   bs e) = ALet  (subst sub ty) (map substBinding bs)
                                        (substExpr sub e)
  where substBinding (v, b) = (v, substExpr sub b)
substExpr sub (AFree ty   vs e) = AFree (subst sub ty) (map substFree vs)
                                        (substExpr sub e)
  where substFree (v, vty) = (v, subst sub vty)
substExpr sub (ATyped ty e ty') = ATyped (subst sub ty) (substExpr sub e)
                                                        (subst sub ty')

--- Applies a substitution to a branch expression.
---
--- @param sub - the substitution
--- @param b - the branch
--- @return the branch with the substitution applied
substBranch :: Subst -> ABranchExpr TypeExpr -> ABranchExpr TypeExpr
substBranch sub (ABranch p e) = ABranch (substPattern sub p) (substExpr sub e)

--- Applies a substitution to a pattern.
---
--- @param sub - the substitution
--- @param p - the pattern
--- @return the pattern with the substitution applied
substPattern :: Subst -> APattern TypeExpr -> APattern TypeExpr
substPattern sub (APattern  t f vs) = APattern  (subst sub t) f vs
substPattern sub (ALPattern t    l) = ALPattern (subst sub t) l

--- Looks up a type in a substitution and converts the resulting Term
--- to a TypeExpr. Returns a given default value if the lookup fails.
---
--- @param t - the type to look up
--- @param e - the default value
--- @param sub - the substitution to search in
--- @return either the looked-up and converted type or the default type
subst :: Subst -> TypeExpr -> TypeExpr
subst sub e@(TVar     n) = maybe e toTypeExpr (lookupSubst sub n)
subst sub (TCons  t tys) = TCons t (map (subst sub) tys)
subst sub (FuncType a b) = FuncType (subst sub a) (subst sub b)

-- ---------------------------------------------------------------------------
-- Functions for interfacing with the Unification module
-- ---------------------------------------------------------------------------

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

--- Converts a string to a qualified name
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
showUnificationError (Clash a b)
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
