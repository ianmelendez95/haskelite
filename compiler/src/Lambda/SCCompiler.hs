{-# LANGUAGE TupleSections #-}

module Lambda.SCCompiler
  ( Prog (..)
  , SC (..)
  , compileExpr
  , compileSCs) where

import Data.Maybe (fromMaybe)
import Data.Either (partitionEithers)
import Data.List (foldl1', sortOn, (\\), intersect)
import Data.Bifunctor (second)
import qualified Control.Monad.State.Lazy as ST

import qualified Data.Map.Lazy as Map

-- import Control.Exception (assert)
-- import Debug.Trace

import qualified Lambda.Syntax as S
import Lambda.AlphaConv (alphaConvertWithMap)


data Prog = Prog [SC] S.Exp

-- traverseProgs :: [Prog] -> ([SC], [S.Exp])
-- traverseProgs = mconcat . map (\(Prog scs expr) -> (scs, [expr]))

appendProgs :: (S.Exp -> S.Exp -> S.Exp) -> Prog -> Prog -> Prog
appendProgs ef (Prog scs1 e1) (Prog scs2 e2) = Prog (scs1 ++ scs2) (ef e1 e2)

-- mapProg :: (S.Exp -> S.Exp) -> Prog -> Prog
-- mapProg f (Prog scs e) = Prog scs (f e)


data SuperComb = Let (String, SuperComb) SuperComb
               | Letrec [(String, SuperComb)] SuperComb
               | Term S.Term
               | App SuperComb SuperComb

               -- 'n'ary combinator, one that takes arguments
               -- NComb name params free_vars body
               | NComb String [String] SuperComb
               deriving Show

data SC = SC {
  scName :: String,
  scParams :: [String],
  scBody :: S.Exp
}

instance Show Prog where
  show (Prog scs expr) = unlines (map show scs ++ [replicate 80 '-', show expr])

instance Show SC where
  show (SC name args expr) = unwords (name : args) ++ " = " ++ show expr

type SCS = ST.State SCEnv

data SCEnv = SCEnv {
  scenvSCCount :: Integer,
  scenvCombs :: [SC]
}


mkApp :: [SuperComb] -> SuperComb
mkApp = foldl1' App

emptySCEnv :: SCEnv
emptySCEnv = SCEnv { scenvSCCount = 0, scenvCombs = [] }

scExpr :: SC -> S.Exp
scExpr (SC _ _ e) = e

newSCName :: SCS String
newSCName =
  do ST.modify (\scenv@SCEnv{ scenvSCCount = c } -> scenv{ scenvSCCount = c+1 })
     cur_count <- ST.gets scenvSCCount
     pure $ '$' : show cur_count

pushSC :: SC -> SCS ()
pushSC sc = ST.modify (\scenv@SCEnv{ scenvCombs = scs } ->
  scenv{ scenvCombs = sc : scs })

renameSC :: String -> String -> SCS ()
renameSC orig_name new_name =
  do (SC _ sc_args sc_expr) <- popSCByName orig_name
     pushSC (SC new_name sc_args sc_expr)

popSCByName :: String -> SCS SC
popSCByName name =
  do scs <- ST.gets scenvCombs
     let (sc, scs') = pop scs
     ST.modify $ \scenv -> scenv{ scenvCombs = scs' }
     return sc
  where
    pop :: [SC] -> (SC, [SC])
    pop [] = error $ "No supercombinator exists with name: " ++ name
    pop (sc:scs) =
      if scName sc == name
        then (sc, scs)
        else second (sc :) (pop scs)


--------------------------------------------------------------------------------
-- Interface


compileExpr :: S.Exp -> Prog
compileExpr expr = 
  ST.evalState (liftSuperCombs <$> compileExpr' expr) 
               (SCEnv {
                 scenvSCCount = 0,
                 scenvCombs = []
               })
     
     

--------------------------------------------------------------------------------
-- Lambda -> SuperComb


compileExpr' :: S.Exp -> SCS SuperComb

compileExpr' (S.Let bind body) = 
  Let <$> compileBinding bind <*> compileExpr' body

compileExpr' (S.Letrec binds body) = 
  do let bindVars' = bindVars (map fst binds)

     binds' <- mapM ((fmap . fmap) bindVars' . compileBinding) binds
     body'  <- bindVars' <$> compileExpr' body
     pure $ Letrec binds' body'

compileExpr' (S.Term t) = pure $ Term t

compileExpr' (S.Apply e1 e2) = 
  App <$> compileExpr' e1 <*> compileExpr' e2

compileExpr' (S.Lambda var body) =
  do body_sc <- compileExpr' body
     case body_sc of
       NComb sc_name sc_params sc_body ->
         if var `notElem` sc_params
           -- join the supercombinator with the lambda var
           then pure $ NComb sc_name (sc_params ++ [var]) sc_body
           -- wrap the resulting combinator as usual
           else newComb var body_sc
       _ -> newComb var body_sc
  where
    newComb :: String -> SuperComb -> SCS SuperComb
    newComb sc_param sc_body =
      do sc_name <- newSCName
         pure $ NComb sc_name [sc_param] (bindVar var sc_body)


-- compiles the value as normal, but also 
-- when value is an nary combinator, 
-- appends the binding name to the combinator name 
-- for debugging purposes
compileBinding :: (String, S.Exp) -> SCS (String, SuperComb)
compileBinding (bvar, bvalue) = 
  do sc_bvalue <- compileExpr' bvalue
     let sc_bvalue' = case sc_bvalue of 
                        NComb sc_name sc_params sc_body -> 
                          NComb (sc_name ++ bvar) sc_params sc_body
                        _ -> sc_bvalue
     pure (bvar, sc_bvalue')

-- Establishes a new bound variable in the supercombinator
-- context.
-- As a consequence, all contained supercombinators have the new 
-- bound variable as a parameter (if variable is free within the combinator),
-- and the combinator is replaced with an application of said combinator to 
-- the bound variable
bindVar :: String -> SuperComb -> SuperComb
bindVar var l@(Let (bvar, bval) body) = 
  if var == bvar then l else Let (bvar, bindVar var bval) (bindVar var body)
bindVar var l@(Letrec binds body) = 
  if any ((== var) . fst) binds
    then l
    else Letrec (map (second (bindVar var)) binds) (bindVar var body)
bindVar _ t@(Term _) = t
bindVar var (App e1 e2) = App (bindVar var e1) (bindVar var e2)
bindVar var c@(NComb c_name c_params c_body) = 
  -- var is bound in combinator, or var is not free in combinator
  if (var `elem` c_params) || (var `notElem` collectFreeVars' c_params c_body)
    then c
    else App (NComb c_name (var : c_params) c_body) (Term (S.Variable var))

-- see: bindVar
bindVars :: [String] -> SuperComb -> SuperComb
bindVars vars l@(Let (bvar, bval) body) = 
  let non_bvar_vars = filter (bvar ==) vars
      bindVars' = bindVars non_bvar_vars
   in if null non_bvar_vars
        then l
        else Let (bvar, bindVars' bval) (bindVars' body)
bindVars vars l@(Letrec binds body) = 
  let non_bvar_vars = vars \\ map fst binds
   in if null non_bvar_vars
        then l
        else let bindVars' = bindVars non_bvar_vars
              in Letrec (map (second bindVars') binds) (bindVars' body)
bindVars _ t@(Term _) = t
bindVars vars (App e1 e2) = App (bindVars vars e1) (bindVars vars e2)
bindVars vars c@(NComb c_name c_params c_body) = 
  -- free_sc_vars = vars (to be bound) that are free in combinator
  let free_sc_vars = vars `intersect` collectFreeVars' c_params c_body
   in if null free_sc_vars
        then c
        else let sc = NComb c_name (free_sc_vars ++ c_params) c_body 
                 sc_args = map (Term . S.Variable) free_sc_vars
              in mkApp (sc : sc_args)


collectFreeVars' :: [String] -> SuperComb -> [String]
collectFreeVars' bound_vars (Let (bvar, bvalue) body) =
  let bound_vars' = bvar : bound_vars
   in collectFreeVars' bound_vars' bvalue ++ collectFreeVars' bound_vars' body
collectFreeVars' bound_vars (Letrec binds body) =
  let bound_vars' = map fst binds ++ bound_vars
   in concatMap (collectFreeVars' bound_vars' . snd) binds ++ collectFreeVars' bound_vars' body
collectFreeVars' bound_vars (Term (S.Variable v)) = [v | v `notElem` bound_vars]
collectFreeVars' _ (Term _) = []
collectFreeVars' bound_vars (App e1 e2) = collectFreeVars' bound_vars e1 ++ collectFreeVars' bound_vars e2
collectFreeVars' bound_vars (NComb _ params body) = collectFreeVars' (params ++ bound_vars) body


--------------------------------------------------------------------------------
-- SuperComb -> Lambda


liftSuperCombs :: SuperComb -> Prog
liftSuperCombs (Let (bvar, bval) body) = 
  appendProgs (\bval' body' -> S.Let (bvar, bval') body') 
              (liftSuperCombs bval) 
              (liftSuperCombs body)
liftSuperCombs (Letrec binds body) = 
  let (binds_scs, binds') = liftBindings binds
      (Prog body_scs body') = liftSuperCombs body
   in Prog (binds_scs ++ body_scs) (S.Letrec binds' body')
  
-- TODO: consider renaming free vars that match named supercombs
liftSuperCombs (Term t) = Prog [] (S.Term t)
liftSuperCombs (App sc1 sc2) = 
  appendProgs S.Apply (liftSuperCombs sc1) (liftSuperCombs sc2)
liftSuperCombs (NComb name params body) = 
  let (Prog b_scs body') = liftSuperCombs body
      new_sc = SC {
        scName = name,
        scParams = params,
        scBody = body'
      }
   in Prog (new_sc : b_scs) (S.mkApply . map S.mkVariable $ name : params)


liftBindings :: [(String, SuperComb)] -> ([SC], [(String, S.Exp)])
liftBindings = mconcat . (map liftBinding)
  where
    -- 'lift' the binding, retrieving the supercombinators
    -- and the new binding
    --
    -- returns the binding as a singleton list so it can be 
    -- simply concatenated
    liftBinding :: (String, SuperComb) -> ([SC], [(String, S.Exp)])
    liftBinding (var, val) = 
      let (Prog scs val') = liftSuperCombs val
        in (scs, [(var, val')])
   


compileSCs :: S.Exp -> Prog
compileSCs = compileExpr

-- compileSCs (S.Letrec bs e) =
--   -- top-level letrecs treated specially, effectively already 
--   -- known to be supercombinators, and are lexical level 0 (totally free)

--   let -- compile the supercombinators from the bindings
--       SCEnv{scenvCombs = scs} = ST.execState (mapM_ (uncurry compileTLBind) bs) emptySCEnv

--       -- rename free instances of binding names with supercombinator names
--       v_names_to_sc_names = Map.fromList (map (\(n, _) -> (n, '$' : n)) bs)
--       renamed_scs = map (\sc@SC{ scBody = body } ->
--                           sc{ scBody = alphaConvertWithMap v_names_to_sc_names body }) scs
--       renamed_e = alphaConvertWithMap v_names_to_sc_names e

--       -- compile the renamed body (which is the 'program' to be run)
--       (Prog prog_scs prog_expr) = compileSCs renamed_e

--    in reduceFullEta $ Prog (prog_scs ++ renamed_scs) prog_expr

-- compileSCs expr =
--   let (expr', SCEnv{ scenvCombs = scs }) =
--         ST.runState (csc [] expr) emptySCEnv
--    in reduceFullEta $ Prog scs expr'


-- compile top level binding
compileTLBind :: String -> S.Exp -> SCS ()
compileTLBind name expr =
  do expr' <- csc [] expr
     let free_vars = filter (not . isSCVar) $ S.freeVariables' [] expr'

     -- If resulting expression is an sc name (indicating it was a lambda expression)
     -- then we can safely rename the sc to the name of the let binding.
     -- Otherwise, simply wrap the expression as a new supercombinator
     case expr' of
       (S.Term (S.Variable sc_name@('$' : _))) -> renameSC sc_name ('$' : name)
       _ -> pushSC (SC ('$' : name) free_vars expr')


--------------------------------------------------------------------------------
-- Phase One - Lambda Lifting


-- [lambda bound vars] -> expr to compile -> SC (resulting expr)
csc :: [String] -> S.Exp -> SCS S.Exp

csc _ t@(S.Term _) = pure t

csc lexvars (S.Apply e1 e2 ) = S.Apply <$> csc lexvars e1 <*> csc lexvars e2

csc lexvars (S.Let (var, val) body) =
  do val'  <- csc (var : lexvars) val  -- var should not be in binding value, so we don't push it onto the bound vars
     body' <- csc lexvars body
     pure $ S.Let (var, val') body'

csc lexvars (S.Letrec bs e)  =
  do let lexvars' = map fst bs ++ lexvars
     bs' <- mapM (\(var, val) -> (var,) <$> csc lexvars' val) bs
     e'  <- csc lexvars e
     pure $ S.Letrec bs' e'

csc lexvars (S.Lambda var body) =
  do let lexvars' = var : lexvars
     sc_name <- newSCName

     body' <- csc lexvars' body

     -- So we want the free variables within the body,
     -- but in their 'bound order' (their De Brujin index)
     --
     -- Since we have the bound vars in ascending order (lexvars)
     -- we can just filter that list for our free variables!
     let body_vars = S.freeVariables' [var] body'

         lexed_vars = sortedLexedVars lexvars' (var : body_vars)

         -- the parameters are all variables with lexical number greater than 1
         -- (i.e. all variables that are not either free, or the parameter of the lambda)
         sc_params = map snd lexed_vars

     -- add the supercombinator to the collection
     pushSC (SC sc_name sc_params body')

     pure $ S.mkApply (S.mkVariable sc_name : map (S.mkVariable . snd)
                                                  (init lexed_vars))


-- 'lexed variable' is a variable paired with its lexical ordering
-- returned lexical variables paired with their ordering,
-- sorted on said ordering
sortedLexedVars :: [String] -> [String] -> [(Int, String)]
sortedLexedVars lex_order vars =
  let lex_ord_map = Map.fromList (zip (reverse lex_order) [1..])
      varToOrd v = fromMaybe 0 (Map.lookup v lex_ord_map)
      lexed_vars = map (\v -> (varToOrd v, v)) vars
    in dropWhile ((== 0) . fst) $ sortOn fst lexed_vars


isSCVar :: String -> Bool
isSCVar [] = error "Empty variable"
isSCVar ('$':_) = True
isSCVar _ = False


--------------------------------------------------------------------------------
-- Phase Two - Eta Reduction

-- | Perform 'full' eta reductions on program
-- |
-- | 'full' means eta reduction results in an equivalence of 
-- | supercombinators
-- |
-- | e.g. $1 x y = $2 x y is fully reducible to $1 = $2
-- | in contrast to an expression like $1 x y = $2 z y
-- | which only partially reduces to $1 x = $2 z
reduceFullEta :: Prog -> Prog
reduceFullEta (Prog scs expr) =
  let (rest_scs, equiv_scs) =
        partitionEithers . map scToEitherNormalSCOrEquivSCs $ scs

      equiv_map :: Map.Map String String
      equiv_map = foldr insertEquivName Map.empty equiv_scs
   in Prog rest_scs (alphaConvertWithMap equiv_map expr)
  where
    -- to partition SCs into those that are not (left) and are (right) completely reducible
    -- where when they are reducible, we simply return the equivalent names
    scToEitherNormalSCOrEquivSCs :: SC -> Either SC (String, String)
    scToEitherNormalSCOrEquivSCs sc =
      maybe (Left sc) (Right .  (scName sc,)) (mScReduceFullEta sc)

    -- for building up the map from equivalent names 
    -- identified per SC
    insertEquivName :: (String, String) -> Map.Map String String -> Map.Map String String
    insertEquivName (name, equiv_name) equiv_map =
      Map.insert name (fromMaybe equiv_name (Map.lookup equiv_name equiv_map)) equiv_map


-- | if SC is fully reducible, return
-- | the equivalent SC name
-- | 
-- | e.g. 
-- |   $1 x y = $2 x y
-- |     => Just $2
mScReduceFullEta :: SC -> Maybe String
mScReduceFullEta sc =
  do vars <- traverse S.maybeVariable . S.unApply $ scExpr sc
     case vars of
       [] -> error $ "Supercombinator has empty expression: " ++ show sc
       (v:vs) ->
         if isSCVar v && not (any isSCVar vs)
           then Just v
           else Nothing
