{-# LANGUAGE TupleSections #-}

module Lambda.SCCompiler 
  ( Prog (..)
  , SC (..)
  , compileSCs) where

import Data.Maybe (fromMaybe)
import Data.Either (partitionEithers)
import Data.List (sortOn, deleteBy)
import Data.Bifunctor (second)
import Control.Monad (foldM)
import qualified Control.Monad.State.Lazy as ST

import qualified Data.Map.Lazy as Map

import Control.Exception (assert)
import Debug.Trace

import qualified Lambda.Syntax as S
import Lambda.AlphaConv (alphaConvertWithMap)

data Prog = Prog [SC] S.Exp

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



compileSCs :: S.Exp -> Prog

compileSCs (S.Letrec bs e) = 
  -- top-level letrecs treated specially, effectively already 
  -- known to be supercombinators, and are lexical level 0 (totally free)

  let -- compile the supercombinators from the bindings
      SCEnv{scenvCombs = scs} = ST.execState (mapM_ (uncurry compileTLBind) bs) emptySCEnv

      -- rename free instances of binding names with supercombinator names
      v_names_to_sc_names = Map.fromList (map (\(n, _) -> (n, '$' : n)) bs)
      renamed_scs = map (\sc@SC{ scBody = body } -> 
                          sc{ scBody = alphaConvertWithMap v_names_to_sc_names body }) scs
      renamed_e = alphaConvertWithMap v_names_to_sc_names e

      -- compile the renamed body (which is the 'program' to be run)
      (Prog prog_scs prog_expr) = compileSCs renamed_e

   in reduceFullEta $ Prog (prog_scs ++ renamed_scs) prog_expr

compileSCs expr = 
  let (expr', SCEnv{ scenvCombs = scs }) = 
        ST.runState (csc [] expr) emptySCEnv
   in reduceFullEta $ Prog scs expr'


-- compile top level binding
compileTLBind :: String -> S.Exp -> SCS ()
compileTLBind name expr = 
  do expr' <- csc [] expr
     let free_vars = filter (not . isSCVar) $ S.freeVariables' [] expr'

     -- If resulting expression is an sc name (indicating it was a lambda expression)
     -- then we can safely rename the sc to the name of the let binding.
     -- Otherwise, simply wrap the expression as a new supercombinator
     case expr' of 
       (S.Term (S.Variable sc_name@('$' : _))) -> 
         do (SC _ sc_args sc_expr) <- popSCByName sc_name
            pushSC (SC ('$' : name) sc_args sc_expr)
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
