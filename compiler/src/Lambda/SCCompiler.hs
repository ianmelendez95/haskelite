{-# LANGUAGE TupleSections #-}

module Lambda.SCCompiler 
  ( Prog (..)
  , SC (..)
  , compileSCs) where

import Control.Exception (assert)
import Data.Maybe (fromMaybe)
import Data.Either (partitionEithers)
import Data.List (partition)
import qualified Control.Monad.State.Lazy as ST
import qualified Data.Map.Lazy as Map

import Debug.Trace

import qualified Lambda.Syntax as S
import Lambda.AlphaConv (alphaConvertWithMap)

data Prog = Prog [SC] S.Exp

data SC = SC String [String] S.Exp

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

scName :: SC -> String
scName (SC n _ _) = n

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


compileSCs :: S.Exp -> Prog
compileSCs expr = 
  let (expr', SCEnv{ scenvCombs = scs }) = 
        ST.runState (csc [] expr) emptySCEnv
   in reduceFullEta $ Prog scs expr'

  
--------------------------------------------------------------------------------
-- Phase One - Lambda Lifting


csc :: [String] -> S.Exp -> SCS S.Exp
csc _ t@(S.Term _) = pure t 
csc bound_vars (S.Apply e1 e2 ) = S.Apply <$> csc bound_vars e1 <*> csc bound_vars e2
csc bound_vars (S.Let (var, val) body) = 
  do val'  <- csc bound_vars val  -- var should not be in binding value, so we don't push it onto the bound vars
     body' <- csc (var : bound_vars) body
     pure $ S.Let (var, val') body'
csc bound_vars (S.Letrec bs e)  = 
  do let bound_vars' = map fst bs ++ bound_vars
     bs' <- mapM (\(var, val) -> (var,) <$> csc bound_vars' val) bs
     e'  <- csc bound_vars' e
     pure $ S.Letrec bs' e'
csc bound_vars (S.Lambda var body)   = 
  do sc_name <- newSCName

     body' <- csc (var : bound_vars) body

     -- So we want the free variables within the body,
     -- but in their 'bound order' (their De Brujin index)
     --
     -- Since we have the bound vars in ascending order (bound_vars)
     -- we can just filter that list for our free variables!
     let body_vars = filter (not . isSCVar) $ S.freeVariables' [var] body'
         (sc_bound_params, sc_free_params) = partition (`elem` bound_vars) body_vars  
        --  debug = trace (unlines ["body:    " ++ show body_vars, 
        --                          "sc args: " ++ show sc_free_vars]) sc_free_vars
         sc_params = sc_free_params ++ [var] ++ sc_bound_params

     -- add the supercombinator to the collection
     pushSC (SC sc_name sc_params body')

     -- return the expression that uses the supercombinator
     -- which is just the supercombinator applied to the params that are free in the context
     pure $ S.mkApply (S.mkVariable sc_name : map S.mkVariable sc_free_params)


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
