{-# LANGUAGE TupleSections #-}

module Lambda.SCCompiler 
  ( Prog (..)
  , SC (..)
  , compileSCs) where

import Control.Exception (assert)
import qualified Control.Monad.State.Lazy as ST

import qualified Lambda.Syntax as S

import Debug.Trace


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
   in Prog scs expr'


csc :: [String] -> S.Exp -> SCS S.Exp
csc _ t@(S.Term _) = pure t 
csc bound_vars (S.Apply e1 e2 ) = S.Apply <$> csc bound_vars e1 <*> csc bound_vars e2
csc bound_vars (S.Let (var, val) body) = 
  do val'  <- csc bound_vars val
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
         sc_free_vars = filter (`elem` body_vars) bound_vars
        --  debug = trace (unlines ["body:    " ++ show body_vars, 
        --                          "sc args: " ++ show sc_free_vars]) sc_free_vars
         sc_vars = var : 
           assert (length body_vars == length sc_free_vars) sc_free_vars

     -- add the supercombinator to the collection
     pushSC (SC sc_name sc_vars body')

     -- return the expression that uses the supercombinator
     pure $ S.mkApply (S.mkVariable sc_name : map S.mkVariable sc_free_vars)


isSCVar :: String -> Bool
isSCVar [] = error "Empty variable"
isSCVar ('$':_) = True
isSCVar _ = False