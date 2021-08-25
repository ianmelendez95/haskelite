{-# LANGUAGE TupleSections #-}

module Lambda.SCCompiler where

import Control.Monad.State.Lazy as ST
import qualified Lambda.Syntax as S


data SC = SC String [String] S.Exp

data Prog = Prog [SC] S.Exp

data SCEnv = SCEnv {
  scenvSCCount :: Integer,
  scenvCombs :: [SC],
  scenvBVars :: [(Int, String)],
  scenvFVars :: [(Int, [String])]
}

type SCS = ST.State SCEnv

emptySCEnv :: SCEnv 
emptySCEnv = SCEnv { scenvSCCount = 0, scenvCombs = [], scenvBVars = [], scenvFVars = [] }

newSCName :: SCS String
newSCName = 
  do modify (\scenv@SCEnv{ scenvSCCount = c } -> scenv{ scenvSCCount = c+1 })
     cur_count <- gets scenvSCCount
     pure $ '$' : show cur_count

pushSC :: SC -> SCS ()
pushSC sc = ST.modify (\scenv@SCEnv{ scenvCombs = scs } -> 
  scenv{ scenvCombs = sc : scs })

pushBV :: String -> SCS ()
pushBV var = ST.modify modEnv
  where
    modEnv :: SCEnv -> SCEnv 
    modEnv scenv@SCEnv{ scenvBVars = bvars } = 
      scenv{ scenvBVars = modBVars bvars }
    
    modBVars :: [(Int, String)] -> [(Int, String)]
    modBVars [] = [(1, var)]
    modBVars bvars@((n, _):_) = (n + 1, var) : bvars


compileSCs :: S.Exp -> Prog
compileSCs expr = 
  let (expr', SCEnv{ scenvCombs = scs }) = ST.runState (csc expr) emptySCEnv
   in Prog scs expr'


csc :: S.Exp -> SCS S.Exp
csc t@(S.Term _) = pure t 
csc (S.Apply e1 e2 ) = S.Apply <$> csc e1 <*> csc e2
csc (S.Let (var, val) body) = 
  do val'  <- csc val
     body' <- csc body
     pure $ S.Let (var, val') body'
csc (S.Letrec bs e)  = 
  do bs' <- mapM (\(var, val) -> (var,) <$> csc val) bs
     e'  <- csc e
     pure $ S.Letrec bs' e'
csc (S.Lambda v b)   = 
  do sc_name <- newSCName

     pushBV v
     b' <- csc b

     let sc_vars = undefined

     pushSC (SC sc_name sc_vars b')

     pure $ S.mkApply (S.mkVariable sc_name : map S.mkVariable sc_vars)



