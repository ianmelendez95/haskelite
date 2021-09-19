{-# LANGUAGE TupleSections #-}

module Lambda.SCCompiler
  ( Prog (..)
  , SC (..)
  , compileExpr
  , compileSCs) where

import Data.List (foldl1', (\\), intersect, union)
import Data.Bifunctor (second)
import qualified Control.Monad.State.Lazy as ST
import qualified Lambda.Syntax as S

import Debug.Trace (trace, traceShowId)
import Trace (traceMsg)


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
               -- NComb bound_params free_params body
               | NComb [String] [String] SuperComb
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

newSCName :: SCS String
newSCName =
  do ST.modify (\scenv@SCEnv{ scenvSCCount = c } -> scenv{ scenvSCCount = c+1 })
     cur_count <- ST.gets scenvSCCount
     pure $ '$' : show cur_count


--------------------------------------------------------------------------------
-- Interface


compileExpr :: S.Exp -> Prog
compileExpr = 
  liftSuperCombs 
  . traceMsg "PARAM RES: " . snd . resolveFreeParams []
  . traceMsg "JOINED:    " . joinSuperCombs 
  . traceMsg "INIT:      " . exprToSuperComb
  . traceMsg "EXPR:      "
     
{-# DEPRECATED compileSCs "Use compileExpr" #-}
compileSCs :: S.Exp -> Prog
compileSCs = compileExpr


--------------------------------------------------------------------------------
-- Lambda -> SuperComb


exprToSuperComb :: S.Exp -> SuperComb

exprToSuperComb (S.Let bind body) = 
  Let (exprToSuperComb <$> bind) (exprToSuperComb body)

exprToSuperComb (S.Letrec binds body) = 
  let binds' = map (fmap exprToSuperComb) binds
      body'  = exprToSuperComb body
   in Letrec binds' body'

exprToSuperComb (S.Term t) = Term t

exprToSuperComb (S.Apply e1 e2) = 
  App (exprToSuperComb e1) (exprToSuperComb e2)

exprToSuperComb (S.Lambda var body) = NComb [var] [] (exprToSuperComb body)


-- | free parameter context,
-- | FPCtx = (free_vars, value)
-- |
-- | where 'free_vars' are the free variables in the 
-- | current context *that are bound by an enclosing combinator*
-- |
-- | i.e. *not all free variables are included*, only ones that are 
-- |      bound in an enclosing context
type FPCtx a = ([String], a)


-- | resolves the free parameters in the expression,
-- | adding them to relevant combinator's parameter lists
-- |
-- | NOTE: a free parameter is a free variable that is also
-- |       bound in the outer context
-- |
-- | bound_vars -> naive_comb -> resolved_comb
resolveFreeParams :: [String] -> SuperComb -> FPCtx SuperComb

resolveFreeParams bound (Let (var, val) body) = 
  Let <$> ((var,) <$> resolve val) <*> resolve body
  where 
    resolve = resolveFreeParams (var : bound)

resolveFreeParams bound (Letrec binds body) = 
  let resolved_binds :: FPCtx [(String, SuperComb)]
      resolved_binds = foldMap resolveBind binds

   in Letrec <$> resolved_binds <*> resolve body
  where 
    resolve :: SuperComb -> FPCtx SuperComb
    resolve = resolveFreeParams (map fst binds ++ bound)

    -- resolves the binding, returned as a singleton
    -- within the FPCtx so that the result can be folded 
    -- with other resolved binding results
    resolveBind :: (String, SuperComb) -> FPCtx [(String, SuperComb)]
    resolveBind (var, val) = (:[]) . (var,) <$> resolve val

resolveFreeParams bound t@(Term (S.Variable v)) = 
  if v `elem` bound then ([v], t) else ([], t)

resolveFreeParams _ t@(Term _) = pure t

resolveFreeParams bound (App sc1 sc2) = 
  App <$> resolveFreeParams bound sc1 <*> resolveFreeParams bound sc2

resolveFreeParams bound (NComb bound_ps free_ps body) = 
  -- this is where the FPCtx is interesting, 
  -- since the free params in the body become params
  -- of the combinator
  let (body_free_vars, body') = resolveFreeParams (bound_ps `union` bound) body

      -- the free vars for the enclosing contexts,
      -- and thus the free vars of this supercombinator
      sc_free_vars = body_free_vars \\ bound_ps
   in (sc_free_vars, NComb bound_ps (free_ps `union` sc_free_vars) body')


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
liftSuperCombs (NComb bound_ps free_ps body) = 
  let (Prog b_scs body') = liftSuperCombs body
      new_sc = SC {
        scName = "TODO GEN NAME",
        scParams = bound_ps ++ free_ps,
        scBody = body'
      }
   in Prog (new_sc : b_scs) (S.mkApply . map S.mkVariable $ "TODO GEN NAME" : free_ps)


liftBindings :: [(String, SuperComb)] -> ([SC], [(String, S.Exp)])
liftBindings = mconcat . map liftBinding
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


-- | Identifies directly nested supercombinators,
-- | and attempts to join them
-- |
-- | e.g.
-- |   ($ y = ($ x = y))
-- |   => ($ y x = y)
-- | 
-- | This has the consequence of addressing eventual full
-- | eta reductions as described in 
-- | section 13.3.1 "Eliminating Redundant Parameters"
joinSuperCombs :: SuperComb -> SuperComb
joinSuperCombs (Let (var, val) body) = Let (var, joinSuperCombs val) body
joinSuperCombs (Letrec binds body) = Letrec (map (joinSuperCombs <$>) binds) body
joinSuperCombs t@(Term _) = t
joinSuperCombs (App sc1 sc2) = App (joinSuperCombs sc1) (joinSuperCombs sc2)
joinSuperCombs (NComb out_bound_ps out_free_ps in_sc) = 
  case joinSuperCombs in_sc of 
    in_sc'@(NComb in_bound_ps in_free_ps in_body) -> 
      if null (out_bound_ps `intersect` in_bound_ps) -- don't have shared parameters
        then let bound_ps = out_bound_ps ++ in_bound_ps
                 free_ps = (out_free_ps `union` in_free_ps) \\ bound_ps
              in NComb bound_ps free_ps in_body
        else NComb out_bound_ps out_free_ps in_sc'
    in_sc' -> NComb out_bound_ps out_free_ps in_sc'
   


