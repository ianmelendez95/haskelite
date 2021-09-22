{-# LANGUAGE TupleSections #-}

module Lambda.SCCompiler
  ( Prog (..)
  , SC (..)
  , compileExpr
  ) where

import Data.List (intersect, union)
import Data.Maybe (fromMaybe)
import qualified Control.Monad.State.Lazy as ST
import qualified Lambda.Syntax as S

-- import Trace (traceMsg)
traceMsg :: String -> a -> a
traceMsg _ x = x
-- traceMsg :: String -> a -> a
-- traceMsg _ x = x


--------------------------------------------------------------------------------
-- Final Program Structure


data Prog = Prog [SC] S.Exp

instance Show Prog where
  show (Prog scs expr) = unlines (map show scs ++ [replicate 80 '-', show expr])


data SC = SC {
  scName :: String,
  scParams :: [String],
  scBody :: S.Exp
}

instance Show SC where
  show (SC name args expr) = unwords (name : args) ++ " = " ++ show expr


--------------------------------------------------------------------------------
-- Supercombinator Expression


data SuperComb = Let (String, SuperComb) SuperComb
               | Letrec [(String, SuperComb)] SuperComb
               | Term S.Term
               | App SuperComb SuperComb

               -- 'n'ary combinator, one that takes arguments
               -- NComb bound_params free_params body
               | NComb [String] [String] SuperComb
               deriving Show


--------------------------------------------------------------------------------
-- Supercombinator State


type SCS = ST.State SCEnv

scsToProg :: SCS S.Exp -> Prog
scsToProg scstate = 
  let (body, SCEnv{ scenvCombs = scs }) = ST.runState scstate emptySCEnv
   in Prog scs body

newSCName :: SCS String
newSCName = ('$':) . show <$> new_count
  where 
    new_count = 
      do ST.modify (\scenv@SCEnv{ scenvSCCount = c } -> scenv{ scenvSCCount = c+1 })
         ST.gets scenvSCCount

pushSC :: SC -> SCS ()
pushSC sc = ST.modify (scenvPushSC sc)


data SCEnv = SCEnv {
  scenvSCCount :: Integer,
  scenvCombs :: [SC]
}

scenvPushSC :: SC -> SCEnv -> SCEnv
scenvPushSC sc scenv@SCEnv{ scenvCombs = scs } = scenv{ scenvCombs = sc : scs }

emptySCEnv :: SCEnv
emptySCEnv = SCEnv { scenvSCCount = 0, scenvCombs = [] }


--------------------------------------------------------------------------------
-- Interface


compileExpr :: S.Exp -> Prog
compileExpr = 
  liftSuperCombs 
  . traceMsg "PARAM RES: " . snd . resolveFreeParams []
  . traceMsg "JOINED:    " . joinSuperCombs 
  . traceMsg "INIT:      " . exprToSuperComb
  . traceMsg "EXPR:      "
     

--------------------------------------------------------------------------------
-- Lambda -> Raw SuperComb


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


--------------------------------------------------------------------------------
-- Join Supercombinators


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
  let in_sc' = joinSuperCombs in_sc
      m_joined = 
        case in_sc' of 
          (NComb in_bound_ps in_free_ps in_body) -> 
            if null (out_bound_ps `intersect` in_bound_ps) -- don't have shared parameters
              then let bound_ps = out_bound_ps ++ in_bound_ps
                       free_ps = (out_free_ps `union` in_free_ps) \\ bound_ps
                    in Just $ NComb bound_ps free_ps in_body
              else Nothing
          _ -> Nothing
   in fromMaybe (NComb out_bound_ps out_free_ps in_sc') m_joined


--------------------------------------------------------------------------------
-- Resolve Free Parameters


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
-- Lift the Supercombinators out of the lambda expression,
-- forming the complete program
-- SuperComb -> Lambda


-- | Bind type used as a visual aid
-- | for treating it as a foldable, applicative container
type Bind a = (String, a)


liftSuperCombs :: SuperComb -> Prog
liftSuperCombs comb = scsToProg (liftSuperCombsM comb)
  
liftSuperCombsM :: SuperComb -> SCS S.Exp
liftSuperCombsM (Let bind body) = 
  S.Let <$> mapM liftSuperCombsM bind
        <*> liftSuperCombsM body

liftSuperCombsM (Letrec binds body) = 
  S.Letrec <$> liftBindings binds
           <*> liftSuperCombsM body
  where
    liftBindings :: [Bind SuperComb] -> SCS [Bind S.Exp]
    liftBindings = traverse liftBinding

    liftBinding :: Bind SuperComb -> SCS (Bind S.Exp)
    liftBinding bind = mapM liftSuperCombsM bind
  
-- TODO: consider renaming free vars that match named supercombs
liftSuperCombsM (Term t) = pure $ S.Term t
liftSuperCombsM (App sc1 sc2) = 
  S.Apply <$> liftSuperCombsM sc1 <*> liftSuperCombsM sc2
liftSuperCombsM (NComb bound_ps free_ps body) = 
  do sc_name <- newSCName
     body'   <- liftSuperCombsM body
     pushSC $ SC{
       scName = sc_name,
       scParams = free_ps ++ bound_ps,
       scBody = body'
     }
     pure $ S.mkApply (map S.mkVariable (sc_name : free_ps))
   

--------------------------------------------------------------------------------
-- Common Util


-- | override default \\ implementation, as it 
-- | only removes the first matching occurrence
(\\) :: Eq a => [a] -> [a] -> [a]
(\\) = foldl deleteAll
  where
    deleteAll :: Eq a => [a] -> a -> [a]
    deleteAll [] _ = []
    deleteAll (y':ys') x' = if x' == y' then deleteAll ys' x' else y' : deleteAll ys' x'
