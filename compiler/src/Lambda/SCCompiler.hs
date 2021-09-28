{-# LANGUAGE TupleSections #-}

module Lambda.SCCompiler
  ( Prog (..)

  , SC (..)
  , scArity

  , compileExpr
  ) where

import Data.List (intersect, union)
import Data.Maybe (fromMaybe)
import Data.Bifunctor (first)
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

mkSCName :: String -> String
mkSCName = ('$' :)

scArity :: SC -> Int
scArity = length . scParams


--------------------------------------------------------------------------------
-- Initial Program Structure


-- SC program, which explicitly represents top level bindings
data SCProg = SCProg [Bind SuperComb] SuperComb

mapSCProg :: (SuperComb -> SuperComb) -> SCProg -> SCProg
mapSCProg f (SCProg binds body) = SCProg (map (f <$>) binds) (f body)

mapSCProgM :: Monad m => (SuperComb -> m SuperComb) -> SCProg -> m SCProg
mapSCProgM m_f (SCProg binds body) = 
  SCProg <$> traverse (traverse m_f) binds
         <*> m_f body


--------------------------------------------------------------------------------
-- Supercombinator Expression


data SuperComb = Let (String, SuperComb) SuperComb
               | Letrec [(String, SuperComb)] SuperComb
               | Term S.Term
               | App SuperComb SuperComb

               -- 'n'ary combinator, one that takes arguments
               -- NComb bound_params free_params body
               | NComb String [String] [String] SuperComb
               deriving Show


--------------------------------------------------------------------------------
-- Supercombinator Naming Monad


type NameS = ST.State Int

newName :: NameS String
newName = ('$':) . show <$> (ST.modify (+1) >> ST.get)


--------------------------------------------------------------------------------
-- Supercombinator Lifting/Collection


type SCS = ST.State [SC]

scsToProg :: SCS S.Exp -> Prog
scsToProg scstate = 
  let (body, scs) = ST.runState scstate []
   in Prog scs body

pushSC :: SC -> SCS ()
pushSC sc = ST.modify (sc :)


--------------------------------------------------------------------------------
-- Interface


compileExpr :: S.Exp -> Prog
compileExpr = 
  liftSuperCombs 
  . traceMsg "PARAM RES: " . resolveFreeParams
  . traceMsg "NAMED:     " . nameCombs
  . traceMsg "JOINED:    " . mapSCProg joinSuperCombs 
  . traceMsg "TL NAMES:  " . nameTLBinds
  . traceMsg "INIT:      " . exprToSCProg
  . traceMsg "EXPR:      "


--------------------------------------------------------------------------------
-- Lambda -> SCProg


exprToSCProg :: S.Exp -> SCProg
exprToSCProg (S.Let bind body) = _letToSCProg [bind] body
exprToSCProg (S.Letrec binds body) = _letToSCProg binds body
exprToSCProg expr = SCProg [] (exprToSuperComb expr)
        
_letToSCProg :: [(String, S.Exp)] -> S.Exp -> SCProg
_letToSCProg binds body = 
  let (SCProg prog_binds prog_body) = exprToSCProg body
    in SCProg (map (exprToSuperComb <$>) binds ++ prog_binds) prog_body

     

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

exprToSuperComb (S.Lambda var body) = NComb "__NONE__" [var] [] (exprToSuperComb body)


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
joinSuperCombs (NComb out_name out_bound_ps out_free_ps in_sc) = 
  let in_sc' = joinSuperCombs in_sc
      m_joined = 
        case in_sc' of 
          (NComb _ in_bound_ps in_free_ps in_body) -> 
            if null (out_bound_ps `intersect` in_bound_ps) -- don't have shared parameters
              then let bound_ps = out_bound_ps ++ in_bound_ps
                       free_ps = (out_free_ps `union` in_free_ps) \\ bound_ps
                    in Just $ NComb out_name bound_ps free_ps in_body
              else Nothing
          _ -> Nothing
   in fromMaybe (NComb out_name out_bound_ps out_free_ps in_sc') m_joined


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


resolveFreeParams :: SCProg -> SCProg
resolveFreeParams = snd . mapSCProgM (resolveFreeParamsM [])


-- | resolves the free parameters in the expression,
-- | adding them to relevant combinator's parameter lists
-- |
-- | NOTE: a free parameter is a free variable that is also
-- |       bound in the outer context
-- |
-- | bound_vars -> naive_comb -> resolved_comb
resolveFreeParamsM :: [String] -> SuperComb -> FPCtx SuperComb

resolveFreeParamsM bound (Let (var, val) body) = 
  first (`deleteAll` var) $ Let <$> ((var,) <$> resolve val) <*> resolve body
  where 
    resolve = resolveFreeParamsM (var : bound)

resolveFreeParamsM bound (Letrec binds body) = 
  let resolved_binds :: FPCtx [(String, SuperComb)]
      resolved_binds = foldMap resolveBind binds

   in first (\\ map fst binds) $ Letrec <$> resolved_binds <*> resolve body
  where 
    resolve :: SuperComb -> FPCtx SuperComb
    resolve = resolveFreeParamsM (map fst binds ++ bound)

    -- resolves the binding, returned as a singleton
    -- within the FPCtx so that the result can be folded 
    -- with other resolved binding results
    resolveBind :: (String, SuperComb) -> FPCtx [(String, SuperComb)]
    resolveBind (var, val) = (:[]) . (var,) <$> resolve val

resolveFreeParamsM bound t@(Term (S.Variable v)) = 
  if v `elem` bound then ([v], t) else ([], t)

resolveFreeParamsM _ t@(Term _) = pure t

resolveFreeParamsM bound (App sc1 sc2) = 
  App <$> resolveFreeParamsM bound sc1 <*> resolveFreeParamsM bound sc2

resolveFreeParamsM bound (NComb name bound_ps free_ps body) = 
  -- this is where the FPCtx is interesting, 
  -- since the free params in the body become params
  -- of the combinator
  let (body_free_vars, body') = resolveFreeParamsM (bound_ps `union` bound) body

      -- the free vars for the enclosing contexts,
      -- and thus the free vars of this supercombinator
      sc_free_vars = body_free_vars \\ bound_ps
   in (sc_free_vars, NComb name bound_ps (free_ps `union` sc_free_vars) body')


--------------------------------------------------------------------------------
-- Handle top level binding names 


nameTLBinds :: SCProg -> SCProg 
nameTLBinds (SCProg tl_binds body) = 
  SCProg (map (nameBinds <$>) tl_binds) (nameBinds body)
  where 
    nameBinds = nameTLBinds' (map fst tl_binds)


nameTLBinds' :: [String] -> SuperComb -> SuperComb

nameTLBinds' tl_ns (Let (var, val) body) = 
  Let (var, nameBinds val) (nameBinds body)
  where
    nameBinds = nameTLBinds' tl_ns

nameTLBinds' tl_ns (Letrec binds body) = 
  Letrec (map (nameBinds <$>) binds) (nameBinds body)
  where
    nameBinds = nameTLBinds' tl_ns

nameTLBinds' tl_ns t@(Term (S.Variable v)) = 
  if v `elem` tl_ns then Term (S.Variable ('$' : v)) 
                    else t

nameTLBinds' _ t@(Term _) = t

nameTLBinds' tl_ns (App sc1 sc2) = 
  App (nameBinds sc1) (nameBinds sc2)
  where
    nameBinds = nameTLBinds' tl_ns

nameTLBinds' tl_ns (NComb n bps fps b) = 
  NComb n bps fps (nameTLBinds' tl_ns' b)
  where
    tl_ns' = tl_ns \\ bps
  

--------------------------------------------------------------------------------
-- Give Supercombinators unique names


nameCombs :: SCProg -> SCProg
nameCombs sc = 
  ST.evalState (mapSCProgM nameCombsM sc) 0

nameCombsM :: SuperComb -> NameS SuperComb

nameCombsM (Let bind body) = 
  Let <$> nameBindingM bind <*> nameCombsM body

nameCombsM (Letrec binds body) = 
  Letrec <$> mapM nameBindingM binds <*> nameCombsM body

nameCombsM t@(Term _) = pure t
nameCombsM (App sc1 sc2) = App <$> nameCombsM sc1 <*> nameCombsM sc2
nameCombsM (NComb _ bps fps b) = 
  NComb <$> newName 
        <*> pure bps 
        <*> pure fps 
        <*> nameCombsM b

nameBindingM :: (String, SuperComb) -> NameS (String, SuperComb)
nameBindingM (var, NComb _ sc_bps sc_fps sc_body) =
  do sc_name  <- (++ var) <$> newName
     sc_body' <- nameCombsM sc_body
     pure (var, NComb sc_name sc_bps sc_fps sc_body')
nameBindingM (var, val) = (var,) <$> nameCombsM val 


--------------------------------------------------------------------------------
-- Lift the Supercombinators out of the lambda expression,
-- forming the complete program
-- SuperComb -> Lambda


-- | Bind type used as a visual aid
-- | for treating it as a foldable, applicative container
type Bind a = (String, a)


liftSuperCombs :: SCProg -> Prog
liftSuperCombs (SCProg tl_binds body) = 
  scsToProg $ mapM_ liftTLBinding tl_binds >> liftSuperCombsM body

liftTLBinding :: Bind SuperComb -> SCS ()
liftTLBinding b@(var, NComb _ bps fps sc_body) = 
  do sc_body' <- liftSuperCombsM sc_body
     if not (null fps) 
       then error $ "Top level binding nary combinator has free paramters: " ++ show b
       else pushSC $ SC {
               scName = mkSCName var,
               scParams = bps,
               scBody = sc_body'
             }

liftTLBinding (var, val) = 
  do val' <- liftSuperCombsM val
     pushSC $ SC {
       scName = mkSCName var,
       scParams = [],
       scBody = val'
     } 

  
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
liftSuperCombsM (NComb name bound_ps free_ps body) = 
  do body'   <- liftSuperCombsM body
     pushSC $ SC {
       scName = name,
       scParams = free_ps ++ bound_ps,
       scBody = body'
     }
     pure $ S.mkApply (map S.mkVariable (name : free_ps))
   

--------------------------------------------------------------------------------
-- Common Util


-- | override default \\ implementation, as it 
-- | only removes the first matching occurrence
(\\) :: Eq a => [a] -> [a] -> [a]
(\\) = foldl deleteAll

deleteAll :: Eq a => [a] -> a -> [a]
deleteAll [] _ = []
deleteAll (y':ys') x' = if x' == y' then deleteAll ys' x' else y' : deleteAll ys' x'
