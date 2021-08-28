-- | Module encapsulating alpha conversions for syntaxes
module Lambda.AlphaConv 
  ( AlphaConv (..)
  , unsafeAlphaConvert
  , alphaConvertWithMap
  ) where 
  
import Data.Bifunctor (second)
import qualified Data.Map.Lazy as Map

import qualified Lambda.Syntax as S
import qualified Lambda.Enriched as E

class AlphaConv a where 
  unsafeAlphaConv :: String -> String -> a -> a

instance AlphaConv S.Exp where 
  unsafeAlphaConv = unsafeAlphaConvert

instance AlphaConv E.Exp where 
  unsafeAlphaConv = enrConv

enrConv :: String -> String -> E.Exp -> E.Exp 
enrConv old new = go
  where 
    go :: E.Exp -> E.Exp
    go (E.Pure e) = E.Pure (unsafeAlphaConv old new e)
    go (E.Let b e) = E.Let (goBinding b) (go e)
    go (E.Letrec bs e) = E.Letrec (map goBinding bs) (go e)
    go (E.Apply e1 e2) = E.Apply (go e1) (go e2)
    go (E.Lambda p e) = E.Lambda (goPatt p) (go e)
    go (E.FatBar e1 e2) = E.FatBar (go e1) (go e2)
    go (E.Case v cs) = E.Case v (map (E.mapCaseExpr go) cs)

    goVar :: S.Variable -> S.Variable
    goVar v = if v == old then new else v

    goBinding :: E.LetBinding -> E.LetBinding
    goBinding (patt, expr) = (goPatt patt, go expr)

    goPatt :: E.Pattern -> E.Pattern
    goPatt c@(E.PConstant _) = c
    goPatt (E.PVariable v) = E.PVariable $ goVar v
    goPatt (E.PConstructor c patts) = E.PConstructor c $ map goPatt patts


-- | rather unsafely converts instances of the old name with the new name
-- | only scrutinizing on formal parameters (that is, if encounter a lambda with the formal parameter 
-- | matching the old name, it doesn't continue)
-- | 
-- | unsafe in that it makes no discernment for whether it is replacing the name with 
-- | a variable that is also free in the expression (i.e. name capture is possible)
-- | (NOTE: this is not a problem in a type checked expression)
unsafeAlphaConvert :: String -> String -> S.Exp -> S.Exp
unsafeAlphaConvert _ _ l@(S.Let _ _) = error $ "Can't alpha convert let: " ++ show l
unsafeAlphaConvert _ _ l@(S.Letrec _ _) = error $ "Can't alpha convert letrec: " ++ show l
unsafeAlphaConvert old_name new_name (S.Term (S.Variable var)) = 
  S.mkVariable $ S.mapVarName (\n -> if n == old_name then new_name else n) var
unsafeAlphaConvert _ _ t@(S.Term _) = t
unsafeAlphaConvert old_name new_name (S.Apply e1 e2) = 
  S.Apply (unsafeAlphaConvert old_name new_name e1)
          (unsafeAlphaConvert old_name new_name e2)
unsafeAlphaConvert old_name new_name l@(S.Lambda v e)
  | old_name == v = l -- name is already bound, doesn't matter
  | otherwise = S.Lambda v (unsafeAlphaConvert old_name new_name e)


-- | NOTE: will only perform a single lookup in the map
-- | e.g. with the following name mapping
-- |   
-- |   a -> b
-- |   b -> c
-- |
-- | A variable 'a' will be converted to 'b'
-- | even though the real value of 'a' should actually be 'c'
-- | since a -> b and b -> c thus a -> c.
-- | Again, since only the first lookup value is used, 
-- | 'a' is converted to 'b'
-- |
-- | As such it is left up to the caller to resolve any
-- | transitive equivalences in their mapping.
alphaConvertWithMap :: Map.Map String String -> S.Exp -> S.Exp

alphaConvertWithMap vmap (S.Let (bname, bval) body) = 
  S.Let (bname, alphaConvertWithMap vmap bval) 
        (alphaConvertWithMap vmap body)

alphaConvertWithMap vmap (S.Letrec bs body) = 
  S.Letrec (map (second (alphaConvertWithMap vmap)) bs) 
           (alphaConvertWithMap vmap body)

alphaConvertWithMap vmap e@(S.Term (S.Variable var)) = 
  maybe e S.mkVariable (Map.lookup var vmap)

alphaConvertWithMap _ t@(S.Term _) = t

alphaConvertWithMap vmap (S.Apply e1 e2) = 
  S.Apply (alphaConvertWithMap vmap e1)
          (alphaConvertWithMap vmap e2)

alphaConvertWithMap vmap (S.Lambda v e) =
  S.Lambda v (alphaConvertWithMap (Map.delete v vmap) e)
