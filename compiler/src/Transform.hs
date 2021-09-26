module Transform 
  ( progToLambda
  ) where 

import qualified Miranda.Syntax as M
import qualified Lambda.Syntax as S
import Lambda.ToLambda (ToLambda (..))

progToLambda :: M.Prog -> S.Exp
progToLambda = toLambda
