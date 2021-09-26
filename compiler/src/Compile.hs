module Compile 
  ( lexprToSCProg
  ) where 

import qualified Lambda.Syntax as S
import qualified Lambda.SCCompiler as SC

lexprToSCProg :: S.Exp -> SC.Prog
lexprToSCProg = SC.compileExpr