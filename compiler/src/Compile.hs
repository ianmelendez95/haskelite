module Compile 
  ( lexprToSCProg
  , mirandaToGCode
  ) where 

import qualified Lambda.Syntax as S
import qualified Miranda.Syntax as M
import qualified Lambda.SCCompiler as SC
import Lambda.ToLambda (ToLambda (..))

import qualified CodeGen.GCode as GC

lexprToSCProg :: S.Exp -> SC.Prog
lexprToSCProg = SC.compileExpr

mirandaToGCode :: M.Prog -> [GC.GInstr]
mirandaToGCode = GC.compileSCProg . SC.compileExpr . toLambda

