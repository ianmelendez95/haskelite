module CodeGen.StackCode (compileLambda) where

import qualified Lambda.Syntax as S


data SC = SC_INT Int
        | SC_CHR Char
        | SC_BOO Bool
        | SC_FUN String
        | SC_VAR String
        | SC_APP
        | SC_LAM String
        | SC_LET String
        | SC_LRE [String]


instance Show SC where
  show (SC_INT i)  = "INT " ++ show i
  show (SC_CHR c)  = "CHR " ++ [c]
  show (SC_BOO b)  = "BOO " ++ show b
  show (SC_FUN f)  = "FUN " ++ f
  show (SC_VAR v)  = "VAR " ++ v
  show SC_APP      = "APP"
  show (SC_LAM v)  = "LAM " ++ v
  show (SC_LET v)  = "LET " ++ v
  show (SC_LRE vs) = "LRE " ++ unwords vs


compileLambda :: S.Exp -> [SC]
compileLambda = reverse . cExpression

cExpression :: S.Exp -> [SC]
cExpression (S.Term (S.Constant c)) = [cConstant c]
cExpression (S.Term (S.Function f)) = [cFunction f]
cExpression (S.Term (S.Variable v)) = [SC_VAR v]
cExpression (S.Apply e1 e2)         = SC_APP : cExpression e1 ++ cExpression e2
cExpression (S.Lambda var body)     = SC_LAM var : cExpression body
cExpression (S.Let (var, val) body) = SC_LET var : cExpression val ++ cExpression body
cExpression (S.Letrec binds body) =
  let b_vars = map fst binds
      b_vals = map snd binds
   in SC_LRE b_vars : concatMap cExpression b_vals ++ cExpression body

cFunction :: S.Function -> SC
cFunction S.FPlus = SC_FUN "+"
cFunction S.FMinus = SC_FUN "-"
cFunction S.FMult = SC_FUN "*"
cFunction S.FDiv = SC_FUN "/"
cFunction S.FIf = SC_FUN "IF"
cFunction S.FEq = SC_FUN "="
cFunction f = error $ "Unsupported function: " ++ show f

cConstant :: S.Constant -> SC
cConstant (S.CNat n) = SC_INT n
cConstant (S.CChar c) = SC_CHR c
cConstant (S.CBool b) = SC_BOO b
cConstant c = error $ "Unsupported constant: " ++ show c

