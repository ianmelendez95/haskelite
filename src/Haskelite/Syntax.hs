module Haskelite.Syntax (Decl, Expr (..), IOp (..)) where 

import qualified Data.Text as T

type Decl = (T.Text, Expr)

data Expr
  = Let T.Text Expr Expr
  | Var T.Text
  | IExpr Expr IOp Expr
  | LInt Integer 
  deriving Show

data IOp 
  = Plus 
  | Minus
  | Mult
  | Div
  deriving Show
