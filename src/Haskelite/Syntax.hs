module Haskelite.Syntax (Expr (..), IOp (..)) where 

data Expr
  = IExpr Expr IOp Expr
  | LInt Integer 
  deriving Show

data IOp = Plus deriving Show
