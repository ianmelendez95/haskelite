module Ox.IR 
  ( Prog
  , Fn (..)
  , Stmt (..)
  , Expr (..)
  , Val (..)
  , BiArith (..)
  ) where 

import qualified Data.Text as T

type Prog = [Fn]

data Fn = Fn T.Text [Stmt] deriving Show

data Stmt 
  = Let T.Text Expr
  | Ret Expr
  deriving Show

data Expr 
  = Var T.Text
  | BiArith BiArith Expr Expr
  | Val Val
  deriving Show

data Val
  = VInt Integer
  | VThunk T.Text
  deriving Show

data BiArith
  = Add 
  | Sub
  | Mul
  | Div
  deriving Show
