{-# LANGUAGE OverloadedStrings #-}


module Ox.IR 
  ( Prog
  , Fn (..)
  , Stmt (..)
  , Expr (..)
  , Val (..)
  , BiArith (..)
  , add_fn_def
  , sub_fn_def
  , mul_fn_def
  , div_fn_def
  ) where 

import qualified Data.Text as T

type Prog = ([Stmt], [Fn])

data Fn = Fn 
  { fnDefName :: T.Text
  , fnImplName :: T.Text
  , fnArity :: Int
  , fnStmts :: [Stmt]
  } deriving Show

data Stmt 
  = Let T.Text Expr
  | Ret Expr
  | PushInt Integer
  | PushFn T.Text
  | MkAp
  deriving Show

data Expr 
  = Var T.Text
  | BiArith BiArith Expr Expr
  | Val Val
  deriving Show

data Val
  = VInt Integer
  | VVar T.Text
  | VThunk T.Text
  deriving Show

data BiArith
  = Add 
  | Sub
  | Mul
  | Div
  deriving Show


--------------
-- Builtins --
--------------

add_fn_def :: T.Text
add_fn_def = "FN_ADD"

sub_fn_def :: T.Text
sub_fn_def = "FN_SUB"

mul_fn_def :: T.Text
mul_fn_def = "FN_MUL"

div_fn_def :: T.Text
div_fn_def = "FN_DIV"
