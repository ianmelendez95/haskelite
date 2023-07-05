{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Compile 
  ( compile
  ) where

import Control.Lens hiding (op)
import TextShow hiding (singleton)
import qualified Data.Text as T
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)

import Haskelite.Parse
import qualified Haskelite.Syntax as H
import qualified Ox.IR as Ox

import Text.Parsec (ParseError)


type C = State CS

data CS = CS 
  { _curStmts :: [Ox.Stmt] 
  , _funcs :: Map T.Text Ox.Fn
  , _nameIdx :: Int
  } deriving Show

makeLenses ''CS


compile :: T.Text -> Either ParseError T.Text
compile input = oxProgToRust . compileHaskeliteToOx <$> parseHaskelite input

-- Compiler Monad


emptyCS :: CS
emptyCS = CS 
  { _curStmts = []
  , _funcs = Map.empty 
  , _nameIdx = 0
  }


newNameIdx :: C Int
newNameIdx = do 
  idx <- use nameIdx 
  nameIdx %= (+1)
  pure idx


pushStmt :: Ox.Stmt -> C ()
pushStmt stmt = curStmts %= (stmt:)


-- Ox Compilation


compileHaskeliteToOx :: H.Expr -> Ox.Prog
compileHaskeliteToOx expr =
  let c_res = execState (compileExprToFn "prog" expr) emptyCS
   in Map.elems $ c_res ^. funcs


compileExprToVal :: H.Expr -> C Ox.Val
compileExprToVal expr = do 
  name_idx <- newNameIdx
  let fname = "e_" <> showt name_idx
  (Ox.Fn fname' _) <- compileExprToFn fname expr
  pure $ Ox.VThunk fname'


compileExprToFn :: T.Text -> H.Expr -> C Ox.Fn
compileExprToFn fname expr = do 
  ox_expr <- compileExpr expr

  -- get the initializing statements so far (and then clear them)
  ox_prestmts <- use curStmts <* (curStmts .= [])

  let ox_fn = Ox.Fn fname (ox_prestmts <> [Ox.Ret ox_expr])
  funcs %= Map.insert fname ox_fn
  pure ox_fn


compileExpr :: H.Expr -> C Ox.Expr
compileExpr (H.LInt x) = pure . Ox.Val $ Ox.VInt x
compileExpr (H.Var v) = pure . Ox.Var $ v
compileExpr (H.IExpr el op er) = do
  ox_el <- Ox.Val <$> compileExprToVal el
  ox_er <- Ox.Val <$> compileExprToVal er
  pure $ Ox.BiArith (opToOxArith op) ox_el ox_er

 --  pure $ rustBinIFun op <> "(" <> compileExpr el <> ", " <> compileExpr er <> ")"
compileExpr (H.Let var val body) = do 
  ox_val <- compileExpr val
  ox_body <- compileExpr body
  pushStmt $ Ox.Let var ox_val
  pure ox_body


-- Rust Gen


oxProgToRust :: Ox.Prog -> T.Text
oxProgToRust prog = 
  let fns :: T.Text
      fns = T.intercalate "\n" $ map oxFnToRust prog
   in fn_imports <> "\n" <> fns
  where 
    fn_imports = "use crate::builtins::*;\n"


oxFnToRust :: Ox.Fn -> T.Text
oxFnToRust (Ox.Fn name stmts) = 
  T.unlines $ fn_begin <> map (tabIndent . oxStmtToRust) stmts <> fn_end
  where 
    fn_begin = [ "pub fn " <> name <> "() -> Node {" ]
    fn_end = [ "}" ]


oxStmtToRust :: Ox.Stmt -> T.Text
oxStmtToRust (Ox.Let n v) = "let " <> n <> ": Node = " <> oxExprToRust v <> ";"
oxStmtToRust (Ox.Ret e) = "return " <> oxExprToRust e <> ";"


oxExprToRust :: Ox.Expr -> T.Text
oxExprToRust (Ox.Var v) = v
oxExprToRust (Ox.BiArith op a1 a2) = 
  oxArithToRust op <> "(" <> oxExprToRust a1 <> ", " <> oxExprToRust a2 <> ")" 
oxExprToRust (Ox.Val v) = oxValToRust v


oxValToRust :: Ox.Val -> T.Text
oxValToRust (Ox.VInt x) = "int(" <> showt x <> ")"
oxValToRust (Ox.VThunk fname) = "thunk(" <> fname <> ")"


oxArithToRust :: Ox.BiArith -> T.Text
oxArithToRust Ox.Add = "add"
oxArithToRust Ox.Sub = "sub"
oxArithToRust Ox.Mul = "mul"
oxArithToRust Ox.Div = "div"


opToOxArith :: H.IOp -> Ox.BiArith
opToOxArith H.Plus  = Ox.Add
opToOxArith H.Minus = Ox.Sub
opToOxArith H.Mult  = Ox.Mul
opToOxArith H.Div   = Ox.Div


tabIndent :: T.Text -> T.Text
tabIndent text = "    " <> text
