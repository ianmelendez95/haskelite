{-# LANGUAGE OverloadedStrings #-}

module Compile where

import TextShow
import qualified Data.Text as T
import Haskelite.Parse
import qualified Haskelite.Syntax as H
import qualified Rust.Instr as R

import Text.Parsec (ParseError)


compile :: T.Text -> Either ParseError T.Text
compile input = genRustProg . genRustInstrs <$> parseHaskelite input


compileExpr :: H.Expr -> [R.Instr]
compileExpr (H.LInt x) = [R.PushInt x]
compileExpr (H.IExpr el H.Plus er) = genRustInstrs er ++ genRustInstrs el ++ [R.Add]


-- Rust Gen


genRustProg :: [R.Instr] -> T.Text
genRustProg instrs = T.unlines (fn_begin ++ map (tabIndent . genRustStmt) instrs ++ fn_end)
  where 
    fn_begin = 
      [ "use crate::builtins::State;"
      , ""
      , "pub fn prog(state: &mut State) {"
      ]

    fn_end = [ "}" ]
 

genRustInstrs :: H.Expr -> [R.Instr]
genRustInstrs (H.LInt x) = [R.PushInt x]
genRustInstrs (H.IExpr el op er) = genRustInstrs er ++ genRustInstrs el ++ [rustBinOp op]
  where 
    rustBinOp :: H.IOp -> R.Instr
    rustBinOp H.Plus  = R.Add
    rustBinOp H.Minus = R.Sub
    rustBinOp H.Mult  = R.Mul
    rustBinOp H.Div   = R.Div


genRustStmt :: R.Instr -> T.Text
genRustStmt (R.PushInt x) = "state.push_int(" <> showt x <> ");"
genRustStmt R.Add = "state.add();"
genRustStmt R.Sub = "state.sub();"
genRustStmt R.Mul = "state.mul();"
genRustStmt R.Div = "state.div();"


tabIndent :: T.Text -> T.Text
tabIndent text = "    " <> text


pushInt :: Integer -> T.Text
pushInt x = "state.push_int(" <> showt x <> ")"
