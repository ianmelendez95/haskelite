{-# LANGUAGE OverloadedStrings #-}

module Compile where

import TextShow hiding (singleton)
import Data.List (singleton)
import qualified Data.Text as T
import Haskelite.Parse
import qualified Haskelite.Syntax as H
import qualified Rust.Instr as R

import Text.Parsec (ParseError)


compile :: T.Text -> Either ParseError T.Text
compile input = genRustProg . compileExpr <$> parseHaskelite input


compileExpr :: H.Expr -> T.Text
compileExpr (H.LInt x) = "int(" <> showt x <> ")"
compileExpr (H.IExpr el op er) = rustBinIFun op <> "(" <> compileExpr el <> ", " <> compileExpr er <> ")"


-- Rust Gen


genRustProg :: T.Text -> T.Text
genRustProg instr = T.unlines (fn_begin ++ [ "return " <> instr <> ";" ] ++ fn_end)
  where 
    fn_begin = 
      [ "use crate::builtins::*;"
      , ""
      , "pub fn prog() -> Node {"
      ]

    fn_end = [ "}" ]


rustBinIFun :: H.IOp -> T.Text
rustBinIFun H.Plus  = "add"
rustBinIFun H.Minus = "sub"
rustBinIFun H.Mult  = "mul"
rustBinIFun H.Div   = "div"
 

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
