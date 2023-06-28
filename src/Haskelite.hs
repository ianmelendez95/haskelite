{-# LANGUAGE OverloadedStrings #-}

module Haskelite where

import TextShow
import qualified Data.Text as T
import qualified Haskelite.Syntax as S

import Text.Parsec (ParseError)

compile :: T.Text -> Either ParseError T.Text
compile input = genRust <$> S.parseHaskelite input

-- Rust Gen

genRust :: S.Exp -> T.Text
genRust (S.LInt x) = progFun (pushInt x)

progFun :: T.Text -> T.Text
progFun body = T.unlines 
  [ "use crate::builtins::State;"
  , ""
  , "pub fn prog(state: &mut State) {"
  , tabIndent body
  , "}"
  ]

tabIndent :: T.Text -> T.Text
tabIndent text = "    " <> text

pushInt :: Integer -> T.Text
pushInt x = "state.push_int(" <> showt x <> ")"