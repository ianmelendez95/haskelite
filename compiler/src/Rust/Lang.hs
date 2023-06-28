{-# LANGUAGE OverloadedStrings #-}

module Rust.Lang where 

import TextShow
import qualified Data.Text as T

progFun :: T.Text -> T.Text
progFun body = T.unlines 
  [ "fn prog(state: &mut State) {"
  , tabIndent body
  , "}"
  ]

tabIndent :: T.Text -> T.Text
tabIndent text = "    " <> text

pushInt :: Integer -> T.Text
pushInt x = "state.push_int(" <> showt x <> ")"