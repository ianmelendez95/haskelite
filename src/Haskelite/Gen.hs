module Haskelite.Gen where 

import qualified Data.Text as T
import qualified Haskelite.Syntax as S
import qualified Rust.Lang as R

genRust :: S.Exp -> T.Text
genRust (S.LInt x) = R.progFun (R.pushInt x)