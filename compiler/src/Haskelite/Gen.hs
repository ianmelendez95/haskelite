module Haskelite.Gen where 

import qualified Haskelite.Syntax as S
import qualified Haskelite.HCode as C

genHCode :: S.Exp -> C.HCode
genHCode (S.LInt x) = C.HInt x  -- 64 bit integer is checked by parser