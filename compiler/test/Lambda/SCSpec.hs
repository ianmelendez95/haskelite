module Lambda.SCSpec where

import Test.Hspec 
import qualified Lambda.SCCompiler as SC
import qualified Lambda.Syntax as S

spec :: Spec 
spec = do 
  describe "Super Combinator Compilation" $ do 
    it "p225: compiles simple expression" $ do
      let lam = S.mkApply [
              S.mkLambda ["x", "y"] (S.mkApply [ S.mkFunction S.FMinus, 
                                                 S.mkVariable "y",
                                                 S.mkVariable "x"]),
              S.mkConstant (S.toConstant (3 :: Int)),
              S.mkConstant (S.toConstant (4 :: Int)) 
            ]

      print (SC.compileSCs lam)