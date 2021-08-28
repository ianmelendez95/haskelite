module Lambda.SCSpec where

import Test.Hspec 
import qualified Lambda.SCCompiler as SC
import qualified Lambda.Syntax as S

spec :: Spec 
spec = do 
  describe "Super Combinator Compilation" $ do 
    it "p225: compiles simple expression" $ do
      -- > (\x. (\y. + y x))
      let lam = S.mkApply [
              S.mkLambda ["x", "y"] (S.mkApply [ S.mkFunction S.FMinus, 
                                                 S.mkVariable "y",
                                                 S.mkVariable "x"]),
              S.mkConstant (S.toConstant (3 :: Int)),
              S.mkConstant (S.toConstant (4 :: Int)) 
            ]

      print (SC.compileSCs lam)

    it "p225: compiles trinary sc" $ do
      -- (\x. (\y. (\z. + x y z))) 3 4 5
      -- > (\x. (\y. $1 y x))
      -- > (\x. $2 x)
      -- =>
      -- > $3 x = $2 x
      -- > $2 y x = $1 y x
      -- > $1 z y x = + x y z
      -- > ----------------
      -- > $3 3 4 5
      --
      -- > $1 z y x = + x y z
      -- > ----------------
      -- > $1 3 4 5
      let lam = S.mkApply [
              S.mkLambda ["x", "y", "z"] 
                         (S.mkApply [S.mkFunction S.FPlus, 
                                     S.mkVariable "x",
                                     S.mkVariable "y",
                                     S.mkVariable "z"]),
              S.mkConstant (S.toConstant (3 :: Int)),
              S.mkConstant (S.toConstant (4 :: Int)),
              S.mkConstant (S.toConstant (5 :: Int))
            ]

      putStrLn "TRINARY"
      print (SC.compileSCs lam)