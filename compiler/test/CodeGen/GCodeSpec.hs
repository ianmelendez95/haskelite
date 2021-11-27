module CodeGen.GCodeSpec where 


import Test.Hspec 

import SpecUtil

import Compile (mirandaToGCode)


spec :: Spec 
spec = do 
  describe "GCode Compilation" $ do 
    it "[const-int] Compiles simple integer" $ do
      let test_file_base = "gcode/const-int"
      mcontent <- readMiranda test_file_base
      gcontent <- readGCode test_file_base

      msrc <- parseHunit mcontent

      let gcode = mirandaToGCode msrc

      unlines (map show gcode) `shouldBe` gcontent

    it "[from] Compiles more moderate example" $ do
      let test_file_base = "gcode/from"
      mcontent <- readMiranda test_file_base
      gcontent <- readGCode test_file_base

      msrc <- parseHunit mcontent
      let gcode = mirandaToGCode msrc

      unlines (map show gcode) `shouldBe` gcontent
