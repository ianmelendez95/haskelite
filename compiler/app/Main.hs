module Main (main) where

import Control.Monad (void)
import System.Environment
import System.FilePath
import System.Directory
import qualified Data.Text.IO as TIO

import Haskelite

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [] -> void $ error "Usage: hlite file"
    [hl_file] -> do
      cwd <- getCurrentDirectory
      let rs_file = cwd </> dropExtension (takeFileName hl_file) ++ ".rs"
      hl_content <- TIO.readFile hl_file
      case compile hl_content of
        Left e -> error $ "Error parsing Haskelite file '" ++ show hl_file ++ ": " ++ show e
        Right rs_code -> TIO.writeFile rs_file rs_code
