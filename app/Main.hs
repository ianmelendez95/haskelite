module Main (main) where

import Control.Monad (void)
import System.Environment

import Build

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [hl_file] -> buildHlFile hl_file
    _ -> void $ error "Usage: hlite file"
