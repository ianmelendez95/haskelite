module Main (main) where

import Control.Monad (void)
import System.Environment
import System.FilePath
import System.Directory
import System.Process
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Haskelite

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  args <- getArgs
  case args of 
    [] -> void $ error "Usage: hlite file"
    [hl_file] -> do
      rs_prog <- compileFile hl_file
      let rs_file = cwd </> "build" </> dropExtension (takeFileName hl_file) ++ ".rs"
      TIO.writeFile rs_file rs_prog
      cloneRuntime

compileFile :: FilePath -> IO T.Text
compileFile hl_file = do
  hl_content <- TIO.readFile hl_file
  case compile hl_content of
    Left e -> error $ "Error parsing Haskelite file '" ++ show hl_file ++ ": " ++ show e
    Right rs_code -> pure rs_code

cloneRuntime :: IO ()
cloneRuntime = do 
  createDirectoryIfMissing True "build"
  callProcess "git" ["clone", runtime_repo_url, "build/runtime"]
  pure ()
  where 
    runtime_repo_url :: String
    runtime_repo_url = "https://github.com/ianmelendez95/haskelite-runtime.git"