module Build (buildHlFile) where 

import Control.Monad (void, when)
import System.FilePath
import System.Directory
import System.Process
import System.Exit
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Compile

buildHlFile :: FilePath -> IO ()
buildHlFile hl_file = do
  rs_prog <- compileFile hl_file
  rmRfDirSafe "build"
  rmRfDirSafe "dist"
  cloneRuntime
  TIO.writeFile "build/runtime/src/prog.rs" rs_prog
  buildProg
  dist (dropExtension . takeFileName $ hl_file)


rmRfDirSafe :: FilePath -> IO ()
rmRfDirSafe dir = do
  exists <- doesDirectoryExist dir
  when exists (removeDirectoryRecursive dir)


compileFile :: FilePath -> IO T.Text
compileFile hl_file = do
  hl_content <- TIO.readFile hl_file
  case compile hl_content of
    Left e -> error $ "Error parsing Haskelite file '" ++ show hl_file ++ ": " ++ show e
    Right rs_code -> pure rs_code


cloneRuntime :: IO ()
cloneRuntime = do 
  createDirectoryIfMissing True "build"
  callProcess "git" ["clone", "--depth=1", runtime_repo_url, "build/runtime"]
  rmRfDirSafe "build/runtime/.git"  -- avoids test builds getting included in git management
  pure ()
  where 
    runtime_repo_url :: String
    runtime_repo_url = "https://github.com/ianmelendez95/haskelite-runtime.git"


buildProg :: IO ()
buildProg = 
  withCreateProcess (proc "cargo" ["build"]){ cwd = Just "build/runtime", delegate_ctlc = True } $ \_ _ _ p -> do
    r <- waitForProcess p
    case r of 
      ExitSuccess -> pure ()
      ExitFailure rc -> void $ error $ "Failed to build executable with exit code: " ++ show rc


dist :: String -> IO ()
dist bin_filename = do 
  createDirectoryIfMissing True "dist"
  copyFile "build/runtime/target/debug/runtime" ("dist" </> bin_filename)