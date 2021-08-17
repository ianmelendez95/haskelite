module Main where

import System.Environment
import System.FilePath

import Miranda.Compiler
import CodeGen.StackCode

main :: IO ()
main =
  do [m_file] <- getArgs
     let s_file = dropExtension m_file ++ ".s"
     m_content <- readFile m_file
     case compileStr m_content of
       Left e -> error $ "Error parsing Miranda file: " ++ show e
       Right lambda_expr ->
         let s_code = compileLambda lambda_expr
          in writeFile s_file (unlines . map show $ s_code)
