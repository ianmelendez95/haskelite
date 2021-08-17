module Main where

import System.Environment
import System.FilePath

import qualified Miranda.Syntax as M
import Lambda.ToLambda
import Parse
import CodeGen.StackCode

main :: IO ()
main =
  do [m_file] <- getArgs
     let s_file = dropExtension m_file ++ ".s"
     m_content <- readFile m_file
     case parse m_content :: Either String M.Prog of
       Left e -> error $ "Error parsing Miranda file: " ++ e
       Right m_prog ->
         let l_prog = toLambda m_prog
             s_code = compileLambda l_prog
          in writeFile s_file (show s_code)

-- main :: IO ()
-- main = do args <- getArgs  
--           case args of 
--             [] -> runInputT (defaultSettings { historyFile = Just "hjs-history" }) 
--                             loop 
--             (file : _) -> do content <- readFile file
--                              evalLambda content

-- loop :: InputT IO ()
-- loop = 
--   do minput <- getInputLine "> "
--      case minput of
--        Nothing -> return ()
--        Just ".exit" -> return ()
--        Just input -> do liftIO $ evalLambda input
--                         loop

-- evalLambda :: String -> IO ()
-- evalLambda input = do let tokens = scanTokens input
--                       putStr "tokens: "        >> catchAll (print tokens)
--                       case parser tokens of 
--                         Left err -> error err
--                         Right parsedEnriched -> 
--                           do let parsed = toLambda parsedEnriched
--                                  reduced = reduce parsedEnriched
--                                  evaled = eval parsedEnriched
--                              putStr "enriched: "      >> catchAll (pPrint parsedEnriched)
--                              putStr "parsed: "        >> catchAll (pPrint parsed)
--                              putStr "raw: "           >> catchAll (pPrint parsed)
--                              putStr "beta reduced: "  >> catchAll (pPrint reduced)
--                              putStr "evaled: "        >> catchAll (pPrint evaled)

-- catchAll :: IO () -> IO ()
-- catchAll io = catch io (print :: SomeException -> IO ())

-- pPrint :: PrettyLambda a => a -> IO ()
-- pPrint expr = renderIO System.IO.stdout (ansiPrettyDoc expr) >> putChar '\n'
                    