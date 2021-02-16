-- | Primarily for use in GHCI
module GhciUtil where 

import Parse
import Lambda.Reduce (Reducible (..))
import Lambda.Pretty (PrettyLambda (..))
import Lambda.Enriched (Exp (..))
import Lambda.ToLambda

import Miranda.Lexer (scanTokens)
import Miranda.Syntax (Prog)

evalMiranda :: String -> IO () 
evalMiranda = pPrint . reduce . either error id . (parse :: String -> Either String Prog)

toLambdaMiranda :: String -> IO () 
toLambdaMiranda = pPrint . toLambda . parseMiranda

parseMiranda :: String -> Prog
parseMiranda = eitherError . parse

lexMiranda :: String -> String
lexMiranda = show . scanTokens

evalEnriched :: String -> IO ()
evalEnriched = pPrint . reduce . either error id . (parse :: String -> Either String Exp)

pPrint :: PrettyLambda a => a -> IO ()
pPrint = putStrLn . pShow

eitherError :: Either String a -> a
eitherError = either error id