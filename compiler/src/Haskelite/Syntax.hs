{-# LANGUAGE OverloadedStrings #-}


module Haskelite.Syntax where 

import Data.Void (Void)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Char

-- data ParsecT s u m a 
-- s = stream type
-- u = user state
-- m = underlying monad
-- a = return type

-- type Parsec s u = ParsecT s u Identity

-- parse :: Stream s Identity t => Parsec s () a -> String -> s -> Either ParseError a
-- parse _ sourceName 

type Parser = Parsec T.Text () 

doParseHaskelite :: String -> IO ()
doParseHaskelite input = 
  case parseHaskelite (T.pack input) of 
    Left err -> print err
    Right res -> print res

parseHaskelite :: T.Text -> Either ParseError Char
parseHaskelite = parse parseChar "(unknown)" 

parseChar :: Parser Char
parseChar = satisfy (const True)
