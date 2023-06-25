{-# LANGUAGE OverloadedStrings #-}

module Haskelite.Syntax where 

import Data.Void (Void)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Char
import qualified Text.Parsec.Token as P
import Control.Monad.Identity (Identity)
import Text.Parsec.Language (emptyDef)

-- data ParsecT s u m a 
-- s = stream type
-- u = user state
-- m = underlying monad
-- a = return type

-- type Parsec s u = ParsecT s u Identity

-- parse :: Stream s Identity t => Parsec s () a -> String -> s -> Either ParseError a
-- parse _ sourceName 

type Parser = Parsec T.Text () 

newtype Exp = LInt Integer deriving Show

doParseHaskelite :: String -> IO ()
doParseHaskelite input = 
  case parseHaskelite (T.pack input) of 
    Left err -> print err
    Right res -> print res

parseHaskelite :: T.Text -> Either ParseError Exp
parseHaskelite = parse (LInt <$> decimal) "(unknown)" 

parseChar :: Parser Char
parseChar = satisfy (const True)

decimal :: Parser Integer
decimal = P.decimal lexer

lexer :: P.GenTokenParser T.Text () Identity
lexer = P.makeTokenParser haskelite

haskelite :: P.GenLanguageDef T.Text () Identity
haskelite = emptyDef 
  { P.commentStart    = "{-"
  , P.commentEnd      = "-}"
  , P.commentLine     = "--"
  , P.nestedComments  = True
  , P.identStart      = lower <|> char '_'
  , P.identLetter     = alphaNum <|> char '\'' <|> char '_'
  , P.opStart         = P.opLetter haskelite
  , P.opLetter        = oneOf ascSymbols
  , P.reservedNames   = reservedIdentifiers
  , P.reservedOpNames = ["..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]
  , P.caseSensitive   = True
  } 
  where 
    ascSymbols = "!#$%&*+./<=>?@\\^|-~:"

    reservedIdentifiers = 
      [ "case"
      , "class"
      , "data"
      , "default"
      , "deriving"
      , "do"
      , "else"
      , "foreign"
      , "if"
      , "import"
      , "in"
      , "infix"
      , "infixl"
      , "infixr"
      , "instance"
      , "let"
      , "module"
      , "newtype"
      , "of"
      , "then"
      , "type"
      , "where"
      , "_"]