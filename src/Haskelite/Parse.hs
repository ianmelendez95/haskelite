{-# LANGUAGE OverloadedStrings #-}

module Haskelite.Parse (doParseHaskelite, parseHaskelite) where 

import qualified Data.Text as T
import Text.Parsec
import qualified Text.Parsec.Token as P
import Control.Monad.Identity (Identity)
import Text.Parsec.Language (emptyDef)

import Haskelite.Syntax
import Text.Parsec.Expr 

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


parseHaskelite :: T.Text -> Either ParseError Expr
parseHaskelite = parse (expr <* eof) "(unknown)"


expr :: Parser Expr 
expr = choice [ letExpr, binExpr ]


letExpr :: Parser Expr
letExpr = do 
  lexeme $ reserved "let"
  var <- lexeme identifier
  lexeme $ reservedOp "="
  val <- lexeme binExpr
  lexeme $ reserved "in"
  body <- lexeme binExpr
  pure $ Let var val body


binExpr :: Parser Expr
binExpr = buildExpressionParser table term <?> "binary/term expression"
  where 
    term :: Parser Expr
    term = choice [ integer, var ]

    table :: OperatorTable T.Text () Identity Expr
    table = 
      [ [ binary "*" (mkIExpr Mult)  AssocLeft
        , binary "/" (mkIExpr Div)   AssocLeft
        ]
      , [ binary "+" (mkIExpr Plus)  AssocLeft
        , binary "-" (mkIExpr Minus) AssocLeft
        ] 
      ]

    binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
    -- prefix  name fun       = Prefix (do{ reservedOp name; return fun })
    -- postfix name fun       = Postfix (do{ reservedOp name; return fun })

    mkIExpr :: IOp -> Expr -> Expr -> Expr
    mkIExpr op el er = IExpr el op er


var :: Parser Expr
var = Var <$> identifier


integer :: Parser Expr
integer = do 
  num <- lexeme $ P.decimal lexer
  if num > fromIntegral max64BitInt
    then fail $ "Integer literals may only be 64 bits (max size" ++ show max64BitInt ++ ")" 
    else pure (LInt num) <?> "integer"
  where 
    max64BitInt :: Integer
    max64BitInt = (2 :: Integer) ^ (63 :: Integer)


identifier :: Parser T.Text
identifier = T.pack <$> P.identifier lexer


reserved :: String -> Parser ()
reserved = P.reserved lexer


reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer


lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer


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