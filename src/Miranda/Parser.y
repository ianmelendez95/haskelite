{
module Miranda.Parser 
  ( ParseResult, 
    parser, 
    parseDef,
    parseExp,
    parseProgram
  ) where 

import Data.Char
import Data.Either (isRight, rights)

import qualified Miranda.Syntax as S
import qualified Miranda.Token as T
import Miranda.Lexer (alexScanTokens, scanTokens, scanTokensEither)

import Debug.Trace
}

%name parser program
%name defParser def
%name expParser exp
%monad { ParseResult } { >>= } { return }
%tokentype { T.Token }
%error { parseError }

%token 
  const       { T.Constant $$       }
  constr      { T.Constructor $$    }
  var         { T.Variable $$       }
  plus        { T.InfixOp T.IPlus       }
  minus       { T.InfixOp T.IMinus      }
  mult        { T.InfixOp T.IMult       }
  div         { T.InfixOp T.IDiv        }
  infix_var   { T.InfixOp (T.IVar _)    } -- $<var-name>

  gtype_2plus { T.GenTypeVar $$     }   -- '2plus' because it's two or more '**', since '*' is mult

  '='         { T.Equal             }
  '::='       { T.TypeEq            }
  '('         { T.LP                }
  ')'         { T.RP                }
  '['         { T.LB                }
  ']'         { T.RB                }
  ';'         { T.Semi              }
  ','         { T.Comma             }
  ':'         { T.Colon             }
  '{'         { T.LC                }
  '}'         { T.RC                }
  '|'         { T.VertBar           }

%%

program :: { S.Prog }
program : '{' stmts '}'          { mkProg (reverse $2) }

-- REVERSE!!
stmts :: { [Stmt] }
stmts : stmts ';' stmt   { $3 : $1 }
      | stmt             { [$1] }

stmt :: { Stmt }
stmt : exp '::=' constructors { Right (checkTypeDef $1 (reverse $3)) }
     | exp '='  exp           { Right (checkFuncOrVarDef $1 $3)      }
     | exp                    { Left  $1 }

-------------------------------------------------------------------------------
-- Def

def :: { S.Def }
def : exp '::=' constructors { (checkTypeDef $1 (reverse $3)) }
    | exp '='  exp           { (checkFuncOrVarDef $1 $3)      }

--------------------------------------
-- Func Def

-- funcDef :: { S.Def }
-- funcDef : funcDefLhs '=' exp    { S.FuncDef (fst $1) (snd $1) $3 }

-- funcDefLhs :: { (String, [S.Pattern]) }
-- funcDefLhs : var funcParams         { ($1, reverse $2) }

-- REVERSE!!: funcParams have *at least one* variable (otherwise it would be a var definition)
-- funcParams :: { [S.Pattern] }
-- funcParams : funcParams patt     { $2 : $1 }
--            | patt                { [$1] }

-- patt :: { S.Pattern }
-- patt : var                 { S.PVar $1    }
--      | constr              { S.PConstr ($1, []) }
--      | '(' constructor ')' { S.PConstr $2 }

--------------------------------------
-- Var Def

-- varDef :: { S.Def }
-- varDef : var '=' exp            { S.VarDef $1 $3 }

--------------------------------------
-- Type Def

-- typeDef :: { S.Def }
-- typeDef : var genTypeVars '::=' constructors  { S.TypeDef $1 (reverse $2) (reverse $4) }

-- REVERSE!!
constructors :: { [S.Constr] }
constructors : constructors '|' constructor { $3 : $1 }
             | constructor                  { [$1] }

constructor :: { S.Constr } -- Constr = (String, [ConstrArg])
constructor : constr constrTypes   { ($1, reverse $2) }

-- REVERSE!!
constrTypes :: { [S.ConstrArg] }
constrTypes : constrTypes constrArg { $2 : $1 }
            | {- empty -}           { [] }

constrArg :: { S.ConstrArg } -- ConstrArg = CAVar String | CAGenTypeVar GenTypeVar
constrArg : var                 { S.CAVar $1 }
          | genTypeVar          { S.CAGenTypeVar $1 }
          | '(' constrArgs ')'  { S.CAList (reverse $2) }

-- REVERSE!!
constrArgs :: { [S.ConstrArg] }
constrArgs : constrArgs constrArg     { $2 : $1 }
           | {- empty -}              { [] }

-- REVERSE!!
genTypeVars :: { [S.GenTypeVar] }
genTypeVars : genTypeVars genTypeVar  { $2 : $1 }
            | {- empty -}             { [] }

genTypeVar :: { S.GenTypeVar } -- GenTypeVar = Int
genTypeVar : mult             { 1 }
           | gtype_2plus      { $1 }

-------------------------------------------------------------------------------
-- Expressions

exp :: { S.Exp }
exp : apply         { $1 }
    | infixApp      { $1 }
    | specialLit    { $1 }
    | term          { $1 }

apply :: { S.Exp }
apply : exp term   { S.Apply $1 $2 }

infixApp :: { S.Exp }
infixApp : exp infixOp exp  { S.InfixApp $2 $1 $3 }

infixOp :: { T.InfixOp }
infixOp : plus      { getInfixOp $1 }
        | minus     { getInfixOp $1 }
        | mult      { getInfixOp $1 }
        | div       { getInfixOp $1 }
        | infix_var { getInfixOp $1 }

term :: { S.Exp }            
term : variable         { $1 }
     | genTypeVar       { S.EGenTypeVar $1 }
     | constr           { S.Constructor $1 }
     | constant         { $1 }
     | '(' exp ')'      { $2 }

variable :: { S.Exp }       
variable : var               { S.Variable $1 }

constant :: { S.Exp }
constant : const             { S.Constant $1 }

-------------------------------------------------------------------------------
-- Special Data Syntax (Lists, Tuples)

specialLit :: { S.Exp }
specialLit : listLit      { $1 }
           | listColonLit { $1 }
           | tupleLit     { $1 }

listLit :: { S.Exp }
listLit : '[' ']'             { S.ListLit [] }
        | '[' commaSepExp ']' { S.ListLit (reverse $2) }

listColonLit :: { S.Exp }
listColonLit : colonSepExp ':' exp    { S.ListColon (reverse ($3 : $1)) }

tupleLit :: { S.Exp }
tupleLit : '(' commaSepExp ')'      { S.Tuple (reverse $2) }

-- REVERSE!!
commaSepExp :: { [S.Exp] }
commaSepExp : commaSepExp ',' exp   { $3 : $1 }
            | exp                   { [$1] }

-- REVERSE!!
colonSepExp :: { [S.Exp] }
colonSepExp : colonSepExp ':' exp   { $3 : $1 }
            | exp                   { [$1] }

{
type ParseResult = Either String

--------------------------------------------------------------------------------
-- Parsers

parseError :: [T.Token] -> ParseResult a
parseError tokens = Left $ "Parse Error, tokens left: " ++ show tokens

parseExp :: String -> ParseResult S.Exp
parseExp input = scanTokensEither input >>= expParser . tail . init

parseDef :: String -> ParseResult S.Def
parseDef input = scanTokensEither input >>= defParser . tail . init

parseProgram :: String -> ParseResult S.Prog
parseProgram input = scanTokensEither input >>= parser

getInfixOp :: T.Token -> T.InfixOp
getInfixOp (T.InfixOp i) = i
getInfixOp tok = error $ "Not an infix op: " ++ show tok

--------------------------------------------------------------------------------
-- The Realm of Ambiguity

type Stmt = Either S.Exp S.Def
type ExpEqExpOrExp = Either S.Exp (S.Exp, S.Exp) -- <exp> OR <exp> = <exp>

mkProg :: [Stmt] -> S.Prog
mkProg stmts = case span isRight stmts of 
                 (defs, [Left expr]) -> S.Prog (rights defs) expr
                 (_, _)        -> error "Expecting a single expression at the end of the program"

--------------------------------------------------------------------------------
-- Coerce Exp -> a

checkTypeDef :: S.Exp -> [S.Constr] -> S.Def
checkTypeDef lhs constrs = 
  case flattenApplyLHS lhs of 
    (S.Variable type_name : rest) -> S.TypeDef type_name (map checkGenTypeVar rest) constrs
    _ -> error $ "Invalid type declaration lhs: " ++ show lhs

checkFuncOrVarDef :: S.Exp -> S.Exp -> S.Def
checkFuncOrVarDef lhs rhs = case flattenApplyLHS lhs of 
                              [S.Variable var_name] -> S.VarDef var_name rhs
                              (S.Variable func_name : rest) -> S.FuncDef func_name (map checkPattern rest) rhs
                              _ -> error $ "Invalid func def lhs: " ++ show lhs

-- coerce an expression into a pattern
checkPattern :: S.Exp -> S.Pattern
checkPattern (S.Variable v) = S.PVar v
checkPattern expr = S.PConstr $ checkConstr expr

checkConstr :: S.Exp -> S.Constr
checkConstr (S.Constructor c) = (c, [])
checkConstr expr@(S.Apply _ _) = case flattenApplyLHS expr of 
                                   (S.Constructor c : rest) -> (c, map checkConstrArg rest)
                                   _ -> error $ "Not a valid constructor: " ++ show expr
checkConstr expr = error $ "Not a valid constructor: " ++ show expr

checkConstrArg :: S.Exp -> S.ConstrArg
checkConstrArg (S.Variable v) = if all (== '*') v then S.CAGenTypeVar (length v)
                                                  else S.CAVar v
checkConstrArg app@(S.Apply _ _) = S.CAList $ map checkConstrArg (flattenApplyLHS app)
checkConstrArg expr = error $ "Not a valid constructor arg: " ++ show expr

checkGenTypeVar :: S.Exp -> S.GenTypeVar
checkGenTypeVar e@(S.Variable v) = if not . all (== '*') $ v then error $ "Not a generalized type variable: " ++ show e
                                                             else length v
checkGenTypeVar (S.EGenTypeVar var) = var
checkGenTypeVar expr = error $ "Not a generalized type variable: " ++ show expr

-- flatten apply for left hand side expressions
--   uniquely flattens infix applications to only recognize '*' infix (interpreted as a gtv)
flattenApplyLHS :: S.Exp -> [S.Exp]
flattenApplyLHS (S.Apply e1 e2) = flattenApplyLHS e1 ++ [e2]
flattenApplyLHS (S.InfixApp T.IMult e1 e2) = flattenApplyLHS e1 ++ [S.EGenTypeVar 1] ++ flattenApplyLHS e2
flattenApplyLHS expr = [expr]
}