module CodeGen.GCode 
  ( compileSCProg
  ) where 


import qualified Lambda.Syntax as S
import qualified Lambda.SCCompiler as SC


data GInstr =  Begin
            | End

            -- stack meta ops
            | MkAp 
            | Eval
            | Unwind
            | Print
            | Alloc Int

            -- stack manipulation
            | PushGlobal String
            | Push Int
            | Update Int
            | Pop Int
            | Slide Int

            -- builtin funcs
            | Neg
            | Add
            | Sub
            | If
            | FatBar
            | Cons
            | Head
            | Tail

            -- pseudo-instruction
            | GlobStart String Int

            -- constants
            | PushInt Int
            | PushChar Char
            | PushBool Bool
            | PushNil
            | PushFail
            | PushError


compileSCProg :: SC.Prog -> [GInstr]
compileSCProg (SC.Prog scs main) = 
  let prelude = 
        [ Begin
        , PushGlobal "$Prog"
        , Eval
        , Print
        , End
        ]
      
      sc_lib = concatMap compileSC scs ++ compileSC (SC.SC "$Prog" [] main)
      sc_builtins = undefined
   in prelude ++ sc_lib ++ sc_builtins


-- | F compilation scheme
compileSC :: SC.SC -> [GInstr]
compileSC sc = 
  let offsets = []
      depth = 0

      globstart = GlobStart (SC.scName sc) (SC.scArity sc)
      
      -- R compilation scheme
      body_code = compileExpr (SC.scBody sc) offsets depth ++ [Update (depth + 1), Pop depth, Unwind]
   in globstart : body_code


--------------------------------------------------------------------------------
-- Expression Compilation


-- Offsets Type

type Offsets = [(String, Int)]

pushOffsets :: [(String, Int)] -> Offsets -> Offsets
pushOffsets = (++)

pushOffset :: String -> Int -> Offsets -> Offsets
pushOffset name offset = ((name, offset) :)

lookupOffset :: String -> Offsets -> Int
lookupOffset name offsets = 
  case lookup name offsets of 
    Nothing -> error $ "No identifier: " ++ name
    Just o -> o


-- | C Compilation scheme
compileExpr :: S.Exp -> Offsets -> Int -> [GInstr]

compileExpr (S.Term (S.Constant c)) _ _ = [compileConstant c]
compileExpr (S.Term (S.Function f)) _ _ = [PushGlobal ('$' : show f)]
compileExpr (S.Term (S.Variable sc@('$' : _))) _ _ = [PushGlobal sc]
compileExpr (S.Term (S.Variable v)) offsets depth = [Push (depth - lookupOffset v offsets)]

compileExpr (S.Apply exp1 exp2) offsets depth = 
  let exp2_code = compileExpr exp2 offsets depth
      exp1_code = compileExpr exp1 offsets (depth + 1)
   in exp2_code ++ exp1_code ++ [MkAp]

compileExpr (S.Let (var, val) body) offsets depth = 
  let val_code = compileExpr val offsets depth

      body_offsets = pushOffset var (depth + 1) offsets
      body_code = compileExpr body body_offsets (depth + 1)

   in val_code ++ body_code ++ [Slide 1]

compileExpr (S.Letrec binds body) offsets depth = 
  let (offsets', depth') = lrBindsContext offsets depth binds

      binds_code = compileLRBinds offsets' depth' binds
      body_code = compileExpr body offsets' depth'

   in binds_code ++ body_code ++ [Slide (depth' - depth)]

compileExpr l@(S.Lambda _ _) _ _ = error $ "Lambda in supercombinator: " ++ show l

compileConstant :: S.Constant -> GInstr
compileConstant (S.CNat n) = PushInt n
compileConstant (S.CChar c) = PushChar c
compileConstant (S.CBool b) = PushBool b
compileConstant S.CNil = PushNil
compileConstant S.CFail = PushFail
compileConstant S.CError = PushError


compileLRBinds :: Offsets -> Int -> [(String, S.Exp)] -> [GInstr]
compileLRBinds offsets depth binds =
  let n = length binds
      binds_code = concat $ zipWith compileEnumBind [n..1] (map snd binds)

   in Alloc n : binds_code
  where 
    compileEnumBind b_n b_val = 
      compileExpr b_val offsets depth ++ [Update b_n]

lrBindsContext :: Offsets -> Int -> [(String, S.Exp)] -> (Offsets, Int)
lrBindsContext offsets depth binds = 
  let bind_offsets = zipWith (\bind_n (bind_v, _) -> (bind_v, bind_n + depth)) 
                             [1..]
                             binds

      offsets' = pushOffsets bind_offsets offsets
      depth'   = depth + length binds
   in (offsets', depth')

