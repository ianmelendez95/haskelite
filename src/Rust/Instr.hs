module Rust.Instr (Instr (..)) where 

data Instr 
  = PushInt Integer
  | Add
  | Sub
  | Mul
  | Div