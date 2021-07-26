use std::fmt;
use std::str::FromStr;

use LExpr::*;
use SElem::*;
use SInstr::*;
use std::fmt::{Formatter, Debug};
use std::string::ParseError;
use std::collections::VecDeque;

pub enum LExpr {
  // constants
  LNat(u32),
  LString(String),
  LBool(bool), 

  // variable
  LVar(String),

  // abstractions
  LApp(Box<LExpr>, Box<LExpr>),
  LLambda(String, Box<LExpr>),

  // enriched bindings
  LLet(LBind, Box<LExpr>),
  LLetrec(Vec<LBind>, Box<LExpr>)
}

pub struct LBind {
  var: String,
  body: Box<LExpr>
}

/// Stack Element
///
/// Represents elements as parsed blindly
/// in the stack parser.
///
/// Intended to be validated and finally
/// coerced into the final LExpr
pub enum SElem {
  SVar(String),

  SNat(u32),
  SChar(char),
  SBool(bool),

  SApp(Box<SElem>, Box<SElem>),
  SLambda(Box<SElem>, Box<SElem>),

  SBind(Box<SElem>, Box<SElem>), // (var, expr)
  SLet(Box<SElem>, Box<SElem>),  // (binding, body)
  SLrec(Vec<SElem>, Box<SElem>), // (bindings, body)
}

impl Debug for SElem {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      SVar(v) => write!(f, "{}", v),
      SNat(n) => write!(f, "{}", n),
      SChar(c) => write!(f, "{}", c),
      SBool(b) => write!(f, "{}", b),
      SApp(l, r) => write!(f, "( {:?} {:?} )", l, r),
      SLambda(v, b) => write!(f, "(\\ {:?} . {:?} )", v, b),
      SBind(var, val) => write!(f, "{:?} = {:?}", var, val),
      SLet(bind, body) => write!(f, "( LET {:?} IN {:?} )", bind, body),
      SLrec(binds, body) => write!(f, "( LETREC {:?} IN {:?} )", binds, body)
    }
  }
}

// stack instructions
pub enum SInstr {
  SIVar(String),
  SINat(u32),
  SIChar(char),
  SIBool(bool),
  SIApp(),
  SILambda(),
  SIBind(),
  SILet(),
  SILetrec(usize) // (binding count)
}

impl fmt::Display for SInstr {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      SIVar(v) => write!(f, "VAR {}", v),
      SINat(n) => write!(f, "NAT {}", n),
      SIChar(c) => write!(f, "CHR {}", c),
      SIBool(b) => write!(f, "BOO {}", b),
      SIApp() => write!(f, "APP"),
      SILambda() => write!(f, "LAM"),
      SIBind() => write!(f, "BIN"),
      SILet() => write!(f, "LET"),
      SILetrec(bc) => write!(f, "LRE {}", bc)
    }
  }
}

impl FromStr for SInstr {
  type Err = ParseError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    Ok(parse_stack_instruction(s))
  }
}

// pub fn eval_stack_code(input: &str) -> SElem {
//   eval_stack_instructions(parse_stack_instructions(input))
// }

// fn parse_stack_instructions(input: &str) -> Vec<SInstr> {
//   if !input.is_ascii() { panic!("Can only parse ASCII text!") }
//
//   input.lines().map(&parse_stack_instruction).collect()
// }

// fn eval_stack_instructions<'a>(instrs: Vec<SInstr>) -> SElem<'a> {
//   let mut stack: Vec<SElem<'a>> = Vec::new();
//   instrs.iter().for_each(|instr| exec_stack_instruction(&mut stack, instr));
//
//   if stack.is_empty() {
//     panic!("Stack instructions resolved to no element")
//   } else if stack.len() > 1 {
//     panic!("Stack instructions did not resolve to single top level element. Final result: {}", )
//   } else {
//     stack.pop().unwrap()
//   }
// }

fn eval_stack_code(input: &str) -> SElem {
  let mut stack: Vec<SElem> = Vec::new();
  input.lines()
    .map(&parse_stack_instruction)
    .for_each(|instr| exec_stack_instruction(&mut stack, instr));

  if stack.is_empty() {
    panic!("Stack instructions resolved to no element")
  } else if stack.len() > 1 {
    panic!("Stack instructions did not resolve to single top level element. Final result: {:?}", stack)
  } else {
    stack.pop().unwrap()
  }
}

fn exec_stack_instruction(stack: &mut Vec<SElem>,
                          instr: SInstr) {
  match instr {
    SIVar(var) => stack.push(SVar(var)),
    SINat(nat) => stack.push(SNat(nat)),
    SIChar(c) => stack.push(SChar(c)),
    SIBool(bool) => stack.push(SBool(bool)),
    SIApp() => {
      let pop1 = stack.pop().unwrap();
      let pop2 = stack.pop().unwrap();
      stack.push(SApp(Box::from(pop1), Box::from(pop2)));
    },
    SILambda() => {
      let pop1 = stack.pop().unwrap();
      let pop2 = stack.pop().unwrap();
      stack.push(SLambda(Box::from(pop1), Box::from(pop2)));
    },
    SIBind() => {
      let pop1 = stack.pop().unwrap();
      let pop2 = stack.pop().unwrap();
      stack.push(SBind(Box::from(pop1), Box::from(pop2)));
    },
    SILet() => {
      let pop1 = stack.pop().unwrap();
      let pop2 = stack.pop().unwrap();
      stack.push(SLet(Box::from(pop1), Box::from(pop2)));
    },
    SILetrec(binding_count) => {
      // pop the bindings
      let bindings: Vec<SElem>; {
        let mut bs_temp: Vec<SElem> = Vec::new();
        for _ in 0..binding_count {
          bs_temp.push(stack.pop().unwrap());
        }
        bindings = bs_temp;
      }

      // pop the body
      let pop2 = stack.pop().unwrap();

      // push the recursive let
      stack.push(SLrec(bindings, Box::from(pop2)));
    }
  }
}


fn parse_stack_instruction(line: &str) -> SInstr {
  match &line[0..3] {
    "VAR" => SIVar(String::from(&line[4..])),
    "NAT" => SINat(line[4..].parse::<u32>().unwrap()),
    "CHR" => SIChar(line[4..].parse::<char>().unwrap()),
    "BOO" => match &line[4..] {
      "T" => SIBool(true),
      "F" => SIBool(false),
      bool_str => panic!("Unknown boolean value: {}", bool_str)
    },

    "APP" => SIApp(),
    "LAM" => SILambda(),
    "BIN" => SIBind(),
    "LET" => SILet(),
    "LRE" => SILetrec(line[4..].parse::<usize>().unwrap()),

    // fail otherwise
    _ => panic!("Unknown stack instruction: {}", line)
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  pub fn test_parse_bool() {
    let (result, _) = parse_bool("true").unwrap();
    match result {
      LExpr::LBool(true) => (),
      _ => panic!("Didn't return a boolean")
    }
  }
}