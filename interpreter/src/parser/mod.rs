use std::fmt;

use LExpr::*;
use std::fmt::{Formatter, Debug};

pub enum LExpr<'a> {
  // constants
  LNat(u32),
  LChar(char),
  LBool(bool), 

  // variable
  LVar(&'a str),

  // abstractions
  LApp(Box<LExpr<'a>>, Box<LExpr<'a>>),
  LLambda(&'a str, Box<LExpr<'a>>),

  // enriched bindings
  LLet(LBind<'a>, Box<LExpr<'a>>),
  LLrec(Vec<LBind<'a>>, Box<LExpr<'a>>)
}

pub struct LBind<'a> {
  var: &'a str,
  val: Box<LExpr<'a>>
}

impl Debug for LExpr<'_> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      LNat(n) => write!(f, "{}", n),
      LChar(c) => write!(f, "{}", c),
      LBool(b) => write!(f, "{}", b),
      LVar(v) => write!(f, "{}", v),
      LApp(e1, e2) => write!(f, "( {:?} {:?} )", e1, e2),
      LLambda(v, b) => write!(f, "(\\ {:?} . {:?} )", v, b),
      LLet(bind, body) => write!(f, "( let {:?} in {:?} )", bind, body),
      LLrec(binds, body) => write!(f, "( letrec {:?} in {:?} )", binds, body)
    }
  }
}

impl Debug for LBind<'_> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{:?} = {:?}", self.var, self.val)
  }
}

pub fn parse_stack_code(input: &str) -> Box<LExpr> {
  let mut stack: Vec<LExpr> = Vec::new();

  for line in input.lines() {
    let opcode = &line[0..3];
    let opand = &line[3..].trim();
    match opcode {
      "VAR" => {
        if opand.is_empty() { panic!("VAR (variable) instruction expects var name operand: {}", line) }
        stack.push(LVar(opand))
      },
      "NAT" => stack.push(LNat(opand.parse::<u32>().unwrap())),
      "CHR" => stack.push(LChar(parse_char_opand(opand))),
      "BOO" => stack.push(match opand.trim() {
        "T" => LBool(true),
        "F" => LBool(false),
        bool_str => panic!("BOO expects either 'T' or 'F' operand: {}", bool_str)
      }),
      "APP" => {
        let e1 = stack.pop().unwrap();
        let e2 = stack.pop().unwrap();

        stack.push(LApp(Box::from(e1), Box::from(e2)))
      },
      "LAM" => {
        let var = opand.trim();
        if var.is_empty() { panic!("LAM (lambda) instruction expects var name operand: {}", line) }

        let body = stack.pop().unwrap();

        stack.push(LLambda(var, Box::from(body)))
      },
      "LET" => {
        let bind_val = stack.pop().unwrap();
        let body = stack.pop().unwrap();
        stack.push(LLet(LBind { var: opand, val: Box::from(bind_val) }, Box::from(body)))
      },
      "LRE" => {
        let mut binding_vars: Vec<&str> = opand.split_whitespace().collect();
        let mut bindings: Vec<LBind> = Vec::new();

        binding_vars.reverse(); // reverse so we pop in the right order
        for _ in 0..binding_vars.len() {
          let var = binding_vars.pop().unwrap();
          let val = stack.pop().unwrap();
          let binding: LBind = LBind { var, val: Box::from(val) };
          bindings.push(binding);
        }

        let body = stack.pop().unwrap();
        stack.push(LLrec(bindings, Box::from(body)))
      },

      // fail otherwise
      _ => panic!("Unknown stack instruction: {}", line)
    }
  }

  if stack.is_empty() {
    panic!("Stack instructions resolved to no element")
  } else if stack.len() > 1 {
    panic!("Stack instructions did not resolve to single top level element. Final result: {:?}", stack)
  } else {
    Box::from(stack.pop().unwrap())
  }
}

fn parse_char_opand(opand: &str) -> char {
  if !opand.starts_with("\"") || !opand.ends_with("\"") {
    panic!("CHR opand must be a valid char surrounded by double quotes '\"': {}", opand)
  }

  opand.parse::<char>().unwrap()
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