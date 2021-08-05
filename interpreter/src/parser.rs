use super::syntax::*;
use super::syntax::LExpr::*;
use crate::syntax::LFun::LFPlus;

pub fn parse_stack_code(input: &str) -> LExpr {
  let mut stack: Vec<LExpr> = Vec::new();

  for line in input.lines() {
    let opcode = &line[0..3];
    let opand = &line[3..].trim();
    match opcode {
      "VAR" => {
        if opand.is_empty() { panic!("VAR (variable) instruction expects var name operand: {}", line) }
        stack.push(LVar(String::from(*opand)))
      },
      "NAT" => stack.push(LNat(opand.parse::<u32>().unwrap())),
      "CHR" => stack.push(LChar(parse_char_opand(opand))),
      "BOO" => stack.push(match *opand {
        "T" => LBool(true),
        "F" => LBool(false),
        bool_str => panic!("BOO expects either 'T' or 'F' operand: {}", bool_str)
      }),
      "FUN" => {
        stack.push(LFun(parse_builtin_fun(opand)))
      },
      "APP" => {
        let e1 = stack.pop().unwrap();
        let e2 = stack.pop().unwrap();

        stack.push(LApp(Box::from(e1), Box::from(e2)))
      },
      "LAM" => {
        let var = opand.trim();
        if var.is_empty() { panic!("LAM (lambda) instruction expects var name operand: {}", line) }

        let body = stack.pop().unwrap();

        stack.push(LLambda(String::from(var), Box::from(body)))
      },
      "LET" => {
        let bind_val = stack.pop().unwrap();
        let body = stack.pop().unwrap();
        stack.push(LLet(
          LBind {
            var: String::from(*opand),
            val: Box::from(bind_val),
          },
          Box::from(body)))
      },
      "LRE" => {
        let mut binding_vars: Vec<&str> = opand.split_whitespace().collect();
        let mut bindings: Vec<LBind> = Vec::new();

        binding_vars.reverse(); // reverse so we pop in the right order
        for _ in 0..binding_vars.len() {
          let var = binding_vars.pop().unwrap();
          let val = stack.pop().unwrap();
          let binding = LBind {
            var: String::from(var),
            val: Box::from(val),
          };
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
    stack.pop().unwrap()
  }
}

fn parse_char_opand(opand: &str) -> char {
  if !opand.starts_with("\"") || !opand.ends_with("\"") {
    panic!("CHR opand must be a valid char surrounded by double quotes '\"': {}", opand)
  }

  opand.parse::<char>().unwrap()
}

fn parse_builtin_fun(opand: &str) -> LFun {
  match opand {
    "+" => LFPlus(),
    _ => panic!("Unknown builtin function: {}", opand)
  }
}