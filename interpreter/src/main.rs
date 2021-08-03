mod parser;
mod syntax;
mod evaluator;

use std::io::{self, Read};
use parser::*;
use crate::evaluator::evaluate;
use syntax::LExpr;
use std::fs::read;
use crate::syntax::LExpr::LBool;

const TEXT: &str = "\
NAT 20
VAR ADD
APP
";

fn main() {
  println!("{}", eval_stack_code(&read_stdin()))
}

fn read_stdin() -> String {
  let mut buffer = String::new();
  io::stdin().read_to_string(&mut buffer).expect("Unable to read stack code");
  buffer
}

fn eval_stack_code(code: &str) -> LExpr {
  // evaluator::evaluate(parser::parse_stack_code(code))
  return LBool(false)
}
