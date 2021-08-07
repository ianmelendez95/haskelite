mod parser;
mod syntax;
mod evaluator;

use std::env;
use std::fs;
use syntax::LExpr;


fn main() {
  let args: Vec<String> = env::args().collect();

  if args.len() != 2 {
    panic!("Usage: interpreter <stack-code-file>");
  }

  let stack_code_file = &args[1];
  let stack_code = fs::read_to_string(stack_code_file).unwrap();

  println!("{}", eval_stack_code(stack_code.as_str()))
}

fn eval_stack_code(code: &str) -> LExpr {
  evaluator::evaluate(parser::parse_stack_code(code))
}
