mod parser;
mod syntax;

use parser::*;

const TEXT: &str = "\
NAT 20
VAR ADD
APP
";

fn main() {
  let mut stack_code = String::new();
  stack_code.push_str(TEXT);
  println!("Result: {:?}", parse_stack_code(&*stack_code))
}
