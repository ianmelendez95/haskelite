mod parser;

use parser::*;

const TEXT: &str = "\
NAT 20
VAR ADD
APP
";

fn main() {
  println!("Result: {:?}", parse_stack_code(TEXT))
}
