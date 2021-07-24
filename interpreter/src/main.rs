mod parser;

use parser::*;
use parser::LExpr::*;

const TEXT: &str = "true";

fn main() {
  let (result, _): (LExpr, &str) = parse_bool(TEXT).unwrap();
  match result {
    LBool(b) => println!("Got boolean!: {}", b),
    _ => panic!("didn't parse a boolean")
  }
}
