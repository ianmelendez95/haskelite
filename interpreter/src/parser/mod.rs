use LExpr::*;
use PElem::*;

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

/// Parse Element
///
/// Represents elements as parsed blindly
/// in the stack parser.
///
/// Intended to be validated and finally
/// coerced into the final LExpr
pub enum PElem {
  PVar(String),

  PNat(u32),
  PString(String),
  PBool(bool),

  PApp(Box<PElem>, Box<PElem>),
  PLambda(Box<PElem>, Box<PElem>),

  PBind(Box<PElem>, Box<PElem>), // (var, expr)
  PLet(Box<PElem>, Box<PElem>),  // (binding, body)
  PLrec(Vec<PElem>, Box<PElem>), // (bindings, body)
}

pub fn parse_stack_code(input: &str) {
  if !input.is_ascii() { panic!("Can only parse ASCII text!") }

  let mut stack: Vec<PElem> = Vec::new();
  for line in input.lines() {
    if line.starts_with("VAR") {
      stack.push(PVar(String::from(&line[4..])));
    } else if line.starts_with("CON") {
      stack.push(parse_const(&line[6..]));
    } else if line == "APP" {
      let pop1 = stack.pop().unwrap();
      let pop2 = stack.pop().unwrap();
      stack.push(PApp(Box::from(pop1), Box::from(pop2)));
    } else if line == "LAM" {
      let pop1 = stack.pop().unwrap();
      let pop2 = stack.pop().unwrap();
      stack.push(PLambda(Box::from(pop1), Box::from(pop2)));
    } else if line == "BIN" {
      let pop1 = stack.pop().unwrap();
      let pop2 = stack.pop().unwrap();
      stack.push(PBind(Box::from(pop1), Box::from(pop2)));
    } else if line == "LET" {
      let pop1 = stack.pop().unwrap();
      let pop2 = stack.pop().unwrap();
      stack.push(PLet(Box::from(pop1), Box::from(pop2)));
    } else if line == "LRE" {
      // pop the bindings
      let bindings: Vec<PElem>; {
        let mut bs_temp: Vec<PElem> = Vec::new();
        let binding_count = line[4..].parse::<usize>().unwrap();
        for _ in 0..binding_count {
          bs_temp.push(stack.pop().unwrap());
        }
        bindings = bs_temp;
      }

      // pop the body
      let pop2 = stack.pop().unwrap();

      // push the recursive let
      stack.push(PLrec(bindings, Box::from(pop2)));
    }
  }

  return
}

fn parse_const(input: &str) -> PElem {
  panic!();
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