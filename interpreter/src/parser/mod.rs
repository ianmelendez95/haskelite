
pub enum LExpr {
  // constants
  LNat(u32),
  LString(String),
  LBool(bool), 

  // variable
  LVar(String),


  LApp(Box<LExpr>, Box<LExpr>),
  LLambda(String, Box<LExpr>)
}

pub type ParseResult<'a,T> = Result<(T, &'a str), String>;

pub fn parse(input: &str) {
  if !input.is_ascii() { panic!("Can only parse ASCII text!") }

  next_token(input);
  return
}

fn parse_expr<'a>(input: &'a str) -> ParseResult<'a, LExpr> {
  if input.starts_with("true") || input.starts_with("false") {
    parse_bool(input)
  } else {
    panic!("Cannot parse expression: {}", input)
  }
}

fn next_token<'a>(input: &'a str) -> ParseResult<'a, &str> {
  return Ok(("", input));
}

pub fn parse_bool<'a>(input: &'a str) -> ParseResult<'a, LExpr> {
  if input.starts_with("true") {
    Ok((LExpr::LBool(true), &input[4..]))
  } else if input.starts_with("false") {
    Ok((LExpr::LBool(false), &input[5..]))
  } else {
    panic!("Cannot parse boolean: {}", input)
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