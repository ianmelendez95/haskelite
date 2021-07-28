use super::syntax::*;
use super::syntax::LExpr::*;

use std::collections::HashMap;

pub fn evaluate(expr: LExpr) -> LExpr {
  // let env: HashMap<String, LExpr> = HashMap::new();

  match expr {
    LNat(_) => expr,
    LChar(_) => expr,
    LBool(_) => expr,
    LVar(_) => {
      panic!("Not implemented")
    }
    LApp(_, _) => {
      panic!("Not implemented")
    }
    LLambda(_, _) => {
      panic!("Not implemented")
    }
    LLet(_, _) => {
      panic!("Not implemented")
    }
    LLrec(_, _) => {
      panic!("Not implemented")
    }
  }
}
