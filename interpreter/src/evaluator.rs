use super::syntax::*;
use super::syntax::LExpr::*;

use std::collections::HashMap;

type BindMap = HashMap<String, LExpr>;

pub fn evaluate(expr: LExpr) -> LExpr {
  let mut prim_env: BindMap = HashMap::new();

  let mut env = Vec::new();
  env.push(prim_env);

  let mut spine = Vec::new();
  eval_env(expr, &mut spine, &mut env)
}

fn eval_env(expr: LExpr,
            app_spine: &mut Vec<LExpr>, // application spine
            env: &mut Vec<BindMap>) -> LExpr {
  match expr {
    LNat(_) => expr,
    LChar(_) => expr,
    LBool(_) => expr,
    LVar(var) => panic!("Free variable encountered: {}", var),
    LFun(_) => {
      // unwind the spine as necessary
      panic!("Not implemented")
    }
    LApp(e1, e2) => {
      app_spine.push(*e2);
      eval_env(*e1, app_spine, env)
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

fn apply_lambda(l_var: String, l_body: LExpr, expr: LExpr) -> LExpr {
  panic!("Not implemented")
}
