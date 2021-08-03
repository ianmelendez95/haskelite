use super::syntax::*;
use super::syntax::LExpr::*;

use std::collections::HashMap;
use LValue::*;
use std::borrow::Borrow;

#[derive(Clone, Copy)]
pub enum LValue {
  // builtin functions
  LVFAdd(),

  // constants
  LVCNat(u32),
  LVCChar(char),
  LVCBool(bool)
}

type EnvMap = HashMap<String, LValue>;

pub fn evaluate(expr: LExpr) -> LValue {
  let mut prim_env: EnvMap = HashMap::new();
  prim_env.insert(String::from("+"), LVFAdd());

  let mut env = Vec::new();
  env.push(prim_env);

  eval_env(expr, &mut env)
}

fn eval_env(expr: LExpr, env: &mut Vec<EnvMap>) -> LValue {
  match expr {
    LNat(n) => LVCNat(n),
    LChar(c) => LVCChar(c),
    LBool(b) => LVCBool(b),
    LVar(var) => {
      return *env.get(&*var).expect(&*format!("No variable with name in environment: {}", var));
    }
    LApp(e1, e2) => {
      let v1 = eval_env(*e1, env);
      let v2 = eval_env(*e2, env);

      apply(v1, v2)
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

fn apply(v1: LValue, v2: LValue) -> LValue {

}
