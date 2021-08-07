use super::syntax::*;
use super::syntax::LExpr::*;
use super::syntax::LFun::*;

pub fn evaluate(expr: LExpr) -> LExpr {
  eval(expr)
}

fn eval(expr: LExpr) -> LExpr {
  match expr {
    LNat(_) => expr,
    LChar(_) => expr,
    LBool(_) => expr,
    LVar(var) => panic!("Free variable encountered: {}", var),
    LFun(_) => expr,
    LApp(e1, e2) => {
      let mut app_spine = Vec::new();
      app_spine.push(*e2);
      eval(eval_app(*e1, &mut app_spine))
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

fn eval_app(cur_expr: LExpr,
            app_spine: &mut Vec<LExpr>) -> LExpr {
  match cur_expr {
    LNat(_) => panic!("Cannot apply number: {}", cur_expr),
    LChar(_) => panic!("Cannot apply char: {}", cur_expr),
    LBool(_) => panic!("Cannot apply bool: {}", cur_expr),
    LVar(var) => panic!("Free variable encountered: {}", var),
    LFun(fun) => {
      return match fun {
        LFPlus() => {
          let arg1 = app_spine.pop().expect("+ missing first argument");
          let arg2 = app_spine.pop().expect("+ missing second argument");

          builtin_plus(arg1, arg2)
        },
        LFIf() => {
          let cond = app_spine.pop().expect("IF missing condition");
          let true_val = app_spine.pop().expect("IF missing true clause");
          let false_val = app_spine.pop().expect("IF missing false clause");

          builtin_if(cond, true_val, false_val)
        }
      }
    }
    LApp(e1, e2) => {
      app_spine.push(*e2);
      eval_app(*e1, app_spine)
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

fn builtin_if(cond: LExpr, true_val: LExpr, false_val: LExpr) -> LExpr {
  match eval(cond) {
    LBool(bool) => {
      if bool { true_val } else { false_val }
    },
    res => panic!("IF condition should evaluate to boolean: {}", res)
  }
}

fn builtin_plus(x: LExpr, y: LExpr) -> LExpr {
  match eval(x) {
    LNat(n_x) => {
      match eval(y) {
        LNat(n_y) => LNat(n_x + n_y),
        _ => panic!("Expecting natural numbers for summation, first argument = {}", x)
      }
    }
    _ => panic!("Expecting natural numbers for summation, second argument = {}", y)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_eval_simple_sum() -> Result<(), String> {
    let x = LNat(13);
    let y = LNat(29);

    let plus_x = LApp(Box::from(LFun(LFPlus())),
                      Box::from(x));
    let plus_x_y = LApp(Box::from(plus_x),
                        Box::from(y));

    assert_eq!(LNat(42), evaluate(plus_x_y));
    Ok(())
  }

  #[test]
  fn test_eval_sum_of_sums() -> Result<(), String> {
    let plus = LFun(LFPlus());
    let x = LNat(13);
    let y = LNat(29);

    let plus_x = LApp(Box::from(plus.clone()),
                      Box::from(x.clone()));
    let plus_x_y = LApp(Box::from(plus_x.clone()),
                        Box::from(y.clone()));

    let plus__plus_x_y =
      LApp(Box::from(plus.clone()),
           Box::from(plus_x_y.clone()));
    let plus__plus_x_y__plus_x_y =
      LApp(Box::from(plus__plus_x_y.clone()),
           Box::from(plus_x_y.clone()));

    assert_eq!(LNat(84), evaluate(plus__plus_x_y__plus_x_y));
    Ok(())
  }
}
