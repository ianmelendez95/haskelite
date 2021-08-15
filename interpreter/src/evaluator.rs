use super::syntax::*;
use super::syntax::LExpr::*;
use super::syntax::LFun::*;
use std::rc::Rc;
use std::cell::{RefCell};
use crate::syntax::LThunk::*;
use std::ops::{Add, Sub, Mul};

pub fn evaluate(expr: LExpr) -> LExpr {
  eval(expr)
}

fn eval(expr: LExpr) -> LExpr {
  match expr {
    LInt(_) => expr,
    LChar(_) => expr,
    LBool(_) => expr,
    LVar(var) => panic!("Free variable encountered: {}", var),
    LFun(_) => expr,
    LApp(e1, e2) => {
      let mut app_spine = Vec::new();
      app_spine.push(*e2);
      eval(eval_app(*e1, &mut app_spine))
    }
    LLambda(_, _) => expr,
    LLet(_, _) => {
      panic!("Not implemented")
    }
    LLrec(_, _) => {
      panic!("Not implemented")
    }
    LThunkRef(shared_expr) => {
      eval_thunk(shared_expr)
    }
  }
}

fn eval_thunk(thunk_ref: Rc<RefCell<LThunk>>) -> LExpr {
  let thunk: LThunk = thunk_ref.replace(LThunkEvaling()); // TODO - this is basically a poor man's mutex, likely a candidate for better borrowing practices
  match thunk {
    LThunkEvaling() => panic!("Attempting to evaluate thunk undergoing evaluation!"),
    LThunkEvaled(expr) => {
      let clone_expr = expr.clone();
      thunk_ref.replace(LThunkEvaled(expr));
      clone_expr
    },
    LThunkUnEvaled(expr) => {
      let evaled_expr = eval(expr);
      if let LThunkRef(_) = &evaled_expr {
        panic!("Evaling expression returned thunk: {}", evaled_expr)
      }

      thunk_ref.replace(LThunkEvaled(evaled_expr.clone()));
      evaled_expr
    }
  }
}

fn eval_app(cur_expr: LExpr,
            app_spine: &mut Vec<LExpr>) -> LExpr {
  // get result of attempting to apply left expression
  let res = match cur_expr {
    LInt(_) => panic!("Cannot apply number: {}", cur_expr),
    LChar(_) => panic!("Cannot apply char: {}", cur_expr),
    LBool(_) => panic!("Cannot apply bool: {}", cur_expr),
    LVar(var) => panic!("Free variable encountered: {}", var),

    // builtin functions
    LFun(LFAdd()) => {
      let arg1 = app_spine.pop().expect("+ missing first argument");
      let arg2 = app_spine.pop().expect("+ missing second argument");

      builtin_add(arg1, arg2)
    }
    LFun(LFSub()) => {
      let arg1 = app_spine.pop().expect("- missing first argument");
      let arg2 = app_spine.pop().expect("- missing second argument");

      builtin_sub(arg1, arg2)
    }
    LFun(LFMul()) => {
      let arg1 = app_spine.pop().expect("* missing first argument");
      let arg2 = app_spine.pop().expect("* missing second argument");

      builtin_mul(arg1, arg2)
    }
    LFun(LFIf()) => {
      let cond = app_spine.pop().expect("IF missing condition");
      let true_val = app_spine.pop().expect("IF missing true clause");
      let false_val = app_spine.pop().expect("IF missing false clause");

      builtin_if(cond, true_val, false_val)
    }
    LFun(LFEq()) => {
      let arg1 = app_spine.pop().expect("- missing first argument");
      let arg2 = app_spine.pop().expect("- missing second argument");

      builtin_eq(arg1, arg2)
    }
    LFun(LFY()) => {
      let arg = app_spine.pop().expect("Y: missing argument");

      let arg_shared = LThunkRef(Rc::from(RefCell::from(LThunkUnEvaled(arg))));

      // push Y applied to the arg back on the spine, emulating recursion
      app_spine.push(LApp(Box::from(LFun(LFY())),
                               Box::from(arg_shared.clone())));

      // TODO - avoid going further into the stack (pop out trampoline style or something)
      eval_app(arg_shared, app_spine)
    }

    // further application
    LApp(e1, e2) => {
      app_spine.push(*e2);
      eval_app(*e1, app_spine)
    }

    // lambda application
    LLambda(l_var, l_body) => {
      let subst_val = app_spine.pop().expect(format!("lambda: missing value: l_var='{}'", l_var).as_str());

      let subst_ref;
      if let LThunkRef(val_ref) = subst_val {
        subst_ref = val_ref;
      } else {
        subst_ref =
          Rc::from(RefCell::from(LThunkUnEvaled(subst_val)));
      }

      instantiate_lambda(&l_var, &l_body, subst_ref)
    }

    // let constructs
    LLet(_, _) => {
      panic!("Not implemented")
    }
    LLrec(_, _) => {
      panic!("Not implemented")
    }

    // for thunk, simply return evaluated state
    LThunkRef(shared_expr) => {
      eval_thunk(shared_expr)
    }
  };

  // continue eval if there are still arguments on the app spine
  if !app_spine.is_empty() {
    eval_app(res, app_spine)
  } else {
    res
  }
}

fn instantiate_lambda(l_var: &str, l_expr: &LExpr, subst_val: Rc<RefCell<LThunk>>) -> LExpr {
  match l_expr {
    LInt(n) => LInt(*n),
    LChar(c) => LChar(*c),
    LBool(b) => LBool(*b),
    LFun(f) => LFun(*f),
    LVar(v) => {
      if v == l_var {
        LThunkRef(subst_val)
      } else {
        LVar(v.clone())
      }
    }
    LApp(e1, e2) => {
      let new_e1 = instantiate_lambda(l_var, e1, subst_val.clone()); // this clone could be wasteful, if var isn't free in e1
      let new_e2 = instantiate_lambda(l_var, e2, subst_val);

      LApp(Box::from(new_e1), Box::from(new_e2))
    }
    LLambda(v, b) => {
      if l_var == v {
        // l_var cannot free in b, so we know we can simply clone the body
        LLambda(v.clone(), b.clone())
      } else {
        let new_b = instantiate_lambda(l_var, b, subst_val);

        LLambda(v.clone(), Box::from(new_b))
      }
    }
    LLet(bind, body) => {
      let LBind { var: b_var, val: b_val } = bind;

      if b_var == l_var {
        // l_var is not free in val or body, so simply clone
        l_expr.clone()
      } else {
        let new_bind = instantiate_binding(l_var, b_var, b_val, subst_val.clone());
        let new_body = instantiate_lambda(l_var, body, subst_val);

        LLet(new_bind, Box::from(new_body))
      }
    }
    LLrec(binds, b_body) => {
      if var_bound_in_bindings(l_var, binds) {
        // l_var is not free in expression, so simply clone
        l_expr.clone()
      } else {
        let new_binds = binds.iter()
          .map(| binding |
            instantiate_binding(l_var,
                                &binding.var,
                                &binding.val,
                                subst_val.clone()))
          .collect();
        let new_body = instantiate_lambda(l_var, b_body, subst_val);

        LLrec(new_binds, Box::from(new_body))
      }
    }
    LThunkRef(shared_expr) => {
      // thunks are always an argument to a lambda application,
      // and thus never have free variables,
      // and so we can simply copy the reference
      LThunkRef(shared_expr.clone())
    }
  }
}

fn instantiate_binding(l_var: &str, b_var: &str, b_val: &LExpr, subst_val: Rc<RefCell<LThunk>>) -> LBind {
  let new_b_var = String::from(b_var);
  let new_b_val = instantiate_lambda(l_var, b_val, subst_val);

  LBind { var: new_b_var, val: Box::from(new_b_val) }
}

fn var_bound_in_bindings(var: &str, bindings: &Vec<LBind>) -> bool {
  bindings.iter().any(| binding | binding.var == var)
}

fn builtin_add(x: LExpr, y: LExpr) -> LExpr {
  LInt(builtin_bin_arith(LFAdd().to_string().as_str(),
                         x,
                         y,
                         UBInt::add))
}

fn builtin_sub(x: LExpr, y: LExpr) -> LExpr {
  LInt(builtin_bin_arith(LFSub().to_string().as_str(),
                         x,
                         y,
                         UBInt::sub))
}

fn builtin_mul(x: LExpr, y: LExpr) -> LExpr {
  LInt(builtin_bin_arith(LFSub().to_string().as_str(),
                         x,
                         y,
                         UBInt::mul))
}

fn builtin_bin_arith(name: &str,
                     arg1: LExpr,
                     arg2: LExpr,
                     arith_f: fn(UBInt, UBInt) -> UBInt) -> UBInt {
  // TODO - such type checking shouldn't be done (compiler type checker ensures we never eval the wrong 'type')
  match eval(arg1) {
    LInt(n_x) => {
      match eval(arg2) {
        LInt(n_y) => arith_f(n_x, n_y),
        y_val => panic!("{}: second argument is not a number: {}", name, y_val)
      }
    }
    x_val => panic!("{}: first argument is not a number: {}", name, x_val)
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

fn builtin_eq(x: LExpr, y: LExpr) -> LExpr {
  let x_eval = eval(x);
  if ! is_constant_expression(&x_eval) {
    panic!("=: first argument is not a constant: {}", x_eval)
  }

  let y_eval = eval(y);
  if ! is_constant_expression(&y_eval) {
    panic!("=: second argument is not a constant: {}", y_eval)
  }

  LBool(x_eval == y_eval)
}

fn is_constant_expression(x: &LExpr) -> bool {
  match x {
    LInt(_) => true,
    LChar(_) => true,
    LBool(_) => true,
    _ => false
  }
}
