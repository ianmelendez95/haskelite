use std::fmt;
use std::fmt::{Formatter, Debug};

use LExpr::*;

pub enum LExpr {
  // constants
  LNat(u32),
  LChar(char),
  LBool(bool),

  // variable
  LVar(String),

  // abstractions
  LApp(Box<LExpr>, Box<LExpr>),
  LLambda(String, Box<LExpr>),

  // enriched bindings
  LLet(LBind, Box<LExpr>),
  LLrec(Vec<LBind>, Box<LExpr>)
}

pub struct LBind {
  pub var: String,
  pub val: Box<LExpr>
}

impl fmt::Display for LExpr {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      LNat(n) => write!(f, "{}", n),
      LChar(c) => write!(f, "{}", c),
      LBool(b) => write!(f, "{}", b),
      LVar(v) => write!(f, "{}", v),
      LApp(e1, e2) => write!(f, "( {} {} )", e1, e2),
      LLambda(v, b) => write!(f, "(\\ {} . {} )", v, b),
      LLet(bind, body) =>
        write!(f, "( let {{ {} }} in {} )", bind, body),
      LLrec(binds, body) => {
        write!(f, "( letrec {{ ")?;

        let mut first = true;
        for bind in binds {
          if first { first = false; } else { write!(f, " ; ")?; }
          write!(f, "{}", bind)?;
        }

        write!(f, " }} in {} )", body)
      }
    }
  }

}

impl fmt::Display for LBind {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    let LBind { var, val } = self;
    write!(f, "{} = {}", var, val)
  }
}

impl Debug for LExpr {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      LNat(n) => write!(f, "{}", n),
      LChar(c) => write!(f, "{}", c),
      LBool(b) => write!(f, "{}", b),
      LVar(v) => write!(f, "{}", v),
      LApp(e1, e2) => write!(f, "( {:?} {:?} )", e1, e2),
      LLambda(v, b) => write!(f, "(\\ {:?} . {:?} )", v, b),
      LLet(bind, body) => write!(f, "( let {:?} in {:?} )", bind, body),
      LLrec(binds, body) => write!(f, "( letrec {:?} in {:?} )", binds, body)
    }
  }
}

impl Debug for LBind {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{:?} = {:?}", self.var, self.val)
  }
}

