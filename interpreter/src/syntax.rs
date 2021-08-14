use std::fmt;
use std::fmt::{Formatter, Debug};

use LExpr::*;
use std::rc::Rc;
use std::cell::{RefCell};

pub type UBInt = u64;  // UnBoxed Int

#[derive(PartialEq,Debug,Clone)]
pub enum LExpr {
  // constants
  LInt(UBInt),
  LChar(char),
  LBool(bool),

  // builtin function
  LFun(LFun),

  // variable
  LVar(String),

  // abstractions
  LApp(Box<LExpr>, Box<LExpr>),
  LLambda(String, Box<LExpr>),

  // enriched bindings
  LLet(LBind, Box<LExpr>),
  LLrec(Vec<LBind>, Box<LExpr>),

  // runtime values
  LThunkRef(Rc<RefCell<LThunk>>)
}

#[derive(PartialEq,Debug,Clone)]
pub enum LThunk {
  LThunkUnEvaled(LExpr),
  LThunkEvaling(),
  LThunkEvaled(LExpr)
}

#[derive(PartialEq,Debug,Copy,Clone)]
pub enum LFun {
  LFAdd(),
  LFSub(),
  LFMul(),
  LFIf(),
  LFEq(),

  LFY()  // the glorious Y combinator
}

#[derive(PartialEq,Debug,Clone)]
pub struct LBind {
  pub var: String,
  pub val: Box<LExpr>
}

impl fmt::Display for LExpr {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      LInt(n) => write!(f, "{}", n),
      LChar(c) => write!(f, "{}", c),
      LBool(b) => write!(f, "{}", b),
      LVar(v) => write!(f, "{}", v),
      LFun(fun) => write!(f, "{}", fun),
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
      LThunkRef(shared) => {
        let ref_cell: &RefCell<LThunk> = &shared;
        let thunk: &LThunk = &ref_cell.borrow();
        write!(f, "{}", thunk)
      }
    }
  }

}

impl fmt::Display for LFun {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      LFun::LFAdd() => write!(f, "+"),
      LFun::LFSub() => write!(f, "-"),
      LFun::LFMul() => write!(f, "*"),
      LFun::LFIf() => write!(f, "IF"),
      LFun::LFEq() => write!(f, "="),

      LFun::LFY() => write!(f, "Y")
    }
  }
}

impl fmt::Display for LThunk {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      LThunk::LThunkEvaled(e) => write!(f, "{}", e),
      LThunk::LThunkUnEvaled(e) => write!(f, "[ {} ]", e),
      LThunk::LThunkEvaling() => panic!("Displaying a thunk under evaluation!")
    }
  }
}

impl fmt::Display for LBind {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    let LBind { var, val } = self;
    write!(f, "{} = {}", var, val)
  }
}

