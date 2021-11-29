use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;
use std::fmt::Formatter;

pub type UBInt = i64;

#[derive(Debug,Copy,Clone)]
pub enum GCode {

}

#[derive(Clone)]
pub enum GNode {
  GInt(UBInt),
  GCons(Rc<GNode>, Rc<GNode>),
  GAp(Rc<GNode>, Rc<GNode>),
  GFun(UBInt, Rc<GProg>),
  GProc(fn(&mut GState)),
  GHole(),
  GSRef(Rc<GNode>)
}

pub struct DNode {
  pub stack: GStack,
  pub prog: GProg
}

pub type GStack = Vec<Rc<GNode>>;
pub type GGraph = HashMap<String, GNode>;
pub type GProg = Vec<GCode>;
pub type GDump = Vec<DNode>;

pub struct GState {
  pub stack: GStack,
  pub graph: GGraph,
  pub prog: GProg,
  pub dump: GDump
}

impl fmt::Debug for GNode {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      GNode::GInt(i) => {
        write!(f, "Int({})", i)
      }
      GNode::GCons(l, r) => {
        write!(f, "Cons({:?}, {:?})", l, r)
      }
      GNode::GAp(func, arg) => {
        write!(f, "Ap({:?}, {:?})", func, arg)
      }
      GNode::GFun(arity, body) => {
        write!(f, "GFun({:?}, {:?})", arity, body)
      }
      GNode::GProc(proc) => {
        write!(f, "GProc(?)")
      }
      GNode::GHole() => {
        write!(f, "GHole()")
      }
      GNode::GSRef(r) => {
        write!(f, "GSRef({:?})", r)
      }
    }
  }
}
