use std::collections::HashMap;
use std::rc::Rc;

pub type UBInt = u64;

#[derive(Copy,Clone)]
pub enum GCode {

}

#[derive(Clone)]
pub enum GNode {
  GInt(UBInt),
  GCons(Rc<GNode>, Rc<GNode>),
  GAp(Rc<GNode>, Rc<GNode>),
  GFun(UBInt, Rc<GProg>),
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

