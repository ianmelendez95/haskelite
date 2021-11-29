use std::collections::HashMap;

mod builtins;

pub type UBInt = u64;

pub enum GCode {

}

pub enum GNode {
  GInt(UBInt),
  GCons(Box<GNode>, Box<GNode>),
  GAp(Box<GNode>, Box<GNode>),
  GFun(u64, Box<Prog>),
  GHole()
}

pub struct DNode {
  pub stack: Stack,
  pub prog: Prog
}

pub type Stack = Vec<GNode>;
pub type Graph = HashMap<String, GNode>;
pub type Prog = Vec<GCode>;
pub type Dump = Vec<DNode>;

pub struct GState {
  pub stack: Stack,
  pub graph: Graph,
  pub prog: Prog,
  pub dump: Dump
}

fn main() {
  let stack: Stack = panic!();
  let graph: Graph = panic!();
  let dump:  Dump = panic!();


}
