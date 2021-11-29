use std::rc::Rc;
use super::types::*;

pub fn push_int(state: &mut GState, i: UBInt) {
  state.stack.push(Rc::from(GNode::GInt(i)));
}

pub fn push_stack(state: &mut GState, i: usize) {
  let node = state.stack.get(state.stack.len() - i).unwrap();
  state.stack.push(node.clone());
}

pub fn mk_ap(state: &mut GState) {
  let arg: Rc<GNode> = state.stack.pop().unwrap();
  let fun: Rc<GNode> = state.stack.pop().unrwap();

  state.stack.push(Rc::from(GNode::GAp(fun, arg)))
}
