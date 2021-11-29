use std::rc::Rc;
use std::mem;
use super::types::*;

pub fn pop_n(state: &mut GState, n: usize) {
  state.stack.truncate(state.stack.len() - n);
}

pub fn push_int(state: &mut GState, int_val: UBInt) {
  state.stack.push(Rc::from(GNode::GInt(int_val)));
}

pub fn push_stack(state: &mut GState, i: usize) {
  let node = state.stack.get(get_stack_index(state, i)).unwrap();
  state.stack.push(node.clone());
}

pub fn push_proc(state: &mut GState, proc: fn(&mut GState)) {
  state.stack.push(Rc::from(GNode::GProc(proc)));
}

pub fn mk_ap(state: &mut GState) {
  let arg: Rc<GNode> = state.stack.pop().unwrap();
  let fun: Rc<GNode> = state.stack.pop().unwrap();

  state.stack.push(Rc::from(GNode::GAp(fun, arg)))
}

pub fn update(state: &mut GState, i: usize) {
  let s_idx = get_stack_index(state, i);
  let node = state.stack.pop().unwrap();

  state.stack[s_idx] = node;
}

pub fn unwind(state: &mut GState) {
  panic!("NOT IMPLEMENTED");
}

pub fn get_stack_index(state: &GState, i: usize) -> usize {
  return state.stack.len() - 1 - i;
}

pub fn eval(state: &mut GState) {
  panic!("NOT IMPLEMENTED");
}

// TODO - should modify in place (would require a modifiable Rc<RefCell<_>>)
pub fn neg(state: &mut GState) {
  eval(state);

  // neg impl
  let arg: Rc<GNode> = state.stack.pop().unwrap();

  // let arg_val = *arg;
  match &*arg {
    GNode::GInt(i) => {
      state.stack.push(Rc::from(GNode::GInt(-i)));
    }
    v => { panic!("NEG called on non-numeric node: {:?}", v); }
  }

  update(state, 1);
  unwind(state);
}
